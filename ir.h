#pragma once

#include <vector>

#include "ast.h"
#include "lai_type.h"

struct IrContainer;
struct IrInstr;
struct IrDeclaration;
struct IrIntegerLiteral;
struct IrFloatLiteral;
struct IrAdd;
struct IrSub;
struct IrStore;
struct IrReturn;

void error(std::string message)
{
    std::cout << message << std::endl;
}

struct IrContainer
{
    IrContainer *parent = nullptr;
    std::vector<IrDeclaration *> declarations;
    std::vector<IrInstr *> body;
};

struct IrInstr
{
    enum struct Type
    {
        DECLARATION,
        INTEGER_LITERAL,
        FLOAT_LITERAL,
        FUNCTION,
        FUNCTION_CALL,
        ADD,
        SUB,
        LOAD,
        STORE,
        CAST,
        RETURN,
    };

    Type type;
    LaiType *laiType = nullptr;
    llvm::Value *llvmValue = nullptr; // set during codegen
};

struct IrDeclaration : IrInstr
{
    IrDeclaration() { type = Type::DECLARATION; }
    Segment name;
    IrInstr *initializer = nullptr;
};

struct IrIntegerLiteral : IrInstr
{
    IrIntegerLiteral() { type = Type::INTEGER_LITERAL; }
    IrIntegerLiteral(long long value)
    {
        type = Type::INTEGER_LITERAL;
        this->value = value;
    }
    long long value = 0;
};

struct IrFloatLiteral : IrInstr
{
    IrFloatLiteral() { type = Type::FLOAT_LITERAL; }
    IrFloatLiteral(double value)
    {
        type = Type::FLOAT_LITERAL;
        this->value = value;
    }
    double value = 0.0;
};

struct IrFunction : IrInstr
{
    IrFunction() { type = Type::FUNCTION; }
    IrContainer *container = nullptr;
    std::vector<IrDeclaration *> parameters;
};

struct IrFunctionCall : IrInstr
{
    IrFunctionCall() { type = Type::FUNCTION_CALL; }
    IrInstr *function = nullptr;
    std::vector<IrInstr *> arguments;
};

struct IrAdd : IrInstr
{
    IrAdd() { type = Type::ADD; }
    IrInstr *lhs = nullptr;
    IrInstr *rhs = nullptr;
};

struct IrSub : IrInstr
{
    IrSub() { type = Type::SUB; }
    IrInstr *lhs = nullptr;
    IrInstr *rhs = nullptr;
};

struct IrLoad : IrInstr
{
    IrLoad() { type = Type::LOAD; }
    IrInstr *value = nullptr;
};

struct IrStore : IrInstr
{
    IrStore() { type = Type::STORE; }
    IrInstr *target = nullptr;
    IrInstr *value = nullptr;
};

struct IrCast : IrInstr
{
    IrCast() { type = Type::CAST; }
    IrInstr *value = nullptr;
};

struct IrReturn : IrInstr
{
    IrReturn() { type = Type::RETURN; }
    IrInstr *value = nullptr;
};

IrContainer *irify(Ast *);
IrFunction *irifyFunction(Ast_FunctionDefinitionExpression *, IrContainer *parent);
IrInstr *irifyDeclaration(Ast_DeclarationStatement *, IrContainer *);
IrInstr *irifyExpression(Ast_Expression *, IrContainer *, bool wantRef = false);
IrInstr *irifyBinaryOp(Ast_BinaryOperatorExpression *, IrContainer *);
IrInstr *irifyBinaryMathOp(IrInstr *, IrInstr *, char, IrContainer *);
IrInstr *promote(IrInstr *, LaiType *);
IrInstr *createLoad(IrInstr *);

LaiType *parseType(Ast_Expression *);
LaiType *resolvePointerToType(LaiType *);
LaiType *resolvePromotionType(IrInstr *, IrInstr *);
LaiType *resolveDereferenceType(LaiType *);
LaiType *resolveIntegerType(long long);
LaiType *resolveFloatType(double);

IrContainer *irify(Ast *ast)
{
    auto root = new IrContainer;

    for (auto statement : ast->statements)
    {
        switch (statement->type)
        {
        case Ast_Statement::Type::DECLARATION:
        {
            auto st = (Ast_DeclarationStatement *)statement;
            irifyDeclaration(st, root);
        }
        break;
        case Ast_Statement::Type::BLOCK:
        {
            error("Unexpected statement block outside of a function");
        }
        break;
        case Ast_Statement::Type::EXPRESSION:
        {
            error("Unexpected Expression outside of a function");
        }
        break;
        case Ast_Statement::Type::RETURN:
        {
            error("Unexpected RETURN statement outside of a function");
        }
        break;
        };
    }
    return root;
}

IrFunction *irifyFunction(Ast_FunctionDefinitionExpression *exp, IrContainer *parent)
{
    auto functionType = (LaiType_Function *)parseType(exp->header);

    auto irFunction = new IrFunction;
    irFunction->laiType = functionType;
    irFunction->container = new IrContainer;

    for (auto p : exp->header->parameters)
    {
        auto dec = new IrDeclaration;
        dec->name = p->identifiers[0]->identifier;
        dec->laiType = resolvePointerToType(parseType(p->explicitType));
        irFunction->parameters.push_back(dec);
        irFunction->container->declarations.push_back(dec);
    }

    for (int i = 0; i < exp->body->statements.size(); i++)
    {
        auto statement = exp->body->statements[i];
        switch (statement->type)
        {
        case Ast_Statement::Type::DECLARATION:
        {
            auto st = (Ast_DeclarationStatement *)statement;
            irifyDeclaration(st, irFunction->container);
        }
        break;
        case Ast_Statement::Type::EXPRESSION:
        {
            auto st = (Ast_ExpressionStatement *)statement;

            if (auto value = irifyExpression(st->value, irFunction->container))
            {
                irFunction->container->body.push_back(value);
            }
        }
        break;
        case Ast_Statement::Type::RETURN:
        {
            auto st = (Ast_ReturnStatement *)statement;

            auto value = irifyExpression(st->value, irFunction->container);

            if (functionType->returnType)
            {
                // @TODO set irFunction->laiType to common type (maybe with promotion)
                // error if types conflict
            }
            else
            {
                functionType->returnType = value->laiType;
            }

            auto irReturn = new IrReturn;
            irReturn->value = promote(value, functionType->returnType);
            irFunction->container->body.push_back(irReturn);
        }
        break;
        };
    }

    return irFunction;
}

IrInstr *irifyDeclaration(Ast_DeclarationStatement *st, IrContainer *container)
{
    auto ir = new IrDeclaration;
    ir->name = st->identifiers[0]->identifier;
    container->declarations.push_back(ir);

    auto declType = parseType(st->explicitType);
    ir->laiType = resolvePointerToType(declType); // set type for initializer

    IrInstr *initializer = irifyExpression(st->value, container);

    if (!declType)
    {
        if (!initializer)
        {
            error("something went wrong, how did we get here");
        }

        declType = initializer->laiType;
    }

    ir->laiType = resolvePointerToType(declType);
    if (initializer)
    {
        // @VALIDATE declType and initializer type should match
        ir->initializer = promote(initializer, declType);
    }
    return ir;
}

IrInstr *irifyExpression(Ast_Expression *expression, IrContainer *container, bool wantRef)
{
    if (!expression)
    {
        return nullptr;
    }

    switch (expression->type)
    {
    case Ast_Expression::Type::INTEGER_LITERAL:
    {
        auto exp = (Ast_IntegerLiteralExpression *)expression;

        auto ir = new IrIntegerLiteral(exp->number);
        ir->laiType = resolveIntegerType(exp->number);

        return ir;
    }
    case Ast_Expression::Type::FLOAT_LITERAL:
    {
        auto exp = (Ast_FloatingPointLiteralExpression *)expression;

        auto ir = new IrFloatLiteral(exp->number);
        ir->laiType = resolveFloatType(exp->number);
        return ir;
    }
    break;
    case Ast_Expression::Type::STRING_LITERAL:
    {
    }
    break;
    case Ast_Expression::Type::VARIABLE:
    {
        auto exp = (Ast_VariableExpression *)expression;

        for (auto dec : container->declarations)
        {
            if (exp->identifier.equals(dec->name))
            {
                return wantRef ? dec : createLoad(dec);
            }
        }
    }
    break;
    case Ast_Expression::Type::UNARY_OPERATION:
    {
        auto exp = (Ast_UnaryOperatorExpression *)expression;

        auto operand = irifyExpression(exp->operand, container);
        assert(operand);

        container->body.push_back(operand);

        switch (exp->operatorSymbol)
        {
        case '-':
        {
            auto irSub = new IrSub;
            irSub->lhs = new IrIntegerLiteral(0); // @TODO maybe this should find the type of rhs, and use that type's zero-val
            irSub->rhs = operand;
            return irSub;
        }
        break;
        }
    }
    break;
    case Ast_Expression::Type::BINARY_OPERATION:
    {
        auto exp = (Ast_BinaryOperatorExpression *)expression;
        return irifyBinaryOp(exp, container);
    }
    break;
    case Ast_Expression::Type::ASSIGNMENT:
    {
        auto exp = (Ast_AssignmentExpression *)expression;

        auto lhs = irifyExpression(exp->lhs, container, true);
        auto rhs = irifyExpression(exp->rhs, container);
        assert(lhs);
        assert(rhs);

        container->body.push_back(lhs);
        container->body.push_back(rhs);

        auto irStore = new IrStore;
        irStore->target = lhs;
        irStore->value = rhs;
        irStore->laiType = lhs->laiType;
        return irStore;
    }
    break;
    case Ast_Expression::Type::FUNCTION_HEADER:
    {
        // maybe we dont generate code here, since function headers only appear as a type?
    }
    break;
    case Ast_Expression::Type::FUNCTION_DEFINITION:
    {
        auto exp = (Ast_FunctionDefinitionExpression *)expression;
        return irifyFunction(exp, container);
    }
    break;
    case Ast_Expression::Type::FUNCTION_CALL:
    {
        auto exp = (Ast_FunctionCallExpression *)expression;

        auto function = irifyExpression(exp->function, container);
        if (function->laiType->laiTypeType != LaiTypeType::FUNCTION)
        {
            error("attempting a function call with a nonfunction type");
        }
        auto functionType = ((LaiType_Function *)function->laiType);

        if (functionType->parameters.size() != exp->arguments.size())
        {
            error("wrong number of args");
        }

        auto irFunctionCall = new IrFunctionCall;
        irFunctionCall->function = function;
        irFunctionCall->laiType = functionType->returnType;
        for (int i = 0; i < exp->arguments.size(); i++)
        {
            auto promoted = promote(irifyExpression(exp->arguments[i], container), functionType->parameters[i]);
            irFunctionCall->arguments.push_back(promoted);
        }

        return irFunctionCall;
    }
    break;
    }

    return nullptr;
}

IrInstr *irifyBinaryOp(Ast_BinaryOperatorExpression *exp, IrContainer *container)
{
    auto lhs = irifyExpression(exp->leftOperand, container);
    auto rhs = irifyExpression(exp->rightOperand, container);
    assert(lhs);
    assert(rhs);

    container->body.push_back(lhs);
    container->body.push_back(rhs);

    if ((lhs->laiType->laiTypeType == LaiTypeType::INTEGER || lhs->laiType->laiTypeType == LaiTypeType::FLOAT) &&
        (rhs->laiType->laiTypeType == LaiTypeType::INTEGER || rhs->laiType->laiTypeType == LaiTypeType::FLOAT))
    {
        return irifyBinaryMathOp(lhs, rhs, exp->operatorSymbol, container);
    }

    return nullptr;
}

IrInstr *irifyBinaryMathOp(IrInstr *lhs, IrInstr *rhs, char op, IrContainer *container)
{
    auto promotionType = resolvePromotionType(lhs, rhs);
    lhs = promote(lhs, promotionType);
    rhs = promote(rhs, promotionType);

    switch (op)
    {
    case '+':
    {
        auto irAdd = new IrAdd;
        irAdd->lhs = lhs;
        irAdd->rhs = rhs;
        irAdd->laiType = lhs->laiType;
        return irAdd;
    }
    case '-':
    {
        auto irSub = new IrSub;
        irSub->lhs = lhs;
        irSub->rhs = rhs;
        irSub->laiType = lhs->laiType;
        return irSub;
    }
    break;
    }
}

IrInstr *promote(IrInstr *val, LaiType *type)
{
    if ((val->laiType->laiTypeType != LaiTypeType::INTEGER && val->laiType->laiTypeType != LaiTypeType::FLOAT) || val->laiType == type)
    {
        return val;
    }

    auto irCast = new IrCast;
    irCast->value = val;
    irCast->laiType = type;
}

IrInstr *createLoad(IrInstr *ptr)
{
    auto irLoad = new IrLoad;
    irLoad->value = ptr;
    irLoad->laiType = resolveDereferenceType(ptr->laiType);

    return irLoad;
}

LaiType *parseVariableType(Ast_VariableExpression *expression)
{
    return parseBuiltinType(expression->identifier);
}
LaiType *parseFunctionType(Ast_FunctionHeaderExpression *expression)
{
    auto type = new LaiType_Function;
    type->returnType = parseType(expression->returnType);
    for (auto p : expression->parameters)
    {
        type->parameters.push_back(parseType(p->explicitType));
    }
    return type;
}
LaiType *parseType(Ast_Expression *expression)
{
    if (!expression)
    {
        return nullptr;
    }

    switch (expression->type)
    {
    case Ast_Expression::Type::VARIABLE:
    {
        auto exp = (Ast_VariableExpression *)expression;
        return parseVariableType(exp);
    }
    break;
    case Ast_Expression::Type::FUNCTION_HEADER:
    {
        auto exp = (Ast_FunctionHeaderExpression *)expression;
        return parseFunctionType(exp);
    }
    break;
    }

    return nullptr;
}

LaiType *resolvePointerToType(LaiType *type)
{
    auto ptr = new LaiType_Pointer;
    ptr->pointeeType = type;
    return ptr;
}

LaiType *resolvePromotionType(IrInstr *lhs, IrInstr *rhs)
{
    if (lhs->laiType->laiTypeType == LaiTypeType::FLOAT)
    {
        return lhs->laiType;
    }
    if (rhs->laiType->laiTypeType == LaiTypeType::FLOAT)
    {
        return rhs->laiType;
    }
    if (!((LaiType_Integer *)lhs->laiType)->isSigned)
    {
        return lhs->laiType;
    }
    if (!((LaiType_Integer *)rhs->laiType)->isSigned)
    {
        return rhs->laiType;
    }
    if (((LaiType_Integer *)lhs->laiType)->size >= ((LaiType_Integer *)rhs->laiType)->size)
    {
        return lhs->laiType;
    }
    return rhs->laiType;
}

LaiType *resolveDereferenceType(LaiType *ptr)
{
    if (ptr->laiTypeType != LaiTypeType::POINTER)
    {
        error("attempting to dereference a nonpointer");
    }

    return ((LaiType_Pointer *)ptr)->pointeeType;
}

LaiType *resolveIntegerType(long long i)
{
    return i < 0 ? &builtinTypeS32 : &builtinTypeI32;
}

LaiType *resolveFloatType(double i)
{
    return &builtinTypeF64;
}