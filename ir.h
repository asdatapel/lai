#pragma once

#include <vector>

#include "ast.h"
#include "lai_type.h"

struct Scope;
struct IrContainer;

struct IrInstr;
struct IrDeclaration;
struct IrIntegerLiteral;
struct IrFloatLiteral;
struct IrFunction;
struct IrAdd;
struct IrSub;
struct IrSLoad;
struct IrStore;
struct IrCast;
struct IrReturn;

void error(std::string message)
{
    std::cout << message << std::endl;
}

struct Scope
{
    Scope *parent = nullptr;
    std::vector<IrDeclaration *> declarations;
};

struct IrContainer : Scope
{
    std::vector<IrInstr *> instrs;

    // remove
    std::vector<IrReturn *> returns;
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
        CMP_EQ,
        CMP_NEQ,
        CMP_LT,
        CMP_GT,
        CMP_LTE,
        CMP_GTE,
        LOAD,
        STORE,
        CAST,
        RETURN,
        LABEL,
        JUMP_IF,
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

struct IrFunction : IrInstr, IrContainer
{
    IrFunction() { type = Type::FUNCTION; }

    // remove
    std::vector<IrDeclaration *> parameters;
};

struct IrFunctionCall : IrInstr
{
    IrFunctionCall() { type = Type::FUNCTION_CALL; }
    IrInstr *function = nullptr;
    std::vector<IrInstr *> arguments;
};

struct IrMathBinaryOp : IrInstr
{
    IrMathBinaryOp(Type type)
    {
        this->type = type;
    }
    IrInstr *lhs = nullptr;
    IrInstr *rhs = nullptr;
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

struct IrCmpEqual : IrInstr
{
    IrCmpEqual()
    {
        type = Type::CMP_EQ;
        laiType = &builtinTypeBool;
    }
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

struct IrLabel : IrInstr
{
    IrLabel() { type = Type::LABEL; }
};

struct IrJumpIf : IrInstr
{
    IrJumpIf() { type = Type::JUMP_IF; }
    IrInstr *condition = nullptr;
    IrInstr *inside = nullptr;
    IrInstr *outside = nullptr;
};

IrContainer *irify(Ast_BlockStatement *);
void irifyStatement(Ast_Statement *, IrContainer *, Scope *);
Scope *irifyBlock(Ast_BlockStatement *, IrContainer *, Scope *, Scope *existingScope = nullptr);
IrFunction *irifyFunction(Ast_FunctionDefinitionExpression *, Scope *);
IrDeclaration *irifyDeclaration(Ast_DeclarationStatement *, IrContainer *, Scope *);
IrInstr *irifyExpression(Ast_Expression *, IrContainer *, Scope *, bool wantRef = false);
IrInstr *irifyBinaryOp(Ast_BinaryOperatorExpression *, IrContainer *, Scope *);
IrInstr *irifyBinaryMathOp(IrInstr *, IrInstr *, TokenType, IrContainer *, Scope *);
IrInstr *promote(IrInstr *, LaiType *, IrContainer *, Scope *);
IrInstr *createLoad(IrInstr *);
IrInstr *findVar(Segment name, Scope *scope);

LaiType *parseType(Ast_Expression *);
LaiType *resolvePointerToType(LaiType *);
LaiType *resolvePromotionType(IrInstr *, IrInstr *);
LaiType *resolveDereferenceType(LaiType *);
LaiType *resolveIntegerType(long long);
LaiType *resolveFloatType(double);

IrContainer *irify(Ast_BlockStatement *rootAst)
{
    auto root = new IrContainer;

    for (auto statement : rootAst->body)
    {
        switch (statement->type)
        {
        case Ast_Statement::Type::DECLARATION:
        {
            auto st = (Ast_DeclarationStatement *)statement;

            auto dec = irifyDeclaration(st, root, root);
            if (dec->initializer)
            {
                root->instrs.push_back(dec);
            }
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

IrDeclaration *irifyDeclaration(Ast_DeclarationStatement *st, IrContainer *ir, Scope *scope)
{
    auto dec = new IrDeclaration;
    dec->name = st->identifiers[0]->identifier;

    ir->declarations.push_back(dec);
    if (ir != scope)
    {
        scope->declarations.push_back(dec);
    }

    auto declType = parseType(st->explicitType);
    dec->laiType = resolvePointerToType(declType); // set type for initializer

    IrInstr *initializer = irifyExpression(st->value, ir, scope);

    if (!declType)
    {
        if (!initializer)
        {
            error("something went wrong, how did we get here");
        }

        declType = initializer->laiType;
    }

    dec->laiType = resolvePointerToType(declType);
    if (initializer)
    {
        // @VALIDATE declType and initializer type should match
        dec->initializer = promote(initializer, declType, ir, scope);
        ir->instrs.push_back(dec->initializer);
    }
    return dec;
}

void irifyStatement(Ast_Statement *statement, IrContainer *ir, Scope *scope)
{
    switch (statement->type)
    {
    case Ast_Statement::Type::DECLARATION:
    {
        auto st = (Ast_DeclarationStatement *)statement;

        auto dec = irifyDeclaration(st, ir, scope);
        if (dec->initializer)
        {
            auto irStore = new IrStore;
            irStore->target = dec;
            irStore->value = dec->initializer;
            ir->instrs.push_back(irStore);
        }
    }
    break;
    case Ast_Statement::Type::BLOCK:
    {
        auto st = (Ast_BlockStatement *)statement;
        irifyBlock(st, ir, scope, nullptr);
    }
    break;
    case Ast_Statement::Type::EXPRESSION:
    {
        auto st = (Ast_ExpressionStatement *)statement;

        if (auto value = irifyExpression(st->value, ir, scope))
        {
            // @TODO pushing should be done within irifyExpression???
            ir->instrs.push_back(value);
        }
    }
    break;
    case Ast_Statement::Type::RETURN:
    {
        auto st = (Ast_ReturnStatement *)statement;

        auto value = irifyExpression(st->value, ir, scope);
        ir->instrs.push_back(value);

        auto irReturn = new IrReturn;
        irReturn->value = value;
        ir->returns.push_back(irReturn);
        ir->instrs.push_back(irReturn);
    }
    break;
    };
}

Scope *irifyBlock(Ast_BlockStatement *exp, IrContainer *ir, Scope *parent, Scope *existingScope)
{
    Scope *scope;
    if (existingScope)
    {
        scope = existingScope;
    }
    else
    {
        scope = new Scope;
        scope->parent = parent;
    }

    for (int i = 0; i < exp->body.size(); i++)
    {
        irifyStatement(exp->body[i], ir, scope);
    }

    return scope;
}

IrFunction *irifyFunction(Ast_FunctionDefinitionExpression *exp, Scope *parent)
{
    auto functionType = (LaiType_Function *)parseType(exp->header);

    auto irFunction = new IrFunction;
    irFunction->laiType = functionType;
    irFunction->parent = parent;

    for (auto p : exp->header->parameters)
    {
        auto dec = new IrDeclaration;
        dec->name = p->identifiers[0]->identifier;
        dec->laiType = resolvePointerToType(parseType(p->explicitType));
        irFunction->parameters.push_back(dec);
        irFunction->declarations.push_back(dec);
    }

    auto blockIr = irifyBlock(exp->body, irFunction, parent, irFunction);

    for (auto ret : irFunction->returns)
    {
        if (functionType->returnType)
        {
            // @TODO set irFunction->laiType to common type (maybe with promotion)
            // error if types conflict
        }
        else
        {
            functionType->returnType = ret->value->laiType;
        }
        ret->value = promote(ret->value, functionType->returnType, nullptr, nullptr);
    }

    return irFunction;
}

IrInstr *irifyExpression(Ast_Expression *expression, IrContainer *ir, Scope *scope, bool wantRef)
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
    case Ast_Expression::Type::BOOLEAN_LITERAL:
    {
        auto exp = (Ast_BooleanExpression *)expression;

        auto ir = new IrIntegerLiteral(exp->value ? 1 : 0);
        ir->laiType = &builtinTypeBool;
        return ir;
    }
    break;
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

        auto var = findVar(exp->identifier, scope);

        if (!var)
        {
            // @VALIDATE error
            std::cout << "asdasd"
                      << "\n";
        }

        return wantRef ? var : createLoad(var);
    }
    break;
    case Ast_Expression::Type::UNARY_OPERATION:
    {
        auto exp = (Ast_UnaryOperatorExpression *)expression;

        auto operand = irifyExpression(exp->operand, ir, scope);
        assert(operand);

        ir->instrs.push_back(operand);

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
        return irifyBinaryOp(exp, ir, scope);
    }
    break;
    case Ast_Expression::Type::ASSIGNMENT:
    {
        auto exp = (Ast_AssignmentExpression *)expression;

        auto lhs = irifyExpression(exp->lhs, ir, scope, true);
        auto rhs = irifyExpression(exp->rhs, ir, scope);
        assert(lhs);
        assert(rhs);

        ir->instrs.push_back(lhs);
        ir->instrs.push_back(rhs);

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
        return irifyFunction(exp, scope);
    }
    break;
    case Ast_Expression::Type::FUNCTION_CALL:
    {
        auto exp = (Ast_FunctionCallExpression *)expression;

        auto function = irifyExpression(exp->function, ir, scope);
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
            auto promoted = promote(irifyExpression(exp->arguments[i], ir, scope), functionType->parameters[i], ir, scope);
            irFunctionCall->arguments.push_back(promoted);
        }

        return irFunctionCall;
    }
    break;
    case Ast_Expression::Type::IF:
    {
        auto exp = (Ast_IfExpression *)expression;

        auto condition = irifyExpression(exp->condition, ir, scope);
        ir->instrs.push_back(condition);

        auto irJump = new IrJumpIf;
        irJump->condition = condition;

        irJump->inside = new IrLabel;
        irJump->outside = new IrLabel;
        ir->instrs.push_back(irJump);

        ir->instrs.push_back(irJump->inside);
        irifyStatement(exp->body, ir, scope);
        ir->instrs.push_back(irJump->outside);
    }
    break;
    }

    return nullptr;
}

IrInstr *irifyBinaryOp(Ast_BinaryOperatorExpression *exp, IrContainer *ir, Scope *scope)
{
    auto lhs = irifyExpression(exp->leftOperand, ir, scope);
    auto rhs = irifyExpression(exp->rightOperand, ir, scope);
    assert(lhs);
    assert(rhs);

    ir->instrs.push_back(lhs);
    ir->instrs.push_back(rhs);

    if ((lhs->laiType->laiTypeType == LaiTypeType::INTEGER || lhs->laiType->laiTypeType == LaiTypeType::FLOAT) &&
        (rhs->laiType->laiTypeType == LaiTypeType::INTEGER || rhs->laiType->laiTypeType == LaiTypeType::FLOAT))
    {
        return irifyBinaryMathOp(lhs, rhs, exp->operatorSymbol, ir, scope);
    }

    return nullptr;
}

IrInstr *irifyBinaryMathOp(IrInstr *lhs, IrInstr *rhs, TokenType op, IrContainer *ir, Scope *scope)
{
    auto promotionType = resolvePromotionType(lhs, rhs);
    lhs = promote(lhs, promotionType, ir, scope);
    rhs = promote(rhs, promotionType, ir, scope);

    switch (op)
    {
    case static_cast<TokenType>('+'):
    {
        auto irAdd = new IrAdd;
        irAdd->lhs = lhs;
        irAdd->rhs = rhs;
        irAdd->laiType = lhs->laiType;
        return irAdd;
    }
    break;
    case static_cast<TokenType>('-'):
    {
        auto irSub = new IrSub;
        irSub->lhs = lhs;
        irSub->rhs = rhs;
        irSub->laiType = lhs->laiType;
        return irSub;
    }
    break;
    case static_cast<TokenType>('>'):
    {
        auto instr = new IrMathBinaryOp(IrInstr::Type::CMP_GT);
        instr->laiType = &builtinTypeBool;
        instr->lhs = lhs;
        instr->rhs = rhs;
        return instr;
    }
    break;
    case static_cast<TokenType>('<'):
    {
        auto instr = new IrMathBinaryOp(IrInstr::Type::CMP_LT);
        instr->laiType = &builtinTypeBool;
        instr->lhs = lhs;
        instr->rhs = rhs;
        return instr;
    }
    break;
    case TokenType::T_DOUBLE_EQUAL:
    {
        auto irCmpEqual = new IrCmpEqual;
        irCmpEqual->lhs = lhs;
        irCmpEqual->rhs = rhs;
        return irCmpEqual;
    }
    break;
    case TokenType::T_NOT_EQUAL:
    {
        auto instr = new IrMathBinaryOp(IrInstr::Type::CMP_NEQ);
        instr->laiType = &builtinTypeBool;
        instr->lhs = lhs;
        instr->rhs = rhs;
        return instr;
    }
    break;
    case TokenType::T_GREATER_THAN_EQUAL:
    {
        auto instr = new IrMathBinaryOp(IrInstr::Type::CMP_GTE);
        instr->laiType = &builtinTypeBool;
        instr->lhs = lhs;
        instr->rhs = rhs;
        return instr;
    }
    break;
    case TokenType::T_LESS_THAN_EQUAL:
    {
        auto instr = new IrMathBinaryOp(IrInstr::Type::CMP_LTE);
        instr->laiType = &builtinTypeBool;
        instr->lhs = lhs;
        instr->rhs = rhs;
        return instr;
    }
    break;
    }
}

// @TODO this should push the cast instr to the container???
IrInstr *promote(IrInstr *val, LaiType *type, IrContainer *ir, Scope *scope)
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

IrInstr *findVar(Segment name, Scope *scope)
{
    if (!scope)
    {
        return nullptr;
    }

    for (auto dec : scope->declarations)
    {
        if (name.equals(dec->name))
        {
            return dec;
        }
    }

    return findVar(name, scope->parent);
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