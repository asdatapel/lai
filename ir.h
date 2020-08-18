#pragma once

#include <vector>

#include "ast.h"
#include "lai_type.h"

struct IrContainer;
struct IrInstr;
struct IrDeclaration;
struct IrGlobalDeclaration;
struct IrIntegerLiteral;
struct IrFloatingPointLiteral;
struct IrAdd;
struct IrSub;
struct IrStore;
struct IrReturn;

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
        FLOATING_POINT_LITERAL,
        FUNCTION,
        FUNCTION_CALL,
        ADD,
        SUB,
        LOAD,
        STORE,
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
};

struct IrGlobalDeclaration : IrDeclaration
{
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

struct IrFloatingPointLiteral : IrInstr
{
    IrFloatingPointLiteral() { type = Type::FLOATING_POINT_LITERAL; }
    IrFloatingPointLiteral(double value)
    {
        type = Type::FLOATING_POINT_LITERAL;
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

struct IrReturn : IrInstr
{
    IrReturn() { type = Type::RETURN; }
    IrInstr *value = nullptr;
};

IrContainer *irify(Ast *);
IrFunction *irifyFunction(Ast_FunctionDefinitionExpression *, IrContainer *parent);
IrInstr *irifyExpression(Ast_Expression *, IrContainer *, bool wantRef = false);
IrInstr *createLoad(IrInstr *);

LaiType *parseType(Ast_Expression *);

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

            auto dec = new IrGlobalDeclaration;
            dec->name = st->identifiers[0]->identifier;
            dec->laiType = parseType(st->explicitType);
            root->declarations.push_back(dec);

            if (auto value = irifyExpression(st->value, root))
            {
                dec->initializer = value;
            }
        }
        break;
        case Ast_Statement::Type::RETURN:
        {
            // @VALIDATE error
        }
        break;
        case Ast_Statement::Type::EXPRESSION:
        {
            // @VALIDATE no expression outside of a function??
        }
        break;
        };
    }
    return root;
}

IrFunction *irifyFunction(Ast_FunctionDefinitionExpression *exp, IrContainer *parent)
{
    auto irFunction = new IrFunction;
    irFunction->laiType = parseType(exp->header);
    irFunction->container = new IrContainer;

    for (auto p : exp->header->parameters)
    {
        auto dec = new IrDeclaration;
        dec->name = p->identifiers[0]->identifier;
        dec->laiType = parseType(p->explicitType);
        irFunction->parameters.push_back(dec);
        irFunction->container->declarations.push_back(dec);
    }

    for (auto statement : exp->body->statements)
    {
        switch (statement->type)
        {
        case Ast_Statement::Type::DECLARATION:
        {
            auto st = (Ast_DeclarationStatement *)statement;

            auto dec = new IrDeclaration;
            dec->name = st->identifiers[0]->identifier;
            dec->laiType = parseType(st->explicitType);
            irFunction->container->declarations.push_back(dec);

            auto value = irifyExpression(st->value, irFunction->container);
            if (value)
            {
                irFunction->container->body.push_back(value);

                auto irStore = new IrStore;
                irStore->target = dec;
                irStore->value = value;
                irFunction->container->body.push_back(irStore);
            }
        }
        break;
        case Ast_Statement::Type::RETURN:
        {
            auto st = (Ast_ReturnStatement *)statement;

            auto value = irifyExpression(st->value, irFunction->container);
            auto irReturn = new IrReturn;
            irReturn->value = value;
            irFunction->container->body.push_back(irReturn);
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
        };
    }

    return irFunction;
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
        return new IrIntegerLiteral(exp->number);
    }
    case Ast_Expression::Type::FLOATING_POINT_LITERAL:
    {
        auto exp = (Ast_FloatingPointLiteralExpression *)expression;
        return new IrFloatingPointLiteral(exp->number);
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
        if (!operand)
        {
            // @VALIDATE error
            return nullptr;
        }

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

        auto lhs = irifyExpression(exp->leftOperand, container);
        auto rhs = irifyExpression(exp->rightOperand, container);
        if (!lhs || !rhs)
        {
            // @VALIDATE error
            return nullptr;
        }

        container->body.push_back(lhs);
        container->body.push_back(rhs);

        switch (exp->operatorSymbol)
        {
        case '+':
        {
            auto irAdd = new IrAdd;
            irAdd->lhs = lhs;
            irAdd->rhs = rhs;
            return irAdd;
        }
        case '-':
        {
            auto irSub = new IrSub;
            irSub->lhs = lhs;
            irSub->rhs = rhs;
            return irSub;
        }
        break;
        }
    }
    break;
    case Ast_Expression::Type::ASSIGNMENT:
    {
        auto exp = (Ast_AssignmentExpression *)expression;

        auto lhs = irifyExpression(exp->lhs, container, true);
        auto rhs = irifyExpression(exp->rhs, container);
        if (!lhs || !rhs)
        {
            // @VALIDATE error
            return nullptr;
        }

        container->body.push_back(lhs);
        container->body.push_back(rhs);

        auto irStore = new IrStore;
        irStore->target = lhs;
        irStore->value = rhs;
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

        auto irFunctionCall = new IrFunctionCall;
        irFunctionCall->function = irifyExpression(exp->function, container);
        irFunctionCall->laiType = irFunctionCall->function->laiType;

        for (auto arg : exp->arguments)
        {
            irFunctionCall->arguments.push_back(irifyExpression(arg, container));
        }

        return irFunctionCall;
    }
    break;
    }

    return nullptr;
}

IrInstr *createLoad(IrInstr *ptr)
{
    auto irLoad = new IrLoad;
    irLoad->value = ptr;
    irLoad->laiType = ptr->laiType;

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