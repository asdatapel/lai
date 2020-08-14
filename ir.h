#pragma once

#include <vector>

#include "ast.h"
#include "lai_type.h"

struct IrContainer;
struct IrInstr;
struct IrDeclaration;
struct IrGlobalDeclaration;
struct IrConstant;
struct IrAdd;
struct IrSub;
struct IrStore;
struct IrReturn;

struct IrContainer
{
    std::vector<IrDeclaration *> declarations;
    std::vector<IrInstr *> body;
};

struct IrInstr
{
    enum struct Type
    {
        DECLARATION,
        CONSTANT,
        FUNCTION,
        ADD,
        SUB,
        STORE,
        RETURN,
    };

    Type type;
    llvm::Value *llvmValue = nullptr; // set during codegen
};

struct IrDeclaration : IrInstr
{
    IrDeclaration() { type = Type::DECLARATION; }
    LaiType *laiType = nullptr;
};

struct IrGlobalDeclaration : IrDeclaration
{
    IrInstr *initializer = nullptr;
};

struct IrConstant : IrInstr
{
    IrConstant() { type = Type::CONSTANT; }
    IrConstant(long long value)
    {
        type = Type::CONSTANT;
        this->value = value;
    }
    long long value = 0;
};

struct IrFunction : IrInstr
{
    IrFunction() { type = Type::FUNCTION; }
    LaiType *functionType = nullptr;
    IrContainer *container = nullptr;
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
IrContainer *irifyFunction(Ast *);
IrInstr *irifyExpression(Ast_Expression *, IrContainer *);
LaiType *resolveType(Ast_Expression *);

IrContainer *irify(Ast *ast)
{
    auto root = new IrContainer;

    for (auto statement : ast->statements)
    {
        switch (statement->type)
        {
        case Ast_Statement::Type::DECLARATION_STATEMENT:
        {
            auto st = (Ast_DeclarationStatement *)statement;

            auto dec = new IrGlobalDeclaration;
            dec->laiType = resolveType(st->explicitType);
            root->declarations.push_back(dec);

            if (auto value = irifyExpression(st->value, root))
            {
                dec->initializer = value;
            }
        }
        break;
        case Ast_Statement::Type::RETURN_STATEMENT:
        {
            // @VALIDATE error
        }
        break;
        case Ast_Statement::Type::EXPRESSION_STATEMENT:
        {
            auto st = (Ast_ExpressionStatement *)statement;

            if (auto value = irifyExpression(st->value, root))
            {
                root->body.push_back(value);
            }
        }
        break;
        };
    }
    return root;
}

IrContainer *irifyFunction(Ast *ast)
{
    auto *container = new IrContainer;
    for (auto statement : ast->statements)
    {
        switch (statement->type)
        {
        case Ast_Statement::Type::DECLARATION_STATEMENT:
        {
            auto st = (Ast_DeclarationStatement *)statement;

            auto dec = new IrDeclaration;
            dec->laiType = resolveType(st->explicitType);
            container->declarations.push_back(dec);

            auto value = irifyExpression(st->value, container);
            if (value)
            {
                container->body.push_back(value);

                auto irStore = new IrStore;
                irStore->target = dec;
                irStore->value = value;
                container->body.push_back(irStore);
            }
        }
        break;
        case Ast_Statement::Type::RETURN_STATEMENT:
        {
            auto st = (Ast_ReturnStatement *)statement;

            auto value = irifyExpression(st->value, container);
            auto irReturn = new IrReturn;
            irReturn->value = value;
            container->body.push_back(irReturn);
        }
        break;
        case Ast_Statement::Type::EXPRESSION_STATEMENT:
        {
            auto st = (Ast_ExpressionStatement *)statement;

            if (auto value = irifyExpression(st->value, container))
            {
                container->body.push_back(value);
            }
        }
        break;
        };
    }

    return container;
}

IrInstr *irifyExpression(Ast_Expression *expression, IrContainer *container)
{
    if (!expression)
    {
        return nullptr;
    }

    switch (expression->type)
    {
    case Ast_Expression::Type::NUMBER_EXPRESSION:
    {
        auto exp = (Ast_NumberExpression *)expression;

        auto irConstant = new IrConstant;
        irConstant->value = exp->number;
        return irConstant;
    }
    break;
    case Ast_Expression::Type::LITERAL_EXPRESSION:
    {
    }
    break;
    case Ast_Expression::Type::VARIABLE_EXPRESSION:
    {
    }
    break;
    case Ast_Expression::Type::UNARY_OPERATION_EXPRESSION:
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
            irSub->lhs = new IrConstant(0);
            irSub->rhs = operand;
            return irSub;
        }
        break;
        }
    }
    break;
    case Ast_Expression::Type::BINARY_OPERATION_EXPRESSION:
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
    case Ast_Expression::Type::ASSIGNMENT_EXPRESSION:
    {
        auto exp = (Ast_AssignmentExpression *)expression;

        auto lhs = irifyExpression(exp->lhs, container);
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
    case Ast_Expression::Type::FUNCTION_HEADER_EXPRESSION:
    {
        // maybe we dont generate code here, since function headers only appear as a type?
    }
    break;
    case Ast_Expression::Type::FUNCTION_DEFINITION_EXPRESSION:
    {
        auto exp = (Ast_FunctionDefinitionExpression *)expression;
        
        auto irFunction = new IrFunction;
        irFunction->functionType = resolveType(exp->header);
        irFunction->container = irifyFunction(exp->body);

        return irFunction;
    }
    break;
    case Ast_Expression::Type::FUNCTION_CALL_EXPRESSION:
    {
    }
    break;
    }
}

LaiType *resolveType(Ast_Expression *expression)
{
    switch (expression->type)
    {
    case Ast_Expression::Type::VARIABLE_EXPRESSION:
    {
        auto exp = (Ast_VariableExpression *)expression;
        return resolveBuiltinType(exp->identifier);
    }
    break;
    case Ast_Expression::Type::FUNCTION_HEADER_EXPRESSION:
    {
        auto exp = (Ast_FunctionHeaderExpression *)expression;

        auto type = new LaiType_Function;
        type->returnType = resolveType(exp->returnType);
        for (auto p : exp->parameters)
        {
            type->parameters.push_back(resolveType(p->explicitType));
        }
        return type;
    }
    break;
    }
}