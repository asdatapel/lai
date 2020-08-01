#pragma once

#include <vector>

#include "ast.h"

enum struct Type
{
    UNKNOWN, // ERROR?
    TYPE,
    ARRAY,
    NUMBER,
    FUNCTION,
    FUNCTION_POINTER,
    POINTER,
    STRUCT,
};

struct Type_Base
{
    Type type;
};
struct Type_Unknown : Type_Base
{
    Type_Unknown() { type = Type::UNKNOWN; };
};
struct Type_Type : Type_Base
{
    Type_Type() { type = Type::TYPE; };
};
struct Type_Array : Type_Base
{
    Type_Array() { type = Type::ARRAY; };

    Type_Base *memberType = nullptr;
    unsigned long long length = 0;
};
struct Type_Number : Type_Base
{
    Type_Number() { type = Type::NUMBER; };

    bool signedd = false;
    bool floating = false;
    char size = 0;
};
struct Type_Function : Type_Base
{
    Type_Function() { type = Type::FUNCTION; };

    Type_Base *returnType = nullptr;
    std::vector<Type_Base *> parameters = {};
};
struct Type_FunctionPointer : Type_Base
{
    Type_FunctionPointer() { type = Type::FUNCTION_POINTER; };

    Type_Function *functionType = nullptr;
};
struct Type_Pointer : Type_Base
{
    Type_Pointer() { type = Type::POINTER; };

    Type_Type *pointeeType = nullptr;
};
struct Type_Struct : Type_Base
{
    Type_Struct() { type = Type::STRUCT; };

    std::vector<Type_Base *> memberTypes = {};
};

void resolveTypeForStatement(Ast_Statement *);
void resolveTypeForExpression(Ast_Expression *, Ast *);
Type_Base *resolveExplicitType(Ast_Expression *);

void resolveTypes(Ast *ast)
{
    for (auto statement : ast->statements)
    {
        switch (statement->type)
        {
        case Ast_Statement::Type::DECLARATION_STATEMENT:
        {
            auto st = (Ast_DeclarationStatement *)statement;
            if (!st->explicitType && !st->value)
            {
                // @VALIDATE: at least one should be set
            }

            resolveTypeForExpression(st->value, ast);
            Type_Base *variableType = resolveExplicitType(st->explicitType);

            if (st->explicitType && st->value)
            {
                // @VALIDATE : types should be compatible
            }
            else if (!st->explicitType)
            {
                // assign type of value
                variableType = st->value->expressionType;
            }

            // auto declaration = new Ast::Ast_Declaration;
            // declaration->name = st->identifiers[0]->identifier;
            // declaration->type = variableType;
            // ast->declarations[declaration->name.toString()] = declaration;
        }
        break;
        case Ast_Statement::Type::RETURN_STATEMENT:
        {
        }
        break;
        case Ast_Statement::Type::EXPRESSION_STATEMENT:
        {
        }
        break;
        };
    }
};

void resolveTypeForExpression(Ast_Expression *expression, Ast *scope)
{
    if (!expression){
        return;
    }
    
    switch (expression->type)
    {
    case Ast_Expression::Type::NUMBER_EXPRESSION:
    {
        auto exp = (Ast_NumberExpression *)expression;
        auto t = new Type_Number;

        // all number are doubles
        t->floating = true;
        t->signedd = true;
        t->size = 64;
        // @TODO set these values correctly

        exp->expressionType = t;
    }
    break;
    case Ast_Expression::Type::LITERAL_EXPRESSION:
    {
        // @TODO
    }
    break;
    case Ast_Expression::Type::VARIABLE_EXPRESSION:
    {
        // lookup name
        auto exp = (Ast_VariableExpression *)expression;
        if (scope->declarations.count(exp->identifier.toString()) == 0){
            return;
        }

        // exp->expressionType = scope->declarations[exp->identifier.toString()]->type;
    }
    break;
    case Ast_Expression::Type::UNARY_OPERATION_EXPRESSION:
    {
        // assuming no unary operators will change type ???????
        auto exp = (Ast_UnaryOperatorExpression *)expression;
        resolveTypeForExpression(exp->operand, scope);
        expression->expressionType = exp->operand->expressionType;
    }
    break;
    case Ast_Expression::Type::BINARY_OPERATION_EXPRESSION:
    {
        // temprorary assumption that no binary operator will change type ???????
        auto exp = (Ast_BinaryOperatorExpression *)expression;
        resolveTypeForExpression(exp->leftOperand, scope);
        resolveTypeForExpression(exp->rightOperand, scope);
        expression->expressionType = exp->leftOperand->expressionType;
    }
    break;
    case Ast_Expression::Type::ASSIGNMENT_EXPRESSION:
    {
        auto exp = (Ast_AssignmentExpression *)expression;
        resolveTypeForExpression(exp->lhs, scope);
        resolveTypeForExpression(exp->rhs, scope);
        expression->expressionType = exp->lhs->expressionType;
    }
    break;
    case Ast_Expression::Type::FUNCTION_HEADER_EXPRESSION:
    {
    }
    break;
    case Ast_Expression::Type::FUNCTION_DEFINITION_EXPRESSION:
    {
    }
    break;
    }
}

Type_Base *resolveExplicitType(Ast_Expression *expression)
{
    if (!expression)
    {
        return nullptr;
    }

    if (expression->type == Ast_Expression::Type::VARIABLE_EXPRESSION)
    {
        auto exp = (Ast_VariableExpression *)expression;

        if (exp->identifier.equals("f64"))
        {
            auto t = new Type_Number;

            // all number are doubles
            t->floating = true;
            t->signedd = true;
            t->size = 64;

            return t;
        }
    }
    return nullptr;
}

std::string printType(Type_Base *type){
    if (!type){
        return "nullptr";
    }
}