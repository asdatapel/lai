#pragma once

#include <map>
#include <vector>

#include <llvm/IR/IRBuilder.h>

#include "token_list.h"

// forward decs
struct Ast;
struct Ast_Statement;
struct Ast_Expression;
struct Ast_VariableExpression;

struct Type_Base;

struct Ast
{
    std::vector<Ast_Statement *> statements;
};

struct Ast_Statement
{
    enum struct Type
    {
        EXPRESSION,
        DECLARATION,
        RETURN,
    };

    Type type;
};
struct Ast_ExpressionStatement : Ast_Statement
{
    Ast_ExpressionStatement() { type = Type::EXPRESSION; };

    Ast_Expression *value = nullptr;
};
struct Ast_DeclarationStatement : Ast_Statement
{
    Ast_DeclarationStatement() { type = Type::DECLARATION; };

    bool constant;
    std::vector<Ast_VariableExpression *> identifiers;
    Ast_Expression *explicitType = nullptr;
    Ast_Expression *value = nullptr;
};
struct Ast_ReturnStatement : Ast_Statement
{
    Ast_ReturnStatement() { type = Type::RETURN; };

    Ast_Expression *value = nullptr;
};

struct Ast_Expression
{
    enum struct Type
    {
        INTEGER_LITERAL,
        FLOAT_LITERAL,
        STRING_LITERAL,
        VARIABLE,
        UNARY_OPERATION,
        BINARY_OPERATION,
        ASSIGNMENT,
        FUNCTION_HEADER,
        FUNCTION_DEFINITION,
        FUNCTION_CALL,
    };

    Type type;
    Type_Base *expressionType = nullptr;
};
struct Ast_IntegerLiteralExpression : Ast_Expression
{
    Ast_IntegerLiteralExpression() { type = Type::INTEGER_LITERAL; };

    long long number = 0;
};
struct Ast_FloatingPointLiteralExpression : Ast_Expression
{
    Ast_FloatingPointLiteralExpression() { type = Type::FLOAT_LITERAL; };

    double number = 0;
};
struct Ast_StringLiteralExpression : Ast_Expression
{
    Ast_StringLiteralExpression() { type = Type::STRING_LITERAL; };

    Segment value;
};
struct Ast_VariableExpression : Ast_Expression
{
    Ast_VariableExpression() { type = Type::VARIABLE; };

    Segment identifier;
};
struct Ast_AssignmentExpression : Ast_Expression
{
    Ast_AssignmentExpression() { type = Type::ASSIGNMENT; };

    Ast_Expression *lhs = nullptr;
    Ast_Expression *rhs = nullptr;
};
struct Ast_UnaryOperatorExpression : Ast_Expression
{
    Ast_UnaryOperatorExpression() { type = Type::UNARY_OPERATION; };

    char operatorSymbol = 0;
    Ast_Expression *operand = nullptr;
};
struct Ast_BinaryOperatorExpression : Ast_Expression
{
    Ast_BinaryOperatorExpression() { type = Type::BINARY_OPERATION; };

    char operatorSymbol = 0;
    Ast_Expression *leftOperand = nullptr;
    Ast_Expression *rightOperand = nullptr;
};
struct Ast_FunctionHeaderExpression : Ast_Expression
{
    Ast_FunctionHeaderExpression() { type = Type::FUNCTION_HEADER; };

    std::vector<Ast_DeclarationStatement *> parameters;
    Ast_Expression *returnType = nullptr;
};
struct Ast_FunctionDefinitionExpression : Ast_Expression
{
    Ast_FunctionDefinitionExpression() { type = Type::FUNCTION_DEFINITION; };

    Ast_FunctionHeaderExpression *header = nullptr;
    Ast *body = nullptr;
};
struct Ast_FunctionCallExpression : Ast_Expression
{
    Ast_FunctionCallExpression() { type = Type::FUNCTION_CALL; };

    Ast_Expression *function = nullptr;
    std::vector<Ast_Expression *> arguments;
};