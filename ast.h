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
    Ast *parent = nullptr;
    std::vector<Ast_Statement *> statements;

    std::map<std::string, llvm::Value*> declarations;
};

struct Ast_Statement
{
    enum struct Type
    {
        EXPRESSION_STATEMENT,
        DECLARATION_STATEMENT,
        RETURN_STATEMENT,
    };

    Type type;
};
struct Ast_ExpressionStatement : Ast_Statement
{
    Ast_ExpressionStatement() { type = Type::EXPRESSION_STATEMENT; };

    Ast_Expression *value = nullptr;
};
struct Ast_DeclarationStatement : Ast_Statement
{
    Ast_DeclarationStatement() { type = Type::DECLARATION_STATEMENT; };

    bool constant;
    std::vector<Ast_VariableExpression *> identifiers;
    Ast_Expression *explicitType = nullptr;
    Ast_Expression *value = nullptr;
};
struct Ast_ReturnStatement : Ast_Statement
{
    Ast_ReturnStatement() { type = Type::RETURN_STATEMENT; };

    Ast_Expression *value = nullptr;
};

struct Ast_Expression
{
    enum struct Type
    {
        NUMBER_EXPRESSION,
        LITERAL_EXPRESSION,
        VARIABLE_EXPRESSION,
        UNARY_OPERATION_EXPRESSION,
        BINARY_OPERATION_EXPRESSION,
        ASSIGNMENT_EXPRESSION,
        FUNCTION_HEADER_EXPRESSION,
        FUNCTION_DEFINITION_EXPRESSION,
        FUNCTION_CALL_EXPRESSION,
    };

    Type type;
    Type_Base *expressionType = nullptr;
};
struct Ast_NumberExpression : Ast_Expression
{
    Ast_NumberExpression() { type = Type::NUMBER_EXPRESSION; };

    double number = 0;
};
struct Ast_LiteralExpression : Ast_Expression
{
    Ast_LiteralExpression() { type = Type::LITERAL_EXPRESSION; };

    Segment value;
};
struct Ast_VariableExpression : Ast_Expression
{
    Ast_VariableExpression() { type = Type::VARIABLE_EXPRESSION; };

    Segment identifier;
};
struct Ast_AssignmentExpression : Ast_Expression
{
    Ast_AssignmentExpression() { type = Type::ASSIGNMENT_EXPRESSION; };

    Ast_Expression *lhs = nullptr;
    Ast_Expression *rhs = nullptr;
};
struct Ast_UnaryOperatorExpression : Ast_Expression
{
    Ast_UnaryOperatorExpression() { type = Type::UNARY_OPERATION_EXPRESSION; };

    char operatorSymbol = 0;
    Ast_Expression *operand = nullptr;
};
struct Ast_BinaryOperatorExpression : Ast_Expression
{
    Ast_BinaryOperatorExpression() { type = Type::BINARY_OPERATION_EXPRESSION; };

    char operatorSymbol = 0;
    Ast_Expression *leftOperand = nullptr;
    Ast_Expression *rightOperand = nullptr;
};
struct Ast_FunctionHeaderExpression : Ast_Expression
{
    Ast_FunctionHeaderExpression() { type = Type::FUNCTION_HEADER_EXPRESSION; };

    std::vector<Ast_DeclarationStatement *> parameters;
    Ast_Expression *returnType = nullptr;
};
struct Ast_FunctionDefinitionExpression : Ast_Expression
{
    Ast_FunctionDefinitionExpression() { type = Type::FUNCTION_DEFINITION_EXPRESSION; };

    Ast_FunctionHeaderExpression *header = nullptr;
    Ast *body = nullptr;
};
struct Ast_FunctionCallExpression : Ast_Expression
{
    Ast_FunctionCallExpression() { type = Type::FUNCTION_CALL_EXPRESSION; };

    Ast_VariableExpression *function = nullptr;
    std::vector<Ast_Expression*> arguments;
};