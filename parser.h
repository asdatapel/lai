#pragma once

#include <iostream>
#include <set>
#include <vector>

#include "llvm.h"

#include "ast.h"
#include "token_list.h"
#include "types.h"

Ast_Statement *parseStatement(TokenList *, Ast *ast);
Ast_DeclarationStatement *parseDeclaration(TokenList *, Ast *ast);

Ast_Expression *parseTerm(TokenList *, Ast *ast);
Ast_Expression *parseExpression(TokenList *, Ast *ast);
Ast_Expression *parseFunction(TokenList *, Ast *ast);

bool isDeclaration(TokenList *);
bool isFunction(TokenList *);
bool isUnaryOperator(TokenType c);
bool isBinaryOperator(TokenType c);

Ast *parseAst(TokenList *tokenList)
{
    auto ast = new Ast;

    auto token = tokenList->peek_next();
    while (token->type != TokenType::T_EOF && token->type != static_cast<TokenType>('}'))
    {
        auto st = parseStatement(tokenList, ast);
        ast->statements.push_back(st);

        if (tokenList->peek_next()->type != static_cast<TokenType>(';'))
        {
            // @VALIDATE error
        }

        tokenList->eat(); // eat ';'
        token = tokenList->peek_next();
    }

    return ast;
}

Ast_Statement *parseStatement(TokenList *tokenList, Ast *ast)
{
    Ast_Statement *statement = nullptr;

    auto token = tokenList->peek_next();
    switch (token->type)
    {
    case static_cast<TokenType>('{'):
    {
        tokenList->eat(); // eat '{'
        auto ast = parseAst(tokenList);

        if (tokenList->peek_next()->type != static_cast<TokenType>('}')){
            // @VALIDATE error
        }

        auto st = new Ast_BlockStatement;
        st->body = ast;

        statement = st;
    }
    break;
    case TokenType::T_RETURN:
    {
        tokenList->eat(); // eat 'return'

        auto exp = parseExpression(tokenList, ast);

        auto st = new Ast_ReturnStatement;
        st->value = exp;

        statement = st;
    }
    break;
    default:
    {
        if (isDeclaration(tokenList))
        {
            statement = parseDeclaration(tokenList, ast);
        }
        else
        {
            auto exp = parseExpression(tokenList, ast);
            if (exp)
            {
                auto st = new Ast_ExpressionStatement;
                st->value = exp;

                statement = st;
            }
        }
    }
    break;
    }

    return statement;
};

Ast_DeclarationStatement *parseDeclaration(TokenList *tokenList, Ast *ast)
{
    auto token = tokenList->peek_next();
    auto st = new Ast_DeclarationStatement;

    auto addIdentiferToLhs = [&]() {
        if (token->type != TokenType::T_IDENTIFIER)
        {
            // @VALIDATE must be identifier
            std::cout << "error207" << std::endl;
        };

        auto varExp = new Ast_VariableExpression;
        varExp->identifier = token->identifier;
        st->identifiers.push_back(varExp);

        tokenList->eat();
        token = tokenList->peek_next();
    };
    addIdentiferToLhs();
    while (token->type == static_cast<TokenType>(','))
    {
        tokenList->eat();
        token = tokenList->peek_next();

        addIdentiferToLhs();
    }

    if (token->type != static_cast<TokenType>(':'))
    {
        // @VALIDATE error
    }
    tokenList->eat(); // eat ':'
    st->explicitType = parseTerm(tokenList, ast);

    token = tokenList->peek_next();

    if (token->type == static_cast<TokenType>(':') || token->type == static_cast<TokenType>('='))
    {
        st->constant = (token->type == static_cast<TokenType>(':'));
        tokenList->eat(); // eat ':' or '='

        st->value = parseExpression(tokenList, ast);
    }

    return st;
};

Ast_Expression *parseTerm(TokenList *tokenList, Ast *ast)
{
    auto token = tokenList->peek_next();
    switch (token->type)
    {
    case static_cast<TokenType>('('):
    {
        if (isFunction(tokenList))
        {
            return parseFunction(tokenList, ast);
        }

        tokenList->eat(); // '('
        auto exp = parseExpression(tokenList, ast);
        if (tokenList->peek_next()->type != static_cast<TokenType>(')'))
        {
            // @VALIDATE error
        }
        tokenList->eat(); // ')'
        return exp;
    }
    break;
    case TokenType::T_INTEGER_LITERAL:
    {
        tokenList->eat(); // eat number

        auto exp = new Ast_IntegerLiteralExpression;
        exp->number = token->intVal;
        return exp;
    }
    break;
    case TokenType::T_FLOAT_LITERAL:
    {
        tokenList->eat(); // eat number

        auto exp = new Ast_FloatingPointLiteralExpression;
        exp->number = token->doubleVal;
        return exp;
    }
    break;
    case TokenType::T_STRING_LITERAL:
    {
        tokenList->eat(); // eat string

        auto exp = new Ast_StringLiteralExpression;
        exp->value = token->stringVal;
        return exp;
    }
    break;
    case TokenType::T_IDENTIFIER:
    {
        tokenList->eat(); // eat identifier

        auto exp = new Ast_VariableExpression;
        exp->identifier = token->identifier;
        return exp;
    }
    break;
    }

    if (token->type == static_cast<TokenType>('-'))
    {
        auto nextToken = tokenList->peek(1);
        if (nextToken->type == TokenType::T_INTEGER_LITERAL)
        {
            tokenList->eat(2); // eat '-' and number

            auto exp = new Ast_IntegerLiteralExpression;
            exp->number = -nextToken->intVal;
            return exp;
        }
        if (nextToken->type == TokenType::T_FLOAT_LITERAL)
        {
            tokenList->eat(2); // eat '-' and number

            auto exp = new Ast_FloatingPointLiteralExpression;
            exp->number = -nextToken->doubleVal;
            return exp;
        }
    }

    if (isUnaryOperator(token->type))
    {
        tokenList->eat(); // eat operator

        auto exp = new Ast_UnaryOperatorExpression;
        exp->operand = parseExpression(tokenList, ast);
        exp->operatorSymbol = static_cast<char>(token->type);

        return exp;
    }

    return nullptr;
};

Ast_Expression *parseExpression(TokenList *tokenList, Ast *ast)
{
    auto firstTerm = parseTerm(tokenList, ast);
    if (!firstTerm)
        return nullptr;

    auto token = tokenList->peek_next();
    switch (token->type)
    {
    case static_cast<TokenType>('('): // function call
    {
        tokenList->eat(); // eat '('

        auto exp = new Ast_FunctionCallExpression;
        exp->function = firstTerm;

        token = tokenList->peek_next();
        while (token->type != static_cast<TokenType>(')'))
        {
            exp->arguments.push_back(parseExpression(tokenList, ast));

            token = tokenList->peek_next();
            if (token->type == static_cast<TokenType>(','))
            {
                tokenList->eat();
                token = tokenList->peek_next();
            }
        }

        tokenList->eat(); // eat ')'

        return exp;
    }
    break;
    case static_cast<TokenType>('='):
    {
        tokenList->eat(); // eat '='

        auto rhs = parseExpression(tokenList, ast);

        auto exp = new Ast_AssignmentExpression;
        exp->lhs = firstTerm;
        exp->rhs = rhs;

        return exp;
    }
    break;
    }

    if (isBinaryOperator(token->type)) // add all binary operators
    {
        tokenList->eat(); // eat operator

        auto exp = new Ast_BinaryOperatorExpression;
        exp->leftOperand = firstTerm;
        exp->operatorSymbol = static_cast<char>(token->type);
        exp->rightOperand = parseExpression(tokenList, ast);

        return exp;
    }

    return firstTerm;
};

Ast_Expression *parseFunction(TokenList *tokenList, Ast *ast)
{
    tokenList->eat(); // eat '('

    std::vector<Ast_DeclarationStatement *> params;
    auto firstStatement = parseDeclaration(tokenList, ast);
    if (firstStatement)
    {
        params.push_back(firstStatement);

        while (tokenList->peek_next()->type == static_cast<TokenType>(','))
        {
            tokenList->eat(); // eat ','
            params.push_back(parseDeclaration(tokenList, ast));
        }
    }
    if (tokenList->peek_next()->type != static_cast<TokenType>(')'))
    {
        // @VALIDATE error
    }
    tokenList->eat(); // eat ')'

    auto header = new Ast_FunctionHeaderExpression;
    header->parameters = params;

    auto token = tokenList->peek_next();
    if (token->type == TokenType::T_ARROW)
    {
        tokenList->eat(); // eat arrow

        auto returnType = parseExpression(tokenList, ast);
        if (!returnType)
        {
            // @VALIDATE error expected type after arrow
        }
        header->returnType = returnType;
    }

    token = tokenList->peek_next();
    if (tokenList->peek_next()->type == static_cast<TokenType>('{'))
    {
        tokenList->eat(); // eat '{'

        auto definition = new Ast_FunctionDefinitionExpression;
        definition->header = header;
        definition->body = parseAst(tokenList);

        // @VALIDATE next token should be }
        tokenList->eat(); // eat '}'

        return definition;
    }

    return header;
};

bool isDeclaration(TokenList *tokenList)
{
    auto nextNextToken = tokenList->peek(1);
    return (nextNextToken->type == static_cast<TokenType>(':') ||
            nextNextToken->type == static_cast<TokenType>(','));
};

bool isFunction(TokenList *tokenList)
{
    uint32_t lookahead = 0; // starting past the expected '('
    while (tokenList->peek(lookahead)->type != static_cast<TokenType>(')'))
        lookahead++;

    auto t = tokenList->peek(lookahead + 1);
    return (t->type == TokenType::T_ARROW) || (t->type == static_cast<TokenType>('{'));
};

bool isUnaryOperator(TokenType c)
{
    const std::set<TokenType> operators = std::set<TokenType>{static_cast<TokenType>('-')};
    return operators.count(c);
};

bool isBinaryOperator(TokenType c)
{
    const std::set<TokenType> operators = std::set<TokenType>{static_cast<TokenType>('+'), static_cast<TokenType>('-')};
    return operators.count(c);
};