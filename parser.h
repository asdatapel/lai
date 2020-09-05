#pragma once

#include <iostream>
#include <set>
#include <vector>

#include "llvm.h"

#include "ast.h"
#include "token_list.h"
#include "types.h"

Ast_Statement *parseStatement(TokenList *);
Ast_DeclarationStatement *parseDeclaration(TokenList *);

Ast_Expression *parseExpressionTerm(TokenList *);
Ast_Expression *parseExpressionPostfix(TokenList *);
Ast_Expression *parseExpression(TokenList *);
Ast_Expression *parseFunction(TokenList *);

bool isDeclaration(TokenList *);
bool isFunction(TokenList *);
bool isUnaryOperator(TokenType c);
bool isBinaryOperator(TokenType c);

Ast_BlockStatement *parseAst(TokenList *tokenList)
{
    auto rootBlock = new Ast_BlockStatement;

    auto token = tokenList->peek_next();
    while (token->type != TokenType::T_EOF && token->type != static_cast<TokenType>('}'))
    {
        auto st = parseStatement(tokenList);
        rootBlock->body.push_back(st);

        if (tokenList->peek_next()->type != static_cast<TokenType>(';'))
        {
            // @VALIDATE error
        }

        tokenList->eat(); // eat ';'
        token = tokenList->peek_next();
    }

    return rootBlock;
}

Ast_Statement *parseStatement(TokenList *tokenList)
{
    Ast_Statement *statement = nullptr;

    auto token = tokenList->peek_next();
    switch (token->type)
    {
    case static_cast<TokenType>('{'):
    {
        tokenList->eat(); // eat '{'
        auto block = parseAst(tokenList);

        if (tokenList->peek_next()->type != static_cast<TokenType>('}'))
        {
            // @VALIDATE error
        }

        statement = block;
    }
    break;
    case TokenType::T_RETURN:
    {
        tokenList->eat(); // eat 'return'

        auto exp = parseExpression(tokenList);

        auto st = new Ast_ReturnStatement;
        st->value = exp;

        statement = st;
    }
    break;
    default:
    {
        if (isDeclaration(tokenList))
        {
            statement = parseDeclaration(tokenList);
        }
        else
        {
            auto exp = parseExpression(tokenList);
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

Ast_DeclarationStatement *parseDeclaration(TokenList *tokenList)
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
    st->explicitType = parseExpressionTerm(tokenList);

    token = tokenList->peek_next();

    if (token->type == static_cast<TokenType>(':') || token->type == static_cast<TokenType>('='))
    {
        st->constant = (token->type == static_cast<TokenType>(':'));
        tokenList->eat(); // eat ':' or '='

        st->value = parseExpression(tokenList);
    }

    return st;
};

Ast_Expression *parseExpressionTerm(TokenList *tokenList)
{
    auto token = tokenList->peek_next();
    switch (token->type)
    {
    case static_cast<TokenType>('('):
    {
        if (isFunction(tokenList))
        {
            return parseFunction(tokenList);
        }

        tokenList->eat(); // '('
        auto exp = parseExpression(tokenList);
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
    case TokenType::T_FALSE:
    case TokenType::T_TRUE:
    {
        tokenList->eat(); // eat false

        auto exp = new Ast_BooleanExpression;
        exp->value = (token->type == TokenType::T_TRUE);
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
    case TokenType::T_IF:
    {
        tokenList->eat(); // eat 'if'

        auto exp = new Ast_IfExpression;
        exp->condition = parseExpression(tokenList);
        exp->body = parseStatement(tokenList);
        return exp;
    }
    break;
    }

    // special case for handling negative number literals
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

    // seperate from the switch case becuase switch/case doesn't support functions
    if (isUnaryOperator(token->type))
    {
        tokenList->eat(); // eat operator

        auto exp = new Ast_UnaryOperatorExpression;
        exp->operand = parseExpressionTerm(tokenList);
        exp->operatorSymbol = static_cast<char>(token->type);

        return exp;
    }

    return nullptr;
};

Ast_Expression *parseExpressionPostfix(TokenList *tokenList, Ast_Expression *prevExpression)
{
    auto token = tokenList->peek_next();
    switch (token->type)
    {
    case static_cast<TokenType>('('): // function call
    {
        tokenList->eat(); // eat '('

        auto call = new Ast_FunctionCallExpression;
        call->function = prevExpression;

        token = tokenList->peek_next();
        while (token->type != static_cast<TokenType>(')'))
        {
            call->arguments.push_back(parseExpression(tokenList));

            token = tokenList->peek_next();
            if (token->type == static_cast<TokenType>(','))
            {
                tokenList->eat();
                token = tokenList->peek_next();
            }
        }

        tokenList->eat(); // eat ')'
        token = tokenList->peek_next();

        return call;
    }
    break;
    case static_cast<TokenType>('['): // function call
    {
        tokenList->eat(); // eat '['

        auto indexOp = new Ast_IndexExpression;
        indexOp->oprand = prevExpression;
        indexOp->index = parseExpression(tokenList);

        if (tokenList->peek_next()->type == static_cast<TokenType>(']'))
        {
            // @VALIDATE
        }
        tokenList->eat(); // eat ']'
        return indexOp;
    }
    break;
    }

    return nullptr;
};

Ast_Expression *parseExpression(TokenList *tokenList)
{
    auto firstTerm = parseExpressionTerm(tokenList);
    if (!firstTerm)
        return nullptr;

    while (auto postFixed = parseExpressionPostfix(tokenList, firstTerm))
    {
        firstTerm = postFixed;
    }
    auto token = tokenList->peek_next();

    switch (token->type)
    {
    case static_cast<TokenType>('='):
    {
        tokenList->eat(); // eat '='

        auto rhs = parseExpression(tokenList);

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
        exp->operatorSymbol = token->type;
        exp->rightOperand = parseExpression(tokenList);

        return exp;
    }

    return firstTerm;
};

Ast_Expression *parseFunction(TokenList *tokenList)
{
    tokenList->eat(); // eat '('

    std::vector<Ast_DeclarationStatement *> params;
    auto firstStatement = parseDeclaration(tokenList);
    if (firstStatement)
    {
        params.push_back(firstStatement);

        while (tokenList->peek_next()->type == static_cast<TokenType>(','))
        {
            tokenList->eat(); // eat ','
            params.push_back(parseDeclaration(tokenList));
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

        auto returnType = parseExpression(tokenList);
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
    const std::set<TokenType> operators =
        std::set<TokenType>{
            static_cast<TokenType>('-'),
            static_cast<TokenType>('.'),
            static_cast<TokenType>('*')};
    return operators.count(c);
};

bool isBinaryOperator(TokenType c)
{
    const std::set<TokenType> operators =
        std::set<TokenType>{
            static_cast<TokenType>('+'),
            static_cast<TokenType>('-'),
            static_cast<TokenType>('<'),
            static_cast<TokenType>('>'),
            TokenType::T_NOT_EQUAL,
            TokenType::T_GREATER_THAN_EQUAL,
            TokenType::T_LESS_THAN_EQUAL,
            TokenType::T_DOUBLE_EQUAL};
    return operators.count(c);
};