#include <string>
#include <vector>

#include "lexer.h"
#include "ast.h"

Ast *Ast::parse(Lexer *lexer)
{
    auto ast = new Ast;

    // TODO(asad): the rest of this should go in Ast_Statement::parse
    while (lexer->token.type != Lexer::TokenType::T_EOF && !lexer->isSymbol('}'))
    {
        if (lexer->token.type == Lexer::TokenType::T_IDENTIFIER)
        {
            std::string identifier = lexer->token.identifier;
            lexer->next();

            if (lexer->isSymbol(':'))
            {
                ast->statements.push_back(Ast_DeclarationStatement::parse(lexer, identifier));
            }
            else if (lexer->isSymbol('='))
            {
                ast->statements.push_back(Ast_AssignmentStatement::parse(lexer, identifier));
            }
        }
        else if (false) // if its a defer statement
        {
        }
        else if (lexer->token.type == Lexer::TokenType::T_RETURN)
        {
            ast->statements.push_back(Ast_ReturnStatement::parse(lexer));
        }
        else
        {
            lexer->next();
        }
    }

    lexer->next(); // eat the possible '}'

    return ast;
};

Ast_Expression *parseExpression(Lexer *lexer)
{
    Ast_Expression *expression = NULL;

    Ast_Expression *lhs = nullptr;

    // part one of the expression
    if (lexer->isSymbol('.'))
    {
        lexer->next();
        // append to variable_expression
    }
    else if (lexer->isSymbol('='))
    {
        lexer->next();
        lhs = Ast_AssignmentExpression::parse(lexer, lhs);
    }
    else if (lexer->token.type == Lexer::TokenType::T_NUMBER)
    {
        lhs = Ast_NumberExpression::parse(lexer);
    }
    else if (lexer->token.type == Lexer::TokenType::T_IDENTIFIER)
    {
        auto e = new Ast_VarReferenceExpression;
        e->name = lexer->token.identifier;
        lhs = e;

        lexer->next(); // eat identifier

        // move this to parsestatement???
        if (lexer->isSymbol(','))
        {
            lexer->next();
            // create a list of varExpressions and return
            // VALIDATE that all the expressions are simple varexpressions
        }
    }
    else if (lexer->isSymbol('+'))
    {
        lexer->next(); // eat '+'

        lhs = Ast_BinaryPlusExpression::parse(lexer, lhs);
    }
    else if (lexer->isSymbol('('))
    {
        lexer->next(); //eat '('

        // parseFunction attempts to parse an expression starting with '(' as a function
        // if it fails, it may return a single identifier (the start of a parenthesis expression)
        auto parseFunction = [](Lexer *lexer) -> std::pair<Ast_FunctionDefinitionExpression *, std::string> {
            std::string firstIdentifier = "";
            bool isFunction = false;

            if (lexer->isSymbol(')'))
            {
                isFunction = true;
            }
            else if (lexer->token.type == Lexer::TokenType::T_IDENTIFIER)
            {
                firstIdentifier = lexer->token.identifier;
                lexer->next(); // eat identifier

                if (lexer->isSymbol(':') || lexer->isSymbol(','))
                {
                    isFunction = true;
                }
            }

            if (!isFunction)
            {
                return {nullptr, firstIdentifier};
            }

            std::vector<Ast_DeclarationStatement> parameters;
            if ()
        };
    }

    // possible part two of the expression
    if (lexer->isSymbol('('))
    {
        lexer->next(); //eat '('

        // function call, where lhs is the function
    }

    if (lexer->isSymbol(';') || lexer->isSymbol(')') || lexer->isSymbol('}'))
    {
        return expression;
    }

    return expression;
};

Ast_Statement *parseStatement(Lexer *lexer)
{
    // if (lexer->token.type == Lexer::TokenType::T_FOR){
    //     return;
    // }

    Ast_Expression *lhs = parseExpression(lexer);

    if (lexer->isSymbol(':'))
    {
        // declaration

        // lhs = declaration???
    }

    if (lexer->isSymbol(':')) // possible second ':'
    {
        // assignment
    }

    if (lexer->isSymbol('='))
    {
        // assignment
    }

    if (!lexer->isSymbol(';'))
    {
        // something is wrong ???
    }
    lexer->next(); // eat ';'
};

Ast_Expression *Ast_Expression::parse(Lexer *lexer)
{
    Ast_Expression *expression = NULL;
    if (lexer->token.type == Lexer::TokenType::T_NUMBER)
    {
        expression = Ast_NumberExpression::parse(lexer);
    }
    else if (lexer->token.type == Lexer::TokenType::T_IDENTIFIER)
    {
        std::string identifier = lexer->token.identifier;

        lexer->next(); // eat identifier

        // TODO(asad): if next char is '(' this is a function call

        if (lexer->isSymbol(':'))
        {
            // if the next character is a ':', it means this is a declaration inside of
            // an expression, which (I think) must mean that we are defining a function
            // TODO(asad): this seems hacky
            expression = Ast_FunctionDefinitionExpression::parse(lexer, identifier);
        }
        else if (lexer->isSymbol(':'))
        {
            // if the next character is a ',', it means this is a function definition
            // TODO(asad): this also seems hacky
            std::vector<Ast_DeclarationStatement *> parameters;
        }
        else
        {
            auto varExpression = new Ast_VarReferenceExpression;
            varExpression->name = lexer->token.identifier;
            expression = varExpression;
        }
    }
    else if (lexer->isSymbol('('))
    {
        expression = Ast_ParensExpression::parse(lexer);
    }

    if (lexer->isSymbol('+'))
    {
        lexer->next(); // eat '+'
        auto lhs = expression;
        auto rhs = Ast_Expression::parse(lexer);

        auto plusExpressson = new Ast_BinaryPlusExpression;
        plusExpressson->lhs = lhs;
        plusExpressson->rhs = rhs;

        expression = plusExpressson;
    }

    return expression;
};

Ast_Expression *Ast_ParensExpression::parse(Lexer *lexer)
{

    lexer->next(); // eat '('

    // if the next character is a ')', it means we are in an empty function parameter list
    // which could either be a function declaration or a function definition
    // TODO(asad): this also seems hacky
    if (lexer->isSymbol(')'))
    {
        lexer->next(); // eat the ')'

        if (lexer->isSymbol('{'))
        {
            lexer->next(); // eat '{'

            auto expression = new Ast_FunctionDefinitionExpression;
            expression->body = Ast::parse(lexer);

            return expression;
        }
        else
        {
            return new Ast_FunctionDeclarationExpression; // TODO(asad): need to set type here
        }
    }
    else
    {
        auto expression = Ast_Expression::parse(lexer);

        if (!lexer->isSymbol(')'))
        {
            // error
        }
        lexer->next(); // eat ')'
        return expression;
    }
}

Ast_NumberExpression *Ast_NumberExpression::parse(Lexer *lexer)
{
    auto e = new Ast_NumberExpression;
    e->number = lexer->token.doubleVal;

    lexer->next();

    return e;
}

Ast_FunctionDefinitionExpression *Ast_FunctionDefinitionExpression::parse(Lexer *lexer, std::string firstIdentifier)
{
    auto e = new Ast_FunctionDefinitionExpression;

    auto declaration = Ast_DeclarationStatement::parse(lexer, firstIdentifier);
    e->parameters.push_back(declaration);

    while (lexer->isSymbol(','))
    {
        lexer->next(); // eat the ','
        auto declaration = Ast_DeclarationStatement::parse(lexer);
        e->parameters.push_back(declaration);
    }

    if (!lexer->isSymbol(')'))
    {
        std::cout << "Error: cant find ')' !" << std::endl;
        // error
    }
    lexer->next(); // eat ')'

    if (!lexer->isSymbol('{'))
    {
        std::cout << "Error: missing function body!" << std::endl;
        // error
    }
    lexer->next(); // eat '{'

    e->body = Ast::parse(lexer);

    return e;
}

Ast_DeclarationStatement *Ast_DeclarationStatement::parse(Lexer *lexer)
{
    auto identifier = lexer->token.identifier;
    lexer->next();

    return Ast_DeclarationStatement::parse(lexer, identifier);
};

Ast_DeclarationStatement *Ast_DeclarationStatement::parse(Lexer *lexer, std::string identifier)
{
    auto e = new Ast_DeclarationStatement;
    e->identifier = identifier;

    lexer->next(); // eat ':'

    if (lexer->token.type == Lexer::TokenType::T_IDENTIFIER)
    {
        e->type = lexer->token.identifier;
        lexer->next(); // eat the identifier
    }

    bool isAlsoAssignment = false;
    if (lexer->isSymbol(':'))
    {
        isAlsoAssignment = true;
        e->constant = true;
        lexer->next(); // eat ':'
    }
    else if (lexer->isSymbol('='))
    {
        isAlsoAssignment = true;
        lexer->next(); // eat '='
    }

    if (isAlsoAssignment)
    {
        e->value = Ast_Expression::parse(lexer);
    }

    return e;
};

Ast_AssignmentStatement *Ast_AssignmentStatement::parse(Lexer *lexer, std::string identifier)
{
    auto e = new Ast_AssignmentStatement;
    e->identifier = identifier;

    lexer->next(); // eat '='

    e->value = Ast_Expression::parse(lexer);

    return e;
};

Ast_ReturnStatement *Ast_ReturnStatement::parse(Lexer *lexer)
{
    auto e = new Ast_ReturnStatement;
    lexer->next(); // eat 'return'

    e->value = Ast_Expression::parse(lexer);

    return e;
};

///////////////////////////////////////////////////////////////
// print functions

std::string Ast_FunctionDefinitionExpression::print()
{
    std::string params;
    for (auto p : parameters)
    {
        params += p->print();
    }
    return " Ast_FunctionParametersExpression : { parameters: " + params + ", body: " + body->print() + " }";
}

///////////////////////////////////////////////////////////////
int main(int argc, char *argv[])
{
    if (argc <= 1)
    {
        std::cout << "missing argument: build file" << std::endl;
    }

    char *filename = argv[1];

    Lexer lexer(filename);
    // while (lexer.token.type != Lexer::TokenType::T_EOF)
    // {

    //     static int HARD_STOP = 0;
    //     HARD_STOP++;
    //     if (HARD_STOP > 50)
    //         exit(0);

    //     if (lexer.token.type == Lexer::TokenType::T_SYMBOL)
    //     {
    //         std::cout << static_cast<int>(lexer.token.type) << ", " << lexer.token.symbol << std::endl;
    //     }
    //     else
    //     {
    //         std::cout << static_cast<int>(lexer.token.type) << ", " << lexer.token.identifier << std::endl;
    //     }

    //     lexer.next();
    // }

    auto ast = Ast::parse(&lexer);
    std::cout << ast->print() << std::endl;

    return 0;
}
