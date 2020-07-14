#include <vector>

#include "lexer.h"

// forward decs
struct Ast;
struct Ast_Statement;
struct Ast_Expression;
struct Ast_VariableExpression;
Ast_Expression *parseTerm(Lexer *);
Ast_Expression *parseExpression(Lexer *);
Ast_Statement *parseStatement(Lexer *);
bool checkDeclaration(Lexer *);
Ast_Statement *parseDeclaration(Lexer *);

struct Ast
{
    std::vector<Ast_Statement *> statements;
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
    };

    Type type;
};
struct Ast_ExpressionStatement : Ast_Statement
{
    Ast_ExpressionStatement() { type = Type::EXPRESSION_STATEMENT; };

    Ast_Expression *value;
};
struct Ast_DeclarationStatement : Ast_Statement
{
    Ast_DeclarationStatement() { type = Type::DECLARATION_STATEMENT; };

    bool constant;
    std::vector<Ast_VariableExpression *> identifiers;
    Ast_Expression *explicitType;
    Ast_Expression *value;
};
struct Ast_ReturnStatement : Ast_Statement
{
    Ast_ReturnStatement() { type = Type::RETURN_STATEMENT; };

    Ast_Expression *value;
};
struct Ast_NumberExpression : Ast_Expression
{
    Ast_NumberExpression() { type = Type::NUMBER_EXPRESSION; };

    double number;
};
struct Ast_VariableExpression : Ast_Expression
{
    Ast_VariableExpression() { type = Type::VARIABLE_EXPRESSION; };

    String identifier;
};
struct Ast_AssignmentExpression : Ast_Expression
{
    Ast_AssignmentExpression() { type = Type::ASSIGNMENT_EXPRESSION; };

    Ast_Expression *lhs;
    Ast_Expression *rhs;
};
struct Ast_UnaryOperatorExpression : Ast_Expression
{
    Ast_UnaryOperatorExpression() { type = Type::UNARY_OPERATION_EXPRESSION; };

    char operatorSymbol = 0;
    Ast_Expression *operand;
};
struct Ast_BinaryOperatorExpression : Ast_Expression
{
    Ast_BinaryOperatorExpression() { type = Type::BINARY_OPERATION_EXPRESSION; };

    char operatorSymbol = 0;
    Ast_Expression *leftOperand;
    Ast_Expression *rightOperand;
};
struct Ast_FunctionHeaderExpression : Ast_Expression
{
    Ast_FunctionHeaderExpression() { type = Type::FUNCTION_HEADER_EXPRESSION; };

    std::vector<Ast_Statement *> parameters;
    Ast_Expression *returnType;
};
struct Ast_FunctionDefinitionExpression : Ast_Expression
{
    Ast_FunctionDefinitionExpression() { type = Type::FUNCTION_DEFINITION_EXPRESSION; };

    Ast_FunctionHeaderExpression *header;
    Ast *body;
};

Ast *parseAst(Lexer *lexer)
{
    auto ast = new Ast;

    auto token = lexer->peek_next();
    while (token.type != Lexer::TokenType::T_EOF && token.type != static_cast<Lexer::TokenType>('}'))
    {
        auto st = parseStatement(lexer);
        ast->statements.push_back(st);

        if (lexer->peek_next().type != static_cast<Lexer::TokenType>(';'))
        {
            // @VALIDATE error
            std::cout << "error 131" << std::endl;
        }

        lexer->next(); // eat ';'
        token = lexer->peek_next();
    }

    return ast;
}

Ast_Statement *parseStatement(Lexer *lexer)
{
    Ast_Statement *statement = nullptr;

    auto token = lexer->peek_next();
    switch (token.type)
    {
    case static_cast<Lexer::TokenType>('#'):
    {
        lexer->next(); // eat '#'
        // auto st = parseCompilerDirective(lexer);
        statement = nullptr;
    }
    break;
    case Lexer::TokenType::T_RETURN:
    {
        lexer->next(); // eat 'return'

        auto exp = parseExpression(lexer);

        auto st = new Ast_ReturnStatement;
        st->value = exp;

        statement = st;
    }
    break;
    default:
    {
        if (checkDeclaration(lexer))
        {
            statement = parseDeclaration(lexer);
        }
        else
        {
            auto exp = parseExpression(lexer);
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

bool checkDeclaration(Lexer *lexer)
{
    auto nextNextToken = lexer->peek(1);
    return (nextNextToken.type == static_cast<Lexer::TokenType>(':') ||
            nextNextToken.type == static_cast<Lexer::TokenType>(','));
};

Ast_Statement *parseDeclaration(Lexer *lexer)
{
    auto token = lexer->peek_next();
    auto st = new Ast_DeclarationStatement;

    auto addIdentiferToLhs = [&]() {
        if (token.type != Lexer::TokenType::T_IDENTIFIER)
        {
            // @VALIDATE must be identifier
            std::cout << "error207" << std::endl;
        };

        auto varExp = new Ast_VariableExpression;
        varExp->identifier = token.identifier;
        st->identifiers.push_back(varExp);

        lexer->next();
        token = lexer->peek_next();
    };
    addIdentiferToLhs();
    while (token.type == static_cast<Lexer::TokenType>(','))
    {
        lexer->next();
        token = lexer->peek_next();

        addIdentiferToLhs();
    }

    if (token.type != static_cast<Lexer::TokenType>(':'))
    {
        // @VALIDATE error
        std::cout << "error229" << std::endl;
    }
    lexer->next(); // eat ':'
    st->explicitType = parseExpression(lexer);

    token = lexer->peek_next();

    if (token.type == static_cast<Lexer::TokenType>(':') || token.type == static_cast<Lexer::TokenType>('='))
    {
        st->constant = (token.type == static_cast<Lexer::TokenType>(':'));
        lexer->next(); // eat ':' or '='

        st->value = parseExpression(lexer);
    }

    return st;
};

Ast_Expression *parseTerm(Lexer *lexer)
{
    auto token = lexer->peek_next();
    switch (token.type)
    {
    case static_cast<Lexer::TokenType>('('):
    {
        // good place to check for a function declaration ??
        lexer->next(); // eat '('

        std::vector<Ast_Statement *> statements;
        auto firstStatement = parseStatement(lexer);
        if (firstStatement)
        {
            statements.push_back(firstStatement);

            while (lexer->peek_next().type == static_cast<Lexer::TokenType>(','))
            {
                lexer->next(); // eat ','
                statements.push_back(parseStatement(lexer));
            }
        }

        if (lexer->peek_next().type != static_cast<Lexer::TokenType>(')'))
        {
            // @VALIDATE error
            std::cout << "error 268" << std::endl;
        }
        lexer->next(); // eat ')'
        token = lexer->peek_next();

        switch (token.type)
        {
        case Lexer::TokenType::T_ARROW:
        {
            lexer->next(); // eat arrow
            token = lexer->peek_next();

            auto exp = new Ast_FunctionHeaderExpression;
            exp->parameters = statements;
            exp->returnType = parseExpression(lexer);

            if (lexer->peek_next().type == static_cast<Lexer::TokenType>('{'))
            {
                lexer->next(); // eat '{'

                auto funcDef = new Ast_FunctionDefinitionExpression;
                funcDef->header = exp;
                funcDef->body = parseAst(lexer);

                // @VALIDATE next token should be }
                lexer->next(); // eat '}'

                return funcDef;
            }
            else
            {
                return exp;
            }
        }
        break;
        default:
        {
            // @VALIDATE statements.size() == 1
            // @VALIDATE statements[0].type == EXPRESSION_STATEMENT
            return ((Ast_ExpressionStatement *)statements[0])->value;
        }
        break;
        }
    }
    break;
    case Lexer::TokenType::T_NUMBER:
    {
        lexer->next(); // eat number

        auto exp = new Ast_NumberExpression;
        exp->number = token.doubleVal;
        return exp;
    }
    break;
    case Lexer::TokenType::T_IDENTIFIER:
    {
        lexer->next(); // eat identifier

        auto exp = new Ast_VariableExpression;
        exp->identifier = token.identifier;
        return exp;
    }
    break;
    case static_cast<Lexer::TokenType>('!'): // need to add all unary operators
    {
        lexer->next(); // eat operator

        auto exp = new Ast_UnaryOperatorExpression;
        exp->operatorSymbol = static_cast<char>(token.type);
        exp->operand = parseExpression(lexer);

        return exp;
    }
    break;
    }

    return nullptr;
};

Ast_Expression *parseExpression(Lexer *lexer)
{
    auto firstTerm = parseTerm(lexer);
    if (!firstTerm)
        return nullptr;

    auto token = lexer->peek_next();
    switch (token.type)
    {
    case static_cast<Lexer::TokenType>('='):
    {
        lexer->next(); // eat '='

        auto rhs = parseExpression(lexer);

        auto exp = new Ast_AssignmentExpression;
        exp->lhs = firstTerm;
        exp->rhs = rhs;

        return exp;
    }
    break;
    case static_cast<Lexer::TokenType>('+'): // add all binary operators
    {
        lexer->next(); // eat operator

        auto exp = new Ast_BinaryOperatorExpression;
        exp->leftOperand = firstTerm;
        exp->operatorSymbol = static_cast<char>(token.type);
        exp->rightOperand = parseExpression(lexer);

        return exp;
    }
    break;
    }

    return firstTerm;
};

std::string indent(int depth)
{
    std::string out = "";
    // for (int i = 0; i < depth; ++i)
    // {
    //     out += '\t';
    // }
    return out;
};

std::string printStatement(Ast_Statement *, int);
std::string printExpression(Ast_Expression *, int);

std::string printExpression(Ast_Expression *expression, int depth)
{
    if (!expression)
    {
        return "\"nullptr\"";
    }

    switch (expression->type)
    {
    case Ast_Expression::Type::FUNCTION_DEFINITION_EXPRESSION:
    {
        auto exp = (Ast_FunctionDefinitionExpression *)expression;
        std::string body = "[";
        for (auto s : exp->body->statements)
        {
            body += printStatement(s, depth + 1) + ",";
        }
        body += "]";

        return "{ type : \"FUNCTION_DEFINITION_EXPRESSION\", header: " + printExpression(exp->header, depth + 1) +
               ", body : " + body +
               "}";
    }
    break;
    case Ast_Expression::Type::FUNCTION_HEADER_EXPRESSION:
    {
        auto exp = (Ast_FunctionHeaderExpression *)expression;
        std::string params = "[";
        for (auto p : exp->parameters)
        {
            params += printStatement(p, depth + 1)+ ",";
        }
        params += "]";

        return indent(depth) + "{ type : \"FUNCTION_HEADER_EXPRESSION\", params: " + params +
               ", returnType: " + printExpression(exp->returnType, depth + 1) +
               "}";
    }
    break;
    case Ast_Expression::Type::NUMBER_EXPRESSION:
    {
        auto exp = (Ast_NumberExpression *)expression;
        return indent(depth) + "{ type : \"NUMBER_EXPRESSION\", value: " + std::to_string(exp->number) + "}";
    }
    break;
    case Ast_Expression::Type::VARIABLE_EXPRESSION:
    {
        auto exp = (Ast_VariableExpression *)expression;
        return indent(depth) + "{ type : \"VARIABLE_EXPRESSION\", identifier: \"" + exp->identifier.toString() + "\"}";
    }
    break;
    case Ast_Expression::Type::UNARY_OPERATION_EXPRESSION:
    {
        auto exp = (Ast_UnaryOperatorExpression *)expression;
        return indent(depth) + "{ type : \"UNARY_OPERATION_EXPRESSION\", operator: \"" + exp->operatorSymbol +
               "\", operand: " + printExpression(exp->operand, depth + 1) +
               "}";
    }
    break;
    case Ast_Expression::Type::BINARY_OPERATION_EXPRESSION:
    {
        auto exp = (Ast_BinaryOperatorExpression *)expression;
        return indent(depth) + "{ type : \"BINARY_OPERATION_EXPRESSION\", operator: \"" + exp->operatorSymbol +
               "\", left_operand: " + printExpression(exp->leftOperand, depth + 1) +
               ", right_operand: \n" + printExpression(exp->rightOperand, depth + 1) +
               "}";
    }
    break;
    case Ast_Expression::Type::ASSIGNMENT_EXPRESSION:
    {
        auto exp = (Ast_AssignmentExpression *)expression;
        return indent(depth) + "{ type : \"ASSIGNMENT_EXPRESSION\", lhs: " + printExpression(exp->lhs, depth + 1) +
               ", rhs: " + printExpression(exp->rhs, depth + 1) +
               "}";
    }
    break;
    default:
    {
        return "exp";
    }
    break;
    }
};

std::string printStatement(Ast_Statement *statement, int depth)
{
    switch (statement->type)
    {
    case Ast_Statement::Type::DECLARATION_STATEMENT:
    {
        auto st = (Ast_DeclarationStatement *)statement;
        std::string identifiers = "[";
        for (auto i : st->identifiers)
        {
            identifiers += "\"" +  i->identifier.toString() + "\",";
        }
        identifiers += "]";

        return "{ type : \"DECLARATION_STATEMENT\", identifiers : " + identifiers +
               ", explicitType: " + printExpression(st->explicitType, depth + 1) +
               ", value: " + printExpression(st->value, depth + 1) +
               ", isConstant: " + (st->constant ? "true" : "false") +
               "}";
    }
    break;
    case Ast_Statement::Type::EXPRESSION_STATEMENT:
    {
        auto st = (Ast_ExpressionStatement *)statement;
        return "{ type : \"EXPRESSION_STATEMENT\", value: " + printExpression(st->value, depth) +
               "}";
    }
    break;
    case Ast_Statement::Type::RETURN_STATEMENT:
    {
        auto st = (Ast_ReturnStatement *)statement;
        return "{ type : \"RETURN_STATEMENT\", value: " + printExpression(st->value, depth) +
               "}";
    }
    break;
    }
};

int main(int argc, char *argv[])
{
    if (argc <= 1)
    {
        std::cout << "missing argument: build file" << std::endl;
    }

    Lexer lexer(argv[1]);

    auto ast = parseAst(&lexer);

    std::cout << ast->statements.size() << std::endl;
    for (auto s : ast->statements)
    {
        std::cout << printStatement(s, 0) << std::endl;
    }

    // auto token = lexer.peek_next();
    // std::cout << "LEN: " << lexer.contents.length << "\n";
    // while (token.type != Lexer::TokenType::T_EOF)
    // {
    //     /////////// for debugging infinite loop
    //     {
    //         static int HARD_STOP = 0;
    //         HARD_STOP++;
    //         if (HARD_STOP > 1000)
    //             exit(0);
    //     }
    //     ///////////////////////////////////////

    //     if (static_cast<uint32_t>(token.type) < 256)
    //     {
    //         std::cout << static_cast<char>(token.type) << std::endl;
    //     }
    //     else if (token.type == Lexer::TokenType::T_IDENTIFIER)
    //     {
    //         std::cout.write(token.identifier.data, token.identifier.length);
    //         std::cout << std::endl;
    //     }
    //     else if (token.type == Lexer::TokenType::T_STRING_LITERAL)
    //     {
    //         std::cout.write(token.stringVal.data, token.stringVal.length);
    //         std::cout << std::endl;
    //     }
    //     else if (token.type == Lexer::TokenType::T_NUMBER)
    //     {
    //         std::cout << token.doubleVal << std::endl;
    //     }
    //     else if (token.type == Lexer::TokenType::T_RETURN)
    //     {
    //         std::cout << "return" << std::endl;
    //     }
    //     else
    //     {
    //         std::cout << static_cast<int>(token.type) << ", ";
    //         std::cout.write(token.identifier.data, token.identifier.length);
    //         std::cout << std::endl;
    //     }

    //     lexer.next();
    //     token = lexer.peek_next();
    // }
}