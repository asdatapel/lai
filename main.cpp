#include <iostream>

#include "token_list.h"
#include "ir.h"
#include "lexer.h"
#include "llvm_codegen.h"
#include "parser.h"

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
    case Ast_Expression::Type::FUNCTION_DEFINITION:
    {
        auto exp = (Ast_FunctionDefinitionExpression *)expression;
        std::string body = "[";
        for (auto s : exp->body->body)
        {
            body += printStatement(s, depth + 1) + ",";
        }
        body += "]";

        return "{ type : \"FUNCTION_DEFINITION\", header: " + printExpression(exp->header, depth + 1) +
               ", body : " + body +
               "}";
    }
    break;
    case Ast_Expression::Type::FUNCTION_HEADER:
    {
        auto exp = (Ast_FunctionHeaderExpression *)expression;
        std::string params = "[";
        for (auto p : exp->parameters)
        {
            params += printStatement(p, depth + 1) + ",";
        }
        params += "]";

        return indent(depth) + "{ type : \"FUNCTION_HEADER\", params: " + params +
               ", returnType: " + printExpression(exp->returnType, depth + 1) +
               "}";
    }
    break;
    case Ast_Expression::Type::INTEGER_LITERAL:
    {
        auto exp = (Ast_IntegerLiteralExpression *)expression;
        return indent(depth) + "{ type : \"NUMBER_EXPRESSION\", value: " + std::to_string(exp->number) + "}";
    }
    break;
    case Ast_Expression::Type::FLOAT_LITERAL:
    {
        auto exp = (Ast_FloatingPointLiteralExpression *)expression;
        return indent(depth) + "{ type : \"NUMBER_EXPRESSION\", value: " + std::to_string(exp->number) + "}";
    }
    break;
    case Ast_Expression::Type::VARIABLE:
    {
        auto exp = (Ast_VariableExpression *)expression;
        return indent(depth) + "{ type : \"VARIABLE\", identifier: \"" + exp->identifier.toString() + "\"}";
    }
    break;
    case Ast_Expression::Type::UNARY_OPERATION:
    {
        auto exp = (Ast_UnaryOperatorExpression *)expression;
        return indent(depth) + "{ type : \"UNARY_OPERATION\", operator: \"" + exp->operatorSymbol +
               "\", operand: " + printExpression(exp->operand, depth + 1) +
               "}";
    }
    break;
    case Ast_Expression::Type::BINARY_OPERATION:
    {
        auto exp = (Ast_BinaryOperatorExpression *)expression;
        return indent(depth) + "{ type : \"BINARY_OPERATION\", operator: \"" + exp->operatorSymbol +
               "\", left_operand: " + printExpression(exp->leftOperand, depth + 1) +
               ", right_operand: \n" + printExpression(exp->rightOperand, depth + 1) +
               "}";
    }
    break;
    case Ast_Expression::Type::ASSIGNMENT:
    {
        auto exp = (Ast_AssignmentExpression *)expression;
        return indent(depth) + "{ type : \"ASSIGNMENT\", lhs: " + printExpression(exp->lhs, depth + 1) +
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
    case Ast_Statement::Type::DECLARATION:
    {
        auto st = (Ast_DeclarationStatement *)statement;
        std::string identifiers = "[";
        for (auto i : st->identifiers)
        {
            identifiers += "\"" + i->identifier.toString() + "\",";
        }
        identifiers += "]";

        return "{ type : \"DECLARATION\", identifiers : " + identifiers +
               ", explicitType: " + printExpression(st->explicitType, depth + 1) +
               ", value: " + printExpression(st->value, depth + 1) +
               ", isConstant: " + (st->constant ? "true" : "false") +
               "}";
    }
    break;
    case Ast_Statement::Type::EXPRESSION:
    {
        auto st = (Ast_ExpressionStatement *)statement;
        return "{ type : \"EXPRESSION\", value: " + printExpression(st->value, depth) +
               "}";
    }
    break;
    case Ast_Statement::Type::RETURN:
    {
        auto st = (Ast_ReturnStatement *)statement;
        return "{ type : \"RETURN\", value: " + printExpression(st->value, depth) +
               "}";
    }
    break;
    }
    
    return "";
};

int main(int argc, char *argv[])
{
    if (argc <= 1)
    {
        std::cout << "missing argument: build file" << std::endl;
    }

    TokenList tokenList = Lexer::lexFile(argv[1]);

    auto ast = parseAst(&tokenList);
    //resolveTypes(ast);

    //std::cout << ast->statements.size() << std::endl;
    for (auto s : ast->body)
    {
        std::cout << printStatement(s, 0) << std::endl;
    }

    auto ir = irify(ast);
    codegenModule(ir);

    //std::cout << "\n\n\n\n\n";
    // for (auto x : ast->declarations)
    // {
        //std::cout << x.first << ", " << x.second << std::endl;
    // }

    //std::cout << "\n\n\n\n\n";
    //codegenModule(ast);

    // auto token = tokenList.peek_next();
    // std::cout << "LEN: " << tokenList.contents.length << "\n";
    // while (token.type != TokenType::T_EOF)
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
    //     else if (token.type == TokenType::T_IDENTIFIER)
    //     {
    //         std::cout.write(token.identifier.data, token.identifier.length);
    //         std::cout << std::endl;
    //     }
    //     else if (token.type == TokenType::T_STRING_LITERAL)
    //     {
    //         std::cout.write(token.stringVal.data, token.stringVal.length);
    //         std::cout << std::endl;
    //     }
    //     else if (token.type == TokenType::T_NUMBER)
    //     {
    //         std::cout << token.doubleVal << std::endl;
    //     }
    //     else if (token.type == TokenType::T_RETURN)
    //     {
    //         std::cout << "return" << std::endl;
    //     }
    //     else
    //     {
    //         std::cout << static_cast<int>(token.type) << ", ";
    //         std::cout.write(token.identifier.data, token.identifier.length);
    //         std::cout << std::endl;
    //     }

    //     tokenList.next();
    //     token = tokenList.peek_next();
    // }
}