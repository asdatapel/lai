#include <iostream>
#include <fstream>
#include <string>
#include <vector>

struct Lexer
{
    enum struct TokenType
    {
        T_UNKNOWN,

        T_EOF,

        T_IDENTIFIER,
        T_NUMBER,
        T_STRING_LITERAL,

        T_COMPILER_DIRECTIVE,
        T_RETURN,

        T_SYMBOL,
    };

    struct TokenData
    {
        TokenType type = TokenType::T_UNKNOWN;

        std::string identifier = "";
        char symbol = ' ';

        std::string stringVal = "";
        double doubleVal = 0.0;

    } token;

    char lastChar = ' ';
    std::ifstream file;

    Lexer(std::string filename) : file(filename)
    {
        next(); // start off with the first token
    };

    ~Lexer()
    {
        file.close();
    };

    void next()
    {

        if (file.eof())
        {
            token.type = TokenType::T_EOF;
            return;
        }

        while (std::isspace(lastChar))
        {
            file.get(lastChar);

            if (file.eof())
            {
                token.type = TokenType::T_EOF;
                return;
            }
        }

        if (std::isalpha(lastChar))
        {
            token.identifier = lastChar;

            file.get(lastChar);
            while (std::isalnum(lastChar)) // identifiers should include '.' (when we have structs or namespaces) and '*' (for defining a pointer type or referencing)
            {
                token.identifier += lastChar;
                file.get(lastChar);
            }

            if (token.identifier == "return")
            {
                token.type = TokenType::T_RETURN;
            }
            else
            {
                token.type = TokenType::T_IDENTIFIER;
            }
            return;
        }

        if (std::isdigit(lastChar))
        {
            token.identifier = lastChar;

            file.get(lastChar);
            while (std::isdigit(lastChar) || lastChar == '.')
            {
                token.identifier += lastChar;
                file.get(lastChar);
            }

            token.doubleVal = strtod(token.identifier.c_str(), 0);
            token.type = TokenType::T_NUMBER;
            return;
        }

        if (lastChar == '#')
        {
            token.identifier = "";

            file.get(lastChar);
            while (std::isalnum(lastChar))
            {
                token.identifier += lastChar;
                file.get(lastChar);
            }
            token.type = TokenType::T_COMPILER_DIRECTIVE;
            return;
        }

        if (lastChar == '"')
        {
            token.identifier = "";

            file.get(lastChar);
            while (lastChar != '"') // @TODO(asad): need to account for escaped quotes
            {
                token.identifier += lastChar;
                file.get(lastChar);
            }

            file.get(lastChar); // swallow the quote

            token.stringVal = token.identifier;
            token.type = TokenType::T_STRING_LITERAL;
            return;
        }

        token.symbol = lastChar;
        file.get(lastChar); // swallow the unkown char

        token.type = TokenType::T_SYMBOL;
        return;
    }

    bool isSymbol(char symbol)
    {
        return token.symbol == symbol;
    };
};
