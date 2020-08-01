#pragma once

#include <cctype>
#include <iostream>
#include <fstream>
#include <string>
#include <deque>

struct String
{
    char *data = nullptr;
    uint32_t length = 0;

    bool equals(const char *val)
    {
        for (int i = 0; i < length; ++i)
        {
            if (data[i] != val[i])
                return false;
        }

        return true;
    };

    std::string toString()
    {
        std::string out = "";
        for (int i = 0; i < length; ++i)
        {
            out += data[i];
        }
        return out;
    };
};

struct Lexer
{
    enum struct TokenType
    {
        T_UNKNOWN = 256,

        T_IDENTIFIER,
        T_NUMBER,
        T_STRING_LITERAL,

        T_ARROW,

        T_RETURN,

        T_EOF,
    };

    struct TokenData
    {
        TokenType type = TokenType::T_UNKNOWN;

        String identifier;
        String stringVal;
        double doubleVal;
    };

    String contents;

    uint32_t currentIndex = 0;
    uint32_t currentLine = 0;
    uint32_t currentColumn = 0;

    std::deque<TokenData> cache;

    Lexer(std::string filename)
    {
        std::ifstream ifs(filename);
        std::string file = std::string((std::istreambuf_iterator<char>(ifs)), (std::istreambuf_iterator<char>())); // don't know whats happening here or why
        contents.length = file.size();
        contents.data = new char[contents.length];
        file.copy(contents.data, file.size(), 0);

        pushNextToken(); // start off with the first token
    };

    TokenData peek_next()
    {
        return peek(0);
    };

    TokenData peek(uint32_t index)
    {
        while (index >= cache.size())
        {
            pushNextToken();
        }

        return cache[index];
    };

    void next()
    {
        cache.pop_front();

        if (cache.size() == 0)
        {
            pushNextToken();
        }
    }

    void pushNextToken()
    {
        String builder;
        builder.data = contents.data + currentIndex;
        builder.length = 0;

        if (currentIndex >= contents.length)
        {
            TokenData t;
            t.type = TokenType::T_EOF;
            cache.push_back(t);

            return;
        }

        while (std::isspace(contents.data[currentIndex]))
        {
            if (*builder.data == '\n')
                currentLine++;

            builder.data++;
            currentIndex++;

            if (currentIndex >= contents.length)
            {
                TokenData t;
                t.type = TokenType::T_EOF;
                cache.push_back(t);

                return;
            }
        }

        if (std::isalpha(contents.data[currentIndex]) || contents.data[currentIndex] == '_')
        {
            builder.length++;
            currentIndex++;

            while (std::isalnum(contents.data[currentIndex]) || contents.data[currentIndex] == '_')
            {
                builder.length++;
                currentIndex++;
            }

            if (builder.equals("return"))
            {
                TokenData t;
                t.type = TokenType::T_RETURN;
                cache.push_back(t);
            }
            else
            {
                TokenData t;
                t.type = TokenType::T_IDENTIFIER;
                t.identifier = builder;
                cache.push_back(t);
            }
            return;
        }

        if (std::isdigit(contents.data[currentIndex]))
        {
            builder.length++;
            currentIndex++;

            while (std::isdigit(contents.data[currentIndex]) || contents.data[currentIndex] == '.')
            {
                builder.length++;
                currentIndex++;
            }

            TokenData t;
            t.type = TokenType::T_NUMBER;
            static char tokenBuffer[1024] = {}; // dumb way to make strtod work with non null-terminated string
            for (int i = 0; i < builder.length; ++i)
            {
                tokenBuffer[i] = builder.data[i];
            }
            tokenBuffer[builder.length] = '\0';

            t.doubleVal = strtod(tokenBuffer, NULL);
            cache.push_back(t);
            return;
        }

        // if (contents.data[currentIndex] == '#')
        // {
        //     builder.length++;
        //     currentIndex++;

        //     while (std::isalnum(contents.data[currentIndex]))
        //     {
        //         builder.length++;
        //         currentIndex++;
        //     }

        //     TokenData t;
        //     t.type = TokenType::T_COMPILER_DIRECTIVE;
        //     cache.push_back(t);

        //     return;
        // }

        if (contents.data[currentIndex] == '"')
        {
            builder.data++; // swallow '"'
            currentIndex++;

            while (contents.data[currentIndex] != '"') // @TODO(asad): need to account for escaped quotes
            {
                builder.length++;
                currentIndex++;
            }

            currentIndex++; // swallow the quote

            TokenData t;
            t.type = TokenType::T_STRING_LITERAL;
            t.stringVal = builder;
            cache.push_back(t);
            return;
        }

        if (contents.data[currentIndex] == '-' && contents.data[currentIndex + 1] == '>')
        {
            builder.length += 2; // swallow '->'
            currentIndex += 2;

            TokenData t;
            t.type = TokenType::T_ARROW;
            cache.push_back(t);
            return;
        }

        TokenData t;
        t.type = static_cast<TokenType>(contents.data[currentIndex]);
        cache.push_back(t);

        currentIndex++; // swallow the symbol
    };
};
