#pragma once

#include <string>

struct Segment
{
    char *data = nullptr;
    uint32_t length = 0;

    bool equals(Segment &other)
    {
        if (length != other.length){
            return false;
        }
        
        for (int i = 0; i < length; ++i)
        {
            if (data[i] != other.data[i])
                return false;
        }

        return true;
    };

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

    std::string toUnescapedString()
    {
        std::string out = "";
        for (int i = 0; i < length; ++i)
        {
            if (data[i] == '\\')
            {
                out += unescaped(data[i + 1]);
                i++;
            }
            else
            {
                out += data[i];
            }
        }
        return out;
    };
    char unescaped(char c)
    {
        switch (c)
        {
        case 'a':
            return '\a';
            break;
        case 'b':
            return '\b';
            break;
        case 'f':
            return '\f';
            break;
        case 'n':
            return '\n';
            break;
        case 'r':
            return '\r';
            break;
        case 't':
            return '\t';
            break;
        case 'v':
            return '\v';
            break;
        case '\\':
            return '\\';
            break;
        case '\'':
            return '\'';
            break;
        case '\"':
            return '\"';
            break;
        case '?':
            return '\?';
            break;
        default:
            // @VALIDATE error
            return ' ';
        }
    }
};

enum struct TokenType
{
    T_UNKNOWN = 256,

    T_IDENTIFIER,
    T_INTEGER_LITERAL,
    T_FLOAT_LITERAL,
    T_STRING_LITERAL,

    T_COMPILER_DIRECTIVE,

    T_ARROW,

    T_RETURN,

    T_EOF,
};

struct Token
{
    TokenType type = TokenType::T_UNKNOWN;

    Segment identifier;
    Segment stringVal;
    long long intVal;
    double doubleVal;

    uint32_t start_line = 0;
    uint32_t start_column = 0;
};