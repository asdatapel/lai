#include "lexer.h"

Token createTokenEof(LexerMetadata &meta)
{
    Token t;
    t.type = TokenType::T_EOF;
    t.start_line = meta.currentLine;
    t.start_column = meta.currentColumn;

    return t;
}

Token createTokenKeywordOrIdentifier(const Segment &file, LexerMetadata &meta)
{
    Token t;
    t.start_line = meta.currentLine;
    t.start_column = meta.currentColumn;

    Segment builder;
    builder.data = file.data + meta.currentIndex;
    builder.length = 0;

    while (std::isalnum(file.data[meta.currentIndex]) || file.data[meta.currentIndex] == '_')
    {
        builder.length++;
        meta.currentIndex++;
    }

    meta.currentColumn += builder.length;

    t.identifier = builder;
    if (builder.equals("return"))
    {
        t.type = TokenType::T_RETURN;
    }
    else if (builder.equals("if"))
    {
        t.type = TokenType::T_IF;
    }
    else if (builder.equals("else"))
    {
        t.type = TokenType::T_ELSE;
    }
    else if (builder.equals("true"))
    {
        t.type = TokenType::T_TRUE;
    }
    else if (builder.equals("false"))
    {
        t.type = TokenType::T_FALSE;
    }
    else
    {
        t.type = TokenType::T_IDENTIFIER;
    }

    return t;
}

Token createTokenNumber(const Segment &file, LexerMetadata &meta)
{
    Token t;
    t.start_line = meta.currentLine;
    t.start_column = meta.currentColumn;

    if (file.data[meta.currentIndex] == '0' && file.data[meta.currentIndex + 1] == 'x')
    {
        // @TODO hex number
    }
    if (file.data[meta.currentIndex] == '0' && file.data[meta.currentIndex + 1] == 'b')
    {
        // @TODO binary number
    }

    Segment builder;
    builder.data = file.data + meta.currentIndex;
    builder.length = 0;

    bool containsPeriod = false;

    while (std::isdigit(file.data[meta.currentIndex]) || file.data[meta.currentIndex] == '.')
    {
        if (file.data[meta.currentIndex] == '.')
        {
            if (containsPeriod)
            {
                // @VALIDATE error
            }

            containsPeriod = true;
        }

        builder.length++;
        meta.currentIndex++;
    }

    meta.currentColumn += builder.length;

    if (containsPeriod)
    {
        t.type = TokenType::T_FLOAT_LITERAL;

        ///////////////////////////////////////////////////////////////
        // dumb way to make strtod work with non null-terminated string
        ///////////////////////////////////////////////////////////////
        static char tokenBuffer[1024] = {};
        for (int i = 0; i < builder.length; ++i)
        {
            tokenBuffer[i] = builder.data[i];
        }
        tokenBuffer[builder.length] = '\0';
        t.doubleVal = strtod(tokenBuffer, NULL);

        return t;
    }
    else
    {
        t.type = TokenType::T_INTEGER_LITERAL;
        t.intVal = std::stoi(builder.toString());
        return t;
    }
}

Token createTokenCompilerDirective(const Segment &file, LexerMetadata &meta)
{
    meta.currentIndex++; // eat '#'
    meta.currentColumn++;

    Token t = createTokenKeywordOrIdentifier(file, meta);
    t.type = TokenType::T_COMPILER_DIRECTIVE;
    t.start_column--; // account for the '#'

    return t;
}

Token createTokenString(const Segment &file, LexerMetadata &meta)
{
    Token t;
    t.type = TokenType::T_STRING_LITERAL;
    t.start_line = meta.currentLine;
    t.start_column = meta.currentColumn;

    auto eatPossibleQuote = [&]() {
        if (file.data[meta.currentIndex] == '"')
        {
            meta.currentIndex++; // eat '"'
            meta.currentColumn++;
        }
    };
    auto isEndOfString = [](char c) {
        return (c == '"') || (c == '\n') || (c == '\r') || (c == '\0');
    };

    eatPossibleQuote();

    Segment builder;
    builder.data = file.data + meta.currentIndex;
    builder.length = 0;
    while (!isEndOfString(file.data[meta.currentIndex]))
    {
        builder.length++;
        meta.currentIndex++;
        meta.currentColumn++;
    }

    eatPossibleQuote();

    t.stringVal = builder;

    return t;
}

Token Lexer::nextToken()
{
    skipWhitespace();

    if (meta.currentIndex >= file.length)
    {
        return createTokenEof(meta);
    }

    if (std::isdigit(file.data[meta.currentIndex]))
    {
        return createTokenNumber(file, meta);
    }
    if (file.data[meta.currentIndex] == '"')
    {
        return createTokenString(file, meta);
    }
    if (file.data[meta.currentIndex] == '#')
    {
        return createTokenCompilerDirective(file, meta);
    }
    if (std::isalpha(file.data[meta.currentIndex]) || file.data[meta.currentIndex] == '_')
    {
        return createTokenKeywordOrIdentifier(file, meta);
    }

    if (file.data[meta.currentIndex] == '-' && file.data[meta.currentIndex + 1] == '>')
    {
        meta.currentIndex += 2; // swallow '->'
        meta.currentColumn += 2;

        Token t;
        t.type = TokenType::T_ARROW;
        return t;
    }

    if (file.data[meta.currentIndex] == '=' && file.data[meta.currentIndex + 1] == '=')
    {
        meta.currentIndex += 2; // swallow '=='
        meta.currentColumn += 2;

        Token t;
        t.type = TokenType::T_DOUBLE_EQUAL;
        return t;
    }
    if (file.data[meta.currentIndex] == '!' && file.data[meta.currentIndex + 1] == '=')
    {
        meta.currentIndex += 2; // swallow '!='
        meta.currentColumn += 2;

        Token t;
        t.type = TokenType::T_NOT_EQUAL;
        return t;
    }
    if (file.data[meta.currentIndex] == '>' && file.data[meta.currentIndex + 1] == '=')
    {
        meta.currentIndex += 2; // swallow '>='
        meta.currentColumn += 2;

        Token t;
        t.type = TokenType::T_GREATER_THAN_EQUAL;
        return t;
    }
    if (file.data[meta.currentIndex] == '<' && file.data[meta.currentIndex + 1] == '=')
    {
        meta.currentIndex += 2; // swallow '<='
        meta.currentColumn += 2;

        Token t;
        t.type = TokenType::T_LESS_THAN_EQUAL;
        return t;
    }

    Token t;
    t.type = static_cast<TokenType>(file.data[meta.currentIndex]);
    meta.currentIndex++; // swallow the symbol
    meta.currentColumn++;
    return t;
};

void Lexer::skipWhitespace()
{
    while (std::isspace(file.data[meta.currentIndex]))
    {
        meta.currentColumn++;
        if (file.data[meta.currentIndex] == '\n')
        {
            meta.currentLine++;
            meta.currentColumn = 0;
        }
        meta.currentIndex++;
    }
};

Lexer::Lexer(std::string filename)
{
    std::ifstream ifs(filename);
    std::string fileData = std::string((std::istreambuf_iterator<char>(ifs)), (std::istreambuf_iterator<char>())); // don't know whats happening here or why
    file.length = fileData.size();
    file.data = new char[file.length + 1];
    file.data[file.length] = '\0';
    fileData.copy(file.data, file.length, 0);

    Token t;
    do
    {
        t = nextToken();
        tokens.push_back(t);
    } while (t.type != TokenType::T_EOF);
}

TokenList Lexer::lexFile(std::string filename)
{
    Lexer l(filename);
    return {l.file, l.tokens};
}
