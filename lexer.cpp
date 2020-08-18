#include "lexer.h"

Token createTokenEof()
{
    Token t;
    t.type = TokenType::T_EOF;

    return t;
}

Token createTokenIdentifier(const Segment &file, uint32_t &currentIndex)
{
    Segment builder;
    builder.data = file.data + currentIndex;
    builder.length = 0;

    while (std::isalnum(file.data[currentIndex]) || file.data[currentIndex] == '_')
    {
        builder.length++;
        currentIndex++;
    }

    Token t;
    t.identifier = builder;
    if (builder.equals("return"))
    {
        t.type = TokenType::T_RETURN;
    }
    else
    {
        t.type = TokenType::T_IDENTIFIER;
    }
    return t;
}

Token createTokenNumber(const Segment &file, uint32_t &currentIndex)
{
    if (file.data[currentIndex] == '0' && file.data[currentIndex + 1] == 'x')
    {
        // @TODO hex number
    }
    if (file.data[currentIndex] == '0' && file.data[currentIndex + 1] == 'b')
    {
        // @TODO binary number
    }

    Segment builder;
    builder.data = file.data + currentIndex;
    builder.length = 0;

    bool containsPeriod = false;

    while (std::isdigit(file.data[currentIndex]) || file.data[currentIndex] == '.')
    {
        if (file.data[currentIndex] == '.')
        {
            if (containsPeriod)
            {
                // @VALIDATE error
            }

            containsPeriod = true;
        }

        builder.length++;
        currentIndex++;
    }

    if (containsPeriod)
    {
        Token t;
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
        Token t;
        t.type = TokenType::T_INTEGER_LITERAL;
        t.intVal = std::stoi(builder.toString());
        return t;
    }
}

Token createTokenCompilerDirective(const Segment &file, uint32_t &currentIndex)
{
    currentIndex++; // eat '#'

    Token t = createTokenIdentifier(file, currentIndex);
    t.type = TokenType::T_COMPILER_DIRECTIVE;
    return t;
}

Token createTokenString(const Segment &file, uint32_t &currentIndex)
{
    auto eatPossibleQuote = [&]() {
        if (file.data[currentIndex] == '"')
        {
            currentIndex++; // eat '"'
        }
    };
    auto isEndOfString = [](char c) {
        return (c == '"') || (c == '\n') || (c == '\r') || (c == '\0');
    };

    eatPossibleQuote();

    Segment builder;
    builder.data = file.data + currentIndex;
    builder.length = 0;
    while (!isEndOfString(file.data[currentIndex]))
    {
        builder.length++;
        currentIndex++;
    }

    eatPossibleQuote();

    Token t;
    t.type = TokenType::T_STRING_LITERAL;
    t.stringVal = builder;

    return t;
}

Token Lexer::nextToken()
{
    skipWhitespace();

    if (currentIndex >= file.length)
    {
        return createTokenEof();
    }

    if (std::isdigit(file.data[currentIndex]))
    {
        return createTokenNumber(file, currentIndex);
    }
    if (file.data[currentIndex] == '"')
    {
        return createTokenString(file, currentIndex);
    }
    if (std::isalpha(file.data[currentIndex]) || file.data[currentIndex] == '_')
    {
        return createTokenIdentifier(file, currentIndex);
    }
    if (file.data[currentIndex] == '#')
    {
        return createTokenCompilerDirective(file, currentIndex);
    }

    if (file.data[currentIndex] == '-' && file.data[currentIndex + 1] == '>')
    {
        Segment builder;
        builder.data = file.data + currentIndex;
        builder.length = 0;

        builder.length += 2; // swallow '->'
        currentIndex += 2;

        Token t;
        t.type = TokenType::T_ARROW;
        return t;
    }

    Token t;
    t.type = static_cast<TokenType>(file.data[currentIndex]);
    currentIndex++; // swallow the symbol
    return t;
};

void Lexer::skipWhitespace()
{
    while (std::isspace(file.data[currentIndex]))
    {
        currentColumn++;
        if (file.data[currentIndex] == '\n')
        {
            currentLine++;
            currentColumn = 0;
        }
        currentIndex++;
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
