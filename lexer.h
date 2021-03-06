#pragma once

#include <cctype>
#include <fstream>
#include <string>
#include <vector>

#include "misc_types.h"
#include "token_list.h"

struct LexerMetadata{
    uint32_t currentIndex = 0;
    uint32_t currentLine = 0;
    uint32_t currentColumn = 0;
};

struct Lexer
{
    Segment file;
    std::vector<Token> tokens;

    LexerMetadata meta;

    Lexer(std::string filename);
    Token nextToken();

    void skipWhitespace();

    static TokenList lexFile(std::string filename);
};
