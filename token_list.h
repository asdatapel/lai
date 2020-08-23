#pragma once

#include <vector>

#include "misc_types.h"

struct TokenList
{
    Segment fileContents;
    std::vector<Token> tokens;
    uint32_t currentIndex;

    Token *peek(uint32_t index)
    {
        return &tokens[currentIndex + index];
    }
    Token *peek_next()
    {
        return peek(0);
    }
    void eat()
    {
        currentIndex++;
    }
    void eat(uint32_t count)
    {
        currentIndex += count;
    }
};