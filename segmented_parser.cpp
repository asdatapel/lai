#include <string>
#include <vector>

#include "lexer.h"

struct SegmentLexer
{
    Lexer tokenLexer;
    struct
    {
        std::vector<Lexer::TokenData> tokens = {};
        Lexer::TokenData nextToken;

        bool containsComma = false;
        bool containsColon = false;
    } segment;

    SegmentLexer(std::string filename) : tokenLexer(filename)
    {
        segment.nextToken = tokenLexer.token;
    };

    void next()
    {
        // reset segment
        segment.tokens.clear();
        segment.containsComma = false;
        segment.containsColon = false;

        while (addSegment())
        {
            // nothing ...
        }
    };

    bool addSegment()
    {
        segment.tokens.push_back(tokenLexer.token);

        if (tokenLexer.token.type == Lexer::TokenType::T_EOF || tokenLexer.isSymbol(';'))
        {
            tokenLexer.next();
            segment.nextToken = tokenLexer.token;

            return false;
        }

        tokenLexer.next();
        segment.nextToken = tokenLexer.token;
         
        return true;
    };
};

/////////////////////////////////////////////////////////
int main(int argc, char *argv[])
{
    if (argc <= 1)
    {
        std::cout << "missing argument: build file" << std::endl;
    }

    char *filename = argv[1];

    SegmentLexer lexer(filename);
    while (lexer.segment.nextToken.type != Lexer::TokenType::T_EOF)
    {
        std::string s;
        for (auto t : lexer.segment.tokens)
        {
            if (t.type == Lexer::TokenType::T_SYMBOL)
            {
                s += "{ " + std::to_string(static_cast<int>(t.type)) + ", " + t.symbol + " }";
            }
            else
            {
                s += "{ " + std::to_string(static_cast<int>(t.type)) + ", " + t.identifier + " }";
            }
        }
        std::cout << s << std::endl;

        lexer.next();
    }
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

    return 0;
}
