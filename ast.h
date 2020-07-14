#include <string>

struct Lexer;

struct Ast;
struct Ast_DeclarationStatement;

struct Ast_Type : Ast_Expression
{
};

struct Ast_FunctionType : Ast_Type
{
    std::vector<Ast_DeclarationStatement *> parameters = {};
    std::string returnType;

    static Ast_FunctionType *parse(Lexer *lexer);
    static Ast_FunctionType *parse(Lexer *lexer, std::string firstIdentifier); // overload for when we have already passed the first identifier and are on the first ':'

    std::string print() override;
};

struct Ast_Expression
{
    static Ast_Expression *parse(Lexer *lexer);

    virtual std::string print() { return ""; };
};
struct Ast_ParensExpression : Ast_Expression
{
    static Ast_Expression *parse(Lexer *lexer);
};
struct Ast_FunctionDefinitionExpression : Ast_Expression
{
    Ast_FunctionType *type;
    Ast *body = NULL;

    static Ast_FunctionDefinitionExpression *parse(Lexer *lexer, std::string firstIdentifier); // overload for when we have already passed the first identifier and are on the first ':'

    std::string print() override;
};
struct Ast_NumberExpression : Ast_Expression
{
    double number = 0.0;

    static Ast_NumberExpression *parse(Lexer *lexer);

    std::string print() override { return " Ast_NumberExpression { " + std::to_string(number) + " }"; };
};
struct Ast_VarReferenceExpression : Ast_Expression
{
    std::string name = "";

    // @unused
    // static Ast_NumberExpression *parse(Lexer *lexer);

    std::string print() override { return " Ast_VarReferenceExpression : { " + name + " }"; };
};
struct Ast_BinaryPlusExpression : Ast_Expression
{
    Ast_Expression *lhs = NULL, *rhs = NULL;

    // @unused
    // static Ast_NumberExpression *parse(Lexer *lexer);

    std::string print() override { return " Ast_BinaryPlusExpression : { " + lhs->print() + ", " + rhs->print() + " }"; };
};

struct Ast_Statement
{
    static Ast_Statement *parse(Lexer *lexer);

    virtual std::string print() { return ""; };
};

struct Ast_DeclarationStatement : Ast_Statement
{
    std::string identifier = "";
    std::string type = ""; // blank means the type should be implicit
    Ast_Expression *value = NULL;
    bool constant = false;

    static Ast_DeclarationStatement *parse(Lexer *lexer);
    static Ast_DeclarationStatement *parse(Lexer *lexer, std::string identifier); // overload for when we have already passed the identifier and are on the ':'
    std::string print() override { return " Ast_DeclarationStatement : { " + identifier + ", " + type + ", " + (value ? value->print() : "uninitialized") + ", " + std::to_string(constant) + " }"; };
};

struct Ast_AssignmentStatement : public Ast_Statement
{
    std::string identifier = "";
    Ast_Expression *value = NULL;

    static Ast_AssignmentStatement *parse(Lexer *lexer, std::string identifier);
    std::string print() override { return " Ast_AssignmentStatement : { " + identifier + ", " + value->print() + " }"; };
};

struct Ast_ReturnStatement : public Ast_Statement
{
    Ast_Expression *value = NULL;

    static Ast_ReturnStatement *parse(Lexer *lexer);
    std::string print() override { return " Ast_ReturnStatement : { " + value->print() + " }"; };
};

struct Ast // represents a scope??
{
    std::vector<Ast_Statement *> statements = {};

    static Ast *parse(Lexer *lexer);
    std::string print()
    {
        std::string ret;
        for (auto s : statements)
        {
            ret += s->print() + '\n';
        }

        return ret;
    };
};
