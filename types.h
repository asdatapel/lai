// #pragma once

// #include <vector>

// #include "ast.h"

// enum struct Type
// {
//     UNKNOWN, // ERROR?
//     FLOAT_NUMBER,
//     INTEGER,
//     FUNCTION,
//     FUNCTION_POINTER,
//     POINTER,
// };

// struct Type_Base
// {
//     Type type;
// };
// struct Type_Unknown : Type_Base
// {
//     Type_Unknown() { type = Type::UNKNOWN; };
// };
// struct Type_FloatingPointNumber : Type_Base
// {
//     Type_FloatingPointNumber() { type = Type::FLOAT_NUMBER; };

//     uint8_t size; // 32 or 64 only
// };
// struct Type_Integer : Type_Base
// {
//     Type_Integer() { type = Type::INTEGER; };
//     Type_Integer(uint8_t size, bool isSigned)
//     {
//         type = Type::INTEGER;
//         this->size = size;
//         this->isSigned = isSigned;
//     };

//     uint8_t size; // 8, 16, 32 or 64 only
//     bool isSigned;
// };
// struct Type_Function : Type_Base
// {
//     Type_Function() { type = Type::FUNCTION; };

//     Type_Base *returnType = nullptr;
//     std::vector<Type_Base *> parameters = {};
// };
// struct Type_FunctionPointer : Type_Base
// {
//     Type_FunctionPointer() { type = Type::FUNCTION_POINTER; };

//     Type_Function *functionType = nullptr;
// };
// struct Type_Pointer : Type_Base
// {
//     Type_Pointer() { type = Type::POINTER; };

//     Type_Base *pointeeType = nullptr;
// };

// ////////////////// Builtins //////////
// Type_Integer builtinTypeChar(8, false);
// Type_Integer builtinTypeI32(32, false);
// Type_Integer builtinTypeS32(32, true);
// //////////////////////////////////////

// void resolveTypeForStatement(Ast_Statement *);
// void resolveTypeForExpression(Ast_Expression *, Ast *);
// Type_Base *resolveExplicitType(Ast_Expression *);

// void analyze(Ast *ast)
// {
//     for (auto statement : ast->statements)
//     {
//         switch (statement->type)
//         {
//         case Ast_Statement::Type::DECLARATION:
//         {
//             auto st = (Ast_DeclarationStatement *)statement;
//             if (!st->explicitType && !st->value)
//             {
//                 // @VALIDATE: at least one should be set
//             }

//             resolveTypeForExpression(st->value, ast);
//             Type_Base *variableType = resolveExplicitType(st->explicitType);

//             if (st->explicitType && st->value)
//             {
//                 // @VALIDATE : types should be compatible
//             }
//             else if (!st->explicitType)
//             {
//                 // assign type of value
//                 variableType = st->value->expressionType;
//             }

//             // auto declaration = new Ast::Ast_Declaration;
//             // declaration->name = st->identifiers[0]->identifier;
//             // declaration->type = variableType;
//             // ast->declarations[declaration->name.toString()] = declaration;
//         }
//         break;
//         case Ast_Statement::Type::RETURN:
//         {
//         }
//         break;
//         case Ast_Statement::Type::EXPRESSION:
//         {
//         }
//         break;
//         };
//     }
// };

// void resolveTypeForExpression(Ast_Expression *expression, Ast *scope)
// {
//     if (!expression)
//     {
//         return;
//     }

//     switch (expression->type)
//     {
//     case Ast_Expression::Type::NUMBER_EXPRESSION:
//     {
//         auto exp = (Ast_NumberExpression *)expression;
//         auto t = new Type_FloatingPointNumber;
//         t->size = 64;

//         exp->expressionType = t;
//     }
//     break;
//     case Ast_Expression::Type::LITERAL_EXPRESSION:
//     {
//         auto exp = (Ast_LiteralExpression *)expression;
//         auto t = new Type_Pointer;
//         t->pointeeType = &builtinTypeChar;

//         exp->expressionType = t;
//     }
//     break;
//     case Ast_Expression::Type::VARIABLE:
//     {
//         // lookup name
//         auto exp = (Ast_VariableExpression *)expression;
//         if (scope->declarations.count(exp->identifier.toString()) == 0)
//         {
//             return;
//         }

//         // exp->expressionType = scope->declarations[exp->identifier.toString()]->type;
//     }
//     break;
//     case Ast_Expression::Type::UNARY_OPERATION:
//     {
//         // assuming no unary operators will change type ???????
//         auto exp = (Ast_UnaryOperatorExpression *)expression;
//         resolveTypeForExpression(exp->operand, scope);
//         expression->expressionType = exp->operand->expressionType;
//     }
//     break;
//     case Ast_Expression::Type::BINARY_OPERATION:
//     {
//         // temprorary assumption that no binary operator will change type ???????
//         auto exp = (Ast_BinaryOperatorExpression *)expression;
//         resolveTypeForExpression(exp->leftOperand, scope);
//         resolveTypeForExpression(exp->rightOperand, scope);
//         expression->expressionType = exp->leftOperand->expressionType;
//     }
//     break;
//     case Ast_Expression::Type::ASSIGNMENT:
//     {
//         auto exp = (Ast_AssignmentExpression *)expression;
//         resolveTypeForExpression(exp->lhs, scope);
//         resolveTypeForExpression(exp->rhs, scope);
//         expression->expressionType = exp->lhs->expressionType;
//     }
//     break;
//     case Ast_Expression::Type::FUNCTION_HEADER:
//     {
//     }
//     break;
//     case Ast_Expression::Type::FUNCTION_DEFINITION:
//     {
//         auto exp = (Ast_FunctionDefinitionExpression *)expression;
//     }
//     break;
//     }
// }

// Type_Base *resolveExplicitType(Ast_Expression *expression)
// {
//     if (!expression)
//     {
//         return nullptr;
//     }

//     if (expression->type == Ast_Expression::Type::VARIABLE)
//     {
//         auto exp = (Ast_VariableExpression *)expression;

//         if (exp->identifier.equals("i32"))
//         {
//             return &builtinTypeI32;
//         }
//         if (exp->identifier.equals("s32"))
//         {
//             return &builtinTypeS32;
//         }
//     }
//     return nullptr;
// }