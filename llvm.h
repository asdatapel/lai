// #pragma once

// #include "ast.h"

// #include <llvm/IR/IRBuilder.h>
// #include <llvm/Support/Debug.h>
// #include <llvm/Support/raw_ostream.h>

// static llvm::LLVMContext llvmContext;
// static llvm::IRBuilder<> llvmBuilder(llvmContext);
// static std::unique_ptr<llvm::Module> llvmmodule;
// static std::map<std::string, llvm::Value *> variables;

// llvm::Value *codegenExpression(Ast_Expression *, Ast *);

// llvm::Value *findVariable(Segment name, Ast *ast)
// {
//     // @TODO remove validation (which should be redundant)
//     if (!ast)
//     {
//         return nullptr;
//     }

//     if (ast->declarations.count(name.toString()))
//     {
//         return ast->declarations[name.toString()];
//     }

//     return findVariable(name, ast->parent);
// }

// void codegenModule(Ast *ast)
// {
//     std::string moduleName = "AsadModule";
//     llvmmodule = std::make_unique<llvm::Module>(moduleName, llvmContext);

//     std::vector<llvm::Type *> printParams;
//     printParams.push_back(llvm::Type::getInt8PtrTy(llvmContext));
//     llvm::FunctionType *printFunctionType = llvm::FunctionType::get(llvm::IntegerType::getInt32Ty(llvmContext), printParams, true);
//     auto printFunction = llvm::Function::Create(printFunctionType, llvm::Function::ExternalLinkage, "printf", llvmmodule.get());
//     auto printGlobal = new llvm::GlobalVariable(*llvmmodule.get(), printFunction->getType(), false, llvm::GlobalVariable::ExternalLinkage, printFunction, "printffunc");
//     ast->declarations["printffunc"] = printGlobal;

//     llvm::FunctionType *llvmFunctionType = llvm::FunctionType::get(llvm::Type::getVoidTy(llvmContext), {}, false);
//     auto llvmFunction = llvm::Function::Create(llvmFunctionType, llvm::Function::ExternalLinkage, /* "__module_" + moduleName */ "main", llvmmodule.get());
//     llvm::BasicBlock *entryBlock = llvm::BasicBlock::Create(llvmContext, "entry", llvmFunction);
//     llvmBuilder.SetInsertPoint(entryBlock);

//     for (auto statement : ast->statements)
//     {
//         switch (statement->type)
//         {
//         case Ast_Statement::Type::DECLARATION:
//         {
//             auto st = (Ast_DeclarationStatement *)statement;

//             auto x = codegenExpression(st->value, ast);
//             if (x)
//             {
//                 auto global = new llvm::GlobalVariable(*llvmmodule.get(), x->getType(), false, llvm::GlobalVariable::ExternalLinkage, nullptr, st->identifiers[0]->identifier.toString());
//                 if (auto init = llvm::dyn_cast<llvm::Constant>(x))
//                 {
//                     global->setInitializer(init);
//                 }
//                 else
//                 {
//                     global->setInitializer(llvm::Constant::getNullValue(x->getType()));
//                     llvmBuilder.CreateStore(x, global, false);
//                 }

//                 ast->declarations[st->identifiers[0]->identifier.toString()] = global;
//             }
//             else
//             {
//                 auto global = new llvm::GlobalVariable(*llvmmodule.get(), llvm::Type::getDoubleTy(llvmContext), false, llvm::GlobalVariable::ExternalLinkage, llvm::ConstantFP::get(llvmContext, llvm::APFloat(0.0)), st->identifiers[0]->identifier.toString());
//                 ast->declarations[st->identifiers[0]->identifier.toString()] = global;
//             }
//         }
//         break;
//         case Ast_Statement::Type::RETURN:
//         {
//             // @VALIDATE not allowed, no can dooo
//             // this validation should be redundant, it should have happened before the codegen stage
//         }
//         break;
//         case Ast_Statement::Type::EXPRESSION:
//         {
//             auto st = (Ast_ExpressionStatement *)statement;
//             auto x = codegenExpression(st->value, ast);
//         }
//         break;
//         };
//     }
//     llvmBuilder.CreateRet(nullptr);

//     std::string Str;
//     llvm::raw_string_ostream OS(Str);
//     OS << *llvmmodule.get();
//     OS.flush();
//     std::cout << Str << std::endl;
// };

// void codegenStatement(Ast_Statement *statement, Ast *ast)
// {
//     switch (statement->type)
//     {
//     case Ast_Statement::Type::DECLARATION:
//     {
//         auto st = (Ast_DeclarationStatement *)statement;

//         auto x = codegenExpression(st->value, ast);
//         if (x)
//         {
//             auto llvmAlloca = llvmBuilder.CreateAlloca(x->getType(), nullptr, st->identifiers[0]->identifier.toString());
//             llvmBuilder.CreateStore(x, llvmAlloca, false);

//             ast->declarations[st->identifiers[0]->identifier.toString()] = llvmAlloca;
//         }
//         else
//         {
//             // add global, determine type with st->explicitType
//         }
//     }
//     break;
//     case Ast_Statement::Type::RETURN:
//     {
//         auto st = (Ast_ReturnStatement *)statement;
//         auto x = codegenExpression(st->value, ast);
//         llvmBuilder.CreateRet(x);
//     }
//     break;
//     case Ast_Statement::Type::EXPRESSION:
//     {
//         auto st = (Ast_ExpressionStatement *)statement;
//         auto x = codegenExpression(st->value, ast);
//     }
//     break;
//     };
// };

// llvm::Value *codegenExpression(Ast_Expression *expression, Ast *ast)
// {
//     if (!expression)
//     {
//         return nullptr;
//     }

//     switch (expression->type)
//     {
//     case Ast_Expression::Type::NUMBER_EXPRESSION:
//     {
//         auto exp = (Ast_NumberExpression *)expression;
//         return llvm::ConstantFP::get(llvmContext, llvm::APFloat(exp->number));
//     }
//     break;
//     case Ast_Expression::Type::STRING_LITERAL:
//     {
//         auto exp = (Ast_LiteralExpression *)expression;
//         std::string s = exp->value.toUnescapedString();

//         return llvmBuilder.CreateGlobalStringPtr(llvm::StringRef(s));
//     }
//     break;
//     case Ast_Expression::Type::VARIABLE:
//     {
//         auto exp = (Ast_VariableExpression *)expression;
//         auto val = findVariable(exp->identifier, ast);
//         if (auto pointerType = llvm::dyn_cast<llvm::PointerType>(val->getType()))
//         {
//             return llvmBuilder.CreateLoad(val);
//         }
//         return val;
//     }
//     break;
//     case Ast_Expression::Type::UNARY_OPERATION:
//     {
//     }
//     break;
//     case Ast_Expression::Type::BINARY_OPERATION:
//     {
//         // temprorary assumption that no binary operator will change type ???????
//         auto exp = (Ast_BinaryOperatorExpression *)expression;

//         auto *lhs = codegenExpression(exp->leftOperand, ast);
//         auto *rhs = codegenExpression(exp->rightOperand, ast);
//         if (!lhs || !rhs)
//         {
//             return nullptr;
//         }

//         switch (exp->operatorSymbol)
//         {
//         case '+':
//         {
//             return llvmBuilder.CreateFAdd(lhs, rhs, "addtmp");
//         }
//         break;
//         case '-':
//         {
//             return llvmBuilder.CreateFSub(lhs, rhs, "subtmp");
//         }
//         break;
//         }
//     }
//     break;
//     case Ast_Expression::Type::ASSIGNMENT:
//     {
//         auto exp = (Ast_AssignmentExpression *)expression;

//         auto *lhs = codegenExpression(exp->lhs, ast);
//         auto *rhs = codegenExpression(exp->rhs, ast);
//         // @TODO do the assignment!
//     }
//     break;
//     case Ast_Expression::Type::FUNCTION_HEADER:
//     {
//         // maybe we dont generate code here, since function headers only appear as a type?
//     }
//     break;
//     case Ast_Expression::Type::FUNCTION_DEFINITION:
//     {
//         static unsigned int counter = 0;
//         counter += 1;

//         auto exp = (Ast_FunctionDefinitionExpression *)expression;

//         auto parentBlock = llvmBuilder.GetInsertBlock();
//         std::vector<llvm::Type *> llvmParams(exp->header->parameters.size(), llvm::Type::getDoubleTy(llvmContext));
//         llvm::FunctionType *llvmFunctionType = llvm::FunctionType::get(llvm::Type::getDoubleTy(llvmContext), llvmParams, false);
//         auto llvmFunction = llvm::Function::Create(llvmFunctionType, llvm::Function::ExternalLinkage, "__function_" + std::to_string(counter), llvmmodule.get());

//         for (int i = 0; i < llvmFunction->arg_size(); i++)
//         {
//             auto paramName = ((Ast_DeclarationStatement *)exp->header->parameters[i])->identifiers[0]->identifier.toString();
//             exp->body->declarations[paramName] = llvmFunction->getArg(i);
//         }

//         llvm::BasicBlock *entryBlock = llvm::BasicBlock::Create(llvmContext, "entry", llvmFunction);
//         llvmBuilder.SetInsertPoint(entryBlock);

//         for (auto st : exp->body->statements)
//         {
//             codegenStatement(st, exp->body);
//         }

//         llvmBuilder.SetInsertPoint(parentBlock);
//         return llvmFunction;
//     }
//     break;
//     case Ast_Expression::Type::FUNCTION_CALL:
//     {
//         auto exp = (Ast_FunctionCallExpression *)expression;

//         auto functionPointer = codegenExpression(exp->function, ast);

//         std::vector<llvm::Value *> args;
//         for (auto a : exp->arguments)
//         {
//             args.push_back(codegenExpression(a, ast));
//         }

//         // auto x = llvmBuilder.CreateLoad(functionPointer);
//         return llvmBuilder.CreateCall(functionPointer, args);
//     }
//     break;
//     }
// }