#pragma once

#include <iostream>

#include <llvm/IR/IRBuilder.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/raw_ostream.h>

#include "debug_util.h"
#include "ir.h"

void codegenModule(IrContainer *);
void codegenValue(IrInstr *, IrContainer *, llvm::BasicBlock *, llvm::Module &);
llvm::Value *codegenCast(IrCast *instr, IrContainer *, llvm::BasicBlock *, llvm::Module &);
llvm::Function *codegenFunction(IrFunction *, llvm::Module &);
llvm::Type *toLlvmVarType(LaiType *, llvm::LLVMContext &);
llvm::FunctionType *toLlvmFunctionType(LaiType *, llvm::LLVMContext &);

void codegenModule(IrContainer *irContainer)
{
    std::string moduleName = "AsadModule";

    llvm::LLVMContext llvmContext;
    llvm::Module module(moduleName, llvmContext);

    ////temp main function////
    //////////////////////////
    llvm::FunctionType *mainType = llvm::FunctionType::get(llvm::Type::getInt32Ty(llvmContext), {}, false);
    llvm::Function *mainFunction = module.getFunction("main");
    mainFunction = llvm::Function::Create(
        mainType,
        llvm::GlobalValue::ExternalLinkage,
        "main",
        module);
    llvm::BasicBlock *entryBlock = llvm::BasicBlock::Create(llvmContext, "entry", mainFunction, 0);
    //////////////////////////

    for (auto declaration : irContainer->declarations)
    {
        auto global = (IrGlobalDeclaration *)declaration;
        auto llvmType = toLlvmVarType(resolveDereferenceType(global->laiType), llvmContext);
        auto llvmGlobal = new llvm::GlobalVariable(module, llvmType, false, llvm::GlobalVariable::ExternalLinkage, nullptr);
        llvmGlobal->setInitializer(llvm::Constant::getNullValue(llvmType));

        if (global->initializer)
        {
            codegenValue(global->initializer, irContainer, entryBlock, module);
            if (auto init = llvm::dyn_cast<llvm::Constant>(global->initializer->llvmValue))
            {
                llvmGlobal->setInitializer(init);
            }
            else
            {
                auto x = llvmGlobal->getType();
                new llvm::StoreInst(global->initializer->llvmValue, llvmGlobal, false, entryBlock);
            }
        }

        global->llvmValue = llvmGlobal;
    }

    for (auto instr : irContainer->body)
    {
        codegenValue(instr, irContainer, entryBlock, module);
    }

    ////temp main function////
    //////////////////////////
    auto lastDec = irContainer->declarations[irContainer->declarations.size() - 1]->llvmValue;
    auto load = new llvm::LoadInst(lastDec, "", false, entryBlock);
    llvm::ReturnInst::Create(module.getContext(), load, entryBlock);
    //////////////////////////

    // dump llvm ir to file
    std::string Str;
    llvm::raw_string_ostream OS(Str);
    OS << module;
    OS.flush();
    write_to_file("foo.ll", Str);
};

void codegenValue(IrInstr *instr, IrContainer *irContainer, llvm::BasicBlock *block, llvm::Module &module)
{
    if (instr->llvmValue)
    {
        return; // value has already been codegenned
    }

    switch (instr->type)
    {
    case IrInstr::Type::INTEGER_LITERAL:
    {
        auto irInteger = (IrIntegerLiteral *)instr;
        irInteger->llvmValue = llvm::ConstantInt::get(module.getContext(), llvm::APInt(32, irInteger->value, true));
    }
    break;
    case IrInstr::Type::FLOAT_LITERAL:
    {
        auto irFloat = (IrFloatLiteral *)instr;
        irFloat->llvmValue = llvm::ConstantFP::get(toLlvmVarType(irFloat->laiType, module.getContext()), irFloat->value);
    }
    break;
    case IrInstr::Type::FUNCTION:
    {
        auto irFunction = (IrFunction *)instr;
        instr->llvmValue = codegenFunction(irFunction, module);
    }
    break;
    case IrInstr::Type::FUNCTION_CALL:
    {
        auto irFunctionCall = (IrFunctionCall *)instr;

        std::vector<llvm::Value *> args;
        for (auto a : irFunctionCall->arguments)
        {
            codegenValue(a, irContainer, block, module);
            args.push_back(a->llvmValue);
        }

        codegenValue(irFunctionCall->function, irContainer, block, module);
        instr->llvmValue = llvm::CallInst::Create(irFunctionCall->function->llvmValue, args, "", block);
    }
    break;
    case IrInstr::Type::ADD:
    {
        auto irAdd = (IrAdd *)instr;
        codegenValue(irAdd->lhs, irContainer, block, module);
        codegenValue(irAdd->rhs, irContainer, block, module);

        if (irAdd->laiType->laiTypeType == LaiTypeType::INTEGER)
        {
            instr->llvmValue = llvm::BinaryOperator::Create(llvm::Instruction::Add, irAdd->lhs->llvmValue, irAdd->rhs->llvmValue, "add", block);
        }
        else if (irAdd->laiType->laiTypeType == LaiTypeType::FLOAT)
        {
            instr->llvmValue = llvm::BinaryOperator::Create(llvm::Instruction::FAdd, irAdd->lhs->llvmValue, irAdd->rhs->llvmValue, "add", block);
        }
    }
    break;
    case IrInstr::Type::SUB:
    {
        auto irSub = (IrSub *)instr;
        codegenValue(irSub->lhs, irContainer, block, module);
        codegenValue(irSub->rhs, irContainer, block, module);
        instr->llvmValue = llvm::BinaryOperator::Create(llvm::Instruction::Sub, irSub->lhs->llvmValue, irSub->rhs->llvmValue, "sub", block);

        if (irSub->laiType->laiTypeType == LaiTypeType::INTEGER)
        {
            instr->llvmValue = llvm::BinaryOperator::Create(llvm::Instruction::Sub, irSub->lhs->llvmValue, irSub->rhs->llvmValue, "sub", block);
        }
        else if (irSub->laiType->laiTypeType == LaiTypeType::FLOAT)
        {
            instr->llvmValue = llvm::BinaryOperator::Create(llvm::Instruction::FSub, irSub->lhs->llvmValue, irSub->rhs->llvmValue, "sub", block);
        }
    }
    break;
    case IrInstr::Type::LOAD:
    {
        auto irLoad = (IrLoad *)instr;
        codegenValue(irLoad->value, irContainer, block, module);
        instr->llvmValue = new llvm::LoadInst(irLoad->value->llvmValue, "", false, block);
    }
    break;
    case IrInstr::Type::STORE:
    {
        auto irStore = (IrStore *)instr;
        codegenValue(irStore->target, irContainer, block, module);
        codegenValue(irStore->value, irContainer, block, module);
        instr->llvmValue = new llvm::StoreInst(irStore->value->llvmValue, irStore->target->llvmValue, false, block);
    }
    break;
    case IrInstr::Type::CAST:
    {
        auto irCast = (IrCast *)instr;
        irCast->llvmValue = codegenCast(irCast, irContainer, block, module);
    }
    break;
    case IrInstr::Type::RETURN:
    {
        auto irReturn = (IrReturn *)instr;
        codegenValue(irReturn->value, irContainer, block, module);
        instr->llvmValue = llvm::ReturnInst::Create(module.getContext(), irReturn->value->llvmValue, block);
    }
    break;
    default:
        // @VALIDATE error
        break;
    };
};

llvm::Function *codegenFunction(IrFunction *irFunction, llvm::Module &module)
{
    ////////////
    //temp function naming
    static int i = 0;
    i++;
    std::string name = "_func_" + std::to_string(i);
    ////////////

    llvm::FunctionType *llvmFuntionType = toLlvmFunctionType(irFunction->laiType, module.getContext());
    llvm::Function *llvmFunction = module.getFunction(name);
    llvmFunction = llvm::Function::Create(
        llvmFuntionType,
        llvm::GlobalValue::ExternalLinkage,
        name,
        module);
    llvm::BasicBlock *entryBlock = llvm::BasicBlock::Create(module.getContext(), "entry", llvmFunction, 0);

    for (int i = 0; i < irFunction->parameters.size(); i++)
    {
        auto parameter = irFunction->parameters[i];
        auto llvmType = toLlvmVarType(resolveDereferenceType(parameter->laiType), module.getContext());
        parameter->llvmValue = new llvm::AllocaInst(llvmType, 0, "", entryBlock);
        new llvm::StoreInst(llvmFunction->getArg(i), parameter->llvmValue, false, entryBlock);
    }

    auto irContainer = irFunction->container;
    for (auto declaration : irContainer->declarations)
    {
        if (!declaration->llvmValue)
        {
            auto llvmType = toLlvmVarType(resolveDereferenceType(declaration->laiType), module.getContext());
            declaration->llvmValue = new llvm::AllocaInst(llvmType, 0, "", entryBlock);
        }
    }

    for (auto instr : irContainer->body)
    {
        codegenValue(instr, irContainer, entryBlock, module);
    }

    return llvmFunction;
};

llvm::Value *codegenCast(IrCast *instr, IrContainer *irContainer, llvm::BasicBlock *block, llvm::Module &module)
{
    codegenValue(instr->value, irContainer, block, module);

    if (instr->value->laiType->laiTypeType == instr->laiType->laiTypeType)
    {
        return instr->value->llvmValue;
        // @TODO resize
    }

    if (instr->value->laiType->laiTypeType == LaiTypeType::INTEGER)
    {
        auto intType = (LaiType_Integer *)instr->value->laiType;
        if (intType->isSigned)
        {
            return new llvm::SIToFPInst(instr->value->llvmValue, toLlvmVarType(instr->laiType, module.getContext()), "", block);
        }
        else
        {
            return new llvm::UIToFPInst(instr->value->llvmValue, toLlvmVarType(instr->laiType, module.getContext()), "", block);
        }
    }
    else
    {

        auto intType = (LaiType_Integer *)instr->laiType;
        if (intType->isSigned)
        {
            return new llvm::FPToSIInst(instr->value->llvmValue, toLlvmVarType(instr->laiType, module.getContext()), "", block);
        }
        else
        {
            return new llvm::FPToUIInst(instr->value->llvmValue, toLlvmVarType(instr->laiType, module.getContext()), "", block);
        }
    }
}

llvm::Type *toLlvmVarType(LaiType *type, llvm::LLVMContext &context)
{
    switch (type->laiTypeType)
    {
    case LaiTypeType::INTEGER:
    {
        auto t = (LaiType_Integer *)type;
        return llvm::Type::getIntNTy(context, t->size);
    }
    case LaiTypeType::FLOAT:
    {
        auto t = (LaiType_Float *)type;
        if (t->size == 32)
        {
            return llvm::Type::getFloatTy(context);
        }
        if (t->size == 64)
        {
            return llvm::Type::getDoubleTy(context);
        }
    }
    case LaiTypeType::POINTER:
    {
        auto t = (LaiType_Pointer *)type;
        return toLlvmVarType(t->pointeeType, context)->getPointerTo();
    }
    case LaiTypeType::FUNCTION:
    {
        return toLlvmFunctionType(type, context)->getPointerTo();
    }
    break;
    }

    return nullptr;
}

llvm::FunctionType *toLlvmFunctionType(LaiType *type, llvm::LLVMContext &context)
{
    switch (type->laiTypeType)
    {
    case LaiTypeType::FUNCTION:
    {
        auto t = (LaiType_Function *)type;

        std::vector<llvm::Type *> paramTypes;
        for (auto p : t->parameters)
        {
            paramTypes.push_back(toLlvmVarType(p, context));
        }
        return llvm::FunctionType::get(toLlvmVarType(t->returnType, context), paramTypes, false);
    }
    break;
    default:
        // @VALIDATE error
        break;
    }

    return nullptr;
}