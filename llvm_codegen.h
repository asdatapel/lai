#pragma once

#include <iostream>

#include <llvm/IR/IRBuilder.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/raw_ostream.h>

#include "debug_util.h"
#include "ir.h"

void codegenModule(IrContainer *);
llvm::BasicBlock *codegenValue(IrInstr *, llvm::BasicBlock *, llvm::Function *, llvm::Module &);
llvm::Value *codegenCast(IrCast *instr, llvm::BasicBlock *, llvm::Function *, llvm::Module &);
llvm::Function *codegenFunction(IrFunction *, llvm::Module &);
llvm::Type *toLlvmVarType(LaiType *, llvm::LLVMContext &);
llvm::FunctionType *toLlvmFunctionType(LaiType *, llvm::LLVMContext &);

void codegenModule(IrContainer *irRoot)
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

    for (auto declaration : irRoot->declarations)
    {
        auto global = (IrDeclaration *)declaration;
        auto llvmType = toLlvmVarType(resolveDereferenceType(global->laiType), llvmContext);
        auto llvmGlobal = new llvm::GlobalVariable(module, llvmType, false, llvm::GlobalVariable::ExternalLinkage, nullptr);
        llvmGlobal->setInitializer(llvm::Constant::getNullValue(llvmType));

        global->llvmValue = llvmGlobal;
    }

    for (auto instr : irRoot->instrs)
    {
        if (instr->type == IrInstr::Type::DECLARATION)
        {
            auto irDeclaration = (IrDeclaration *)instr;

            codegenValue(irDeclaration->initializer, entryBlock, mainFunction, module);
            if (auto init = llvm::dyn_cast<llvm::Constant>(irDeclaration->initializer->llvmValue))
            {
                ((llvm::GlobalVariable *)irDeclaration->llvmValue)->setInitializer(init);
            }
            else
            {
                auto x = irDeclaration->llvmValue->getType();
                new llvm::StoreInst(irDeclaration->initializer->llvmValue, irDeclaration->llvmValue, false, entryBlock);
            }
        }
        else
        {
            codegenValue(instr, entryBlock, mainFunction, module);
        }
    }

    ////temp main function////
    //////////////////////////
    auto lastDec = irRoot->declarations[irRoot->declarations.size() - 1]->llvmValue;
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

llvm::BasicBlock *codegenValue(IrInstr *instr, llvm::BasicBlock *block, llvm::Function *llvmFunction, llvm::Module &module)
{
    if (instr->llvmValue && instr->type != IrInstr::Type::LABEL)
    {
        return block; // value has already been codegenned
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
            codegenValue(a, block, llvmFunction, module);
            args.push_back(a->llvmValue);
        }

        codegenValue(irFunctionCall->function, block, llvmFunction, module);
        instr->llvmValue = llvm::CallInst::Create(irFunctionCall->function->llvmValue, args, "", block);
    }
    break;
    case IrInstr::Type::ADD:
    {
        auto irAdd = (IrAdd *)instr;
        codegenValue(irAdd->lhs, block, llvmFunction, module);
        codegenValue(irAdd->rhs, block, llvmFunction, module);

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
        codegenValue(irSub->lhs, block, llvmFunction, module);
        codegenValue(irSub->rhs, block, llvmFunction, module);

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
    case IrInstr::Type::CMP_EQUAL:
    {
        auto irSub = (IrCmpEqual *)instr;
        codegenValue(irSub->lhs, block, llvmFunction, module);
        codegenValue(irSub->rhs, block, llvmFunction, module);

        auto op = (irSub->laiType->laiTypeType == LaiTypeType::FLOAT) ? llvm::Instruction::OtherOps::FCmp : llvm::Instruction::OtherOps::ICmp;
        auto pred = (irSub->laiType->laiTypeType == LaiTypeType::FLOAT) ? llvm::CmpInst::Predicate::FCMP_OEQ : llvm::CmpInst::Predicate::ICMP_EQ;
        auto cmp = llvm::CmpInst::Create(op, pred, irSub->lhs->llvmValue, irSub->rhs->llvmValue, "", block);
        instr->llvmValue = llvm::CastInst::Create(llvm::Instruction::CastOps::ZExt, cmp, llvm::Type::getInt32Ty(module.getContext()), "", block);
    }
    break;
    case IrInstr::Type::LOAD:
    {
        auto irLoad = (IrLoad *)instr;
        codegenValue(irLoad->value, block, llvmFunction, module);
        instr->llvmValue = new llvm::LoadInst(irLoad->value->llvmValue, "", false, block);
    }
    break;
    case IrInstr::Type::STORE:
    {
        auto irStore = (IrStore *)instr;
        codegenValue(irStore->target, block, llvmFunction, module);
        codegenValue(irStore->value, block, llvmFunction, module);
        instr->llvmValue = new llvm::StoreInst(irStore->value->llvmValue, irStore->target->llvmValue, false, block);
    }
    break;
    case IrInstr::Type::CAST:
    {
        auto irCast = (IrCast *)instr;
        irCast->llvmValue = codegenCast(irCast, block, llvmFunction, module);
    }
    break;
    case IrInstr::Type::RETURN:
    {
        auto irReturn = (IrReturn *)instr;
        codegenValue(irReturn->value, block, llvmFunction, module);
        instr->llvmValue = llvm::ReturnInst::Create(module.getContext(), irReturn->value->llvmValue, block);
    }
    break;
    case IrInstr::Type::LABEL:
    {
        auto irLabel = (IrLabel *)instr;

        // we're expecting the basic block to have already been created by whichever instr references it
        block = (llvm::BasicBlock *)irLabel->llvmValue;
    }
    break;
    case IrInstr::Type::JUMP_IF:
    {
        auto irJumpIf = (IrJumpIf *)instr;

        if (!irJumpIf->inside->llvmValue)
            irJumpIf->inside->llvmValue = llvm::BasicBlock::Create(module.getContext(), "ifinside", llvmFunction, 0);
        if (!irJumpIf->outside->llvmValue)
            irJumpIf->outside->llvmValue = llvm::BasicBlock::Create(module.getContext(), "ifoutside", llvmFunction, 0);

        auto boolCondition = llvm::CmpInst::Create(llvm::Instruction::OtherOps::ICmp, llvm::CmpInst::Predicate::ICMP_NE, irJumpIf->condition->llvmValue,
                                                   llvm::ConstantInt::get(module.getContext(), llvm::APInt(32, 0, true)), "", block);

        instr->llvmValue = llvm::BranchInst::Create((llvm::BasicBlock *)irJumpIf->inside->llvmValue, (llvm::BasicBlock *)irJumpIf->outside->llvmValue, boolCondition, block);
    }
    break;
    default:
        // @VALIDATE error
        break;
    };

    return block;
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

    for (auto declaration : irFunction->declarations)
    {
        if (!declaration->llvmValue)
        {
            auto llvmType = toLlvmVarType(resolveDereferenceType(declaration->laiType), module.getContext());
            declaration->llvmValue = new llvm::AllocaInst(llvmType, 0, "", entryBlock);
        }
    }

    auto currentBlock = entryBlock;
    for (auto instr : irFunction->instrs)
    {
        currentBlock = codegenValue(instr, currentBlock, llvmFunction, module);
    }

    return llvmFunction;
};

llvm::Value *codegenCast(IrCast *instr, llvm::BasicBlock *block, llvm::Function *llvmFunction, llvm::Module &module)
{
    codegenValue(instr->value, block, llvmFunction, module);

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
    break;
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
    break;
    case LaiTypeType::POINTER:
    {
        auto t = (LaiType_Pointer *)type;
        return toLlvmVarType(t->pointeeType, context)->getPointerTo();
    }
    break;
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