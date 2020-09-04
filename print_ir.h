#include <iostream>
#include <string>

#include "ir.h"

int index(IrContainer *container, IrInstr *instr)
{
    for (int i = 0; i < container->instrs.size(); i++)
    {
        if (instr == container->instrs[i])
            return i;
    }

    return -1;
}
int declIndex(IrContainer *container, IrInstr *instr)
{
    for (int i = 0; i < container->declarations.size(); i++)
    {
        if (instr == container->declarations[i])
            return i;
    }

    return -1;
}

void printIr(IrContainer *container, int indents = 0)
{
    for (int i = 0; i < container->declarations.size(); i++)
    {
        for (int i = 0; i < indents; i++)
        {
            std::cout << "\t";
        }
        std::cout << "%decl" << i << "\n";
    }

    for (int i = 0; i < container->instrs.size(); i++)
    {
        for (int i = 0; i < indents; i++)
        {
            std::cout << "\t";
        }

        std::cout << "%" << i << " ";

        auto instruction = container->instrs[i];
        switch (instruction->type)
        {
        case IrInstr::Type::CMP_EQ:
        {
            auto instr = (IrCmpEqual *)instruction;
            std::cout << "CMP_EQUAL ";
            std::cout << index(container, instr->lhs);
            std::cout << index(container, instr->rhs);
        }
        break;
        case IrInstr::Type::DECLARATION:
        {
            auto instr = (IrDeclaration *)instruction;
            std::cout << "INIT/STORE ";
            std::cout << declIndex(container, instr) << " ";
            std::cout << index(container, instr->initializer);
        }
        break;
        case IrInstr::Type::FLOAT_LITERAL:
        {
            auto instr = (IrFloatLiteral *)instruction;
            std::cout << "FLOAT_LITERAL ";
            std::cout << instr->value;
        }
        break;
        case IrInstr::Type::FUNCTION:
        {
            auto instr = (IrFunction *)instruction;
            std::cout << "FUNCTION ";
            printIr(instr, indents + 1);
        }
        break;
        case IrInstr::Type::INTEGER_LITERAL:
        {
            auto instr = (IrIntegerLiteral *)instruction;
            std::cout << "INTEGER_LITERAL ";
            std::cout << instr->value;
        }
        break;
        }

        std::cout << std::endl;
    }
}