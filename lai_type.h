#pragma once

#include <vector>

#include "misc_types.h"

enum struct LaiTypeType
{
    UNKNOWN, // ERROR?
    FLOATING_POINT,
    INTEGER,
    FUNCTION,
    POINTER,
};

struct LaiType
{
    LaiTypeType laiTypeType;
};
struct LaiType_Unknown : LaiType
{
    LaiType_Unknown() { laiTypeType = LaiTypeType::UNKNOWN; };
};
struct LaiType_FloatingPoint : LaiType
{
    LaiType_FloatingPoint() { laiTypeType = LaiTypeType::FLOATING_POINT; };

    uint8_t size = 0; // 32 or 64 only
};
struct LaiType_Integer : LaiType
{
    LaiType_Integer() { laiTypeType = LaiTypeType::INTEGER; };
    LaiType_Integer(uint8_t size, bool isSigned)
    {
        laiTypeType = LaiTypeType::INTEGER;
        this->size = size;
        this->isSigned = isSigned;
    };

    uint8_t size = 0; // 8, 16, 32 or 64 only
    bool isSigned = false;
};
struct LaiType_Function : LaiType
{
    LaiType_Function() { laiTypeType = LaiTypeType::FUNCTION; };

    std::vector<LaiType *> parameters = {};
    LaiType *returnType = nullptr;
};
struct LaiType_Pointer : LaiType
{
    LaiType_Pointer() { laiTypeType = LaiTypeType::POINTER; };

    LaiType *pointeeType = nullptr;
};

////////////////// Builtins //////////
static LaiType_Integer builtinTypeChar(8, false);
static LaiType_Integer builtinTypeI32(32, false);
static LaiType_Integer builtinTypeS32(32, true);
//////////////////////////////////////

LaiType *resolveBuiltinType(Segment identifier)
{
    if (identifier.equals("i32"))
    {
        return &builtinTypeI32;
    }
    if (identifier.equals("s32"))
    {
        return &builtinTypeS32;
    }
    return nullptr;
}