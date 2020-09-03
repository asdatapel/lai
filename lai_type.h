#pragma once

#include <vector>

#include "misc_types.h"

enum struct LaiTypeType
{
    UNKNOWN, // ERROR?
    FLOAT,
    INTEGER,
    POINTER,
    FUNCTION,
};

struct LaiType
{
    LaiTypeType laiTypeType;
};
struct LaiType_Unknown : LaiType
{
    LaiType_Unknown() { laiTypeType = LaiTypeType::UNKNOWN; };
};
struct LaiType_Number : LaiType
{
    uint8_t size = 0;
};
struct LaiType_Float : LaiType_Number
{
    LaiType_Float() { laiTypeType = LaiTypeType::FLOAT; };
    LaiType_Float(uint8_t size)
    {
        laiTypeType = LaiTypeType::FLOAT;
        this->size = size;
    };
};
struct LaiType_Integer : LaiType_Number
{
    LaiType_Integer() { laiTypeType = LaiTypeType::INTEGER; };
    LaiType_Integer(uint8_t size, bool isSigned)
    {
        laiTypeType = LaiTypeType::INTEGER;
        this->size = size;
        this->isSigned = isSigned;
    };

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
static LaiType_Integer builtinTypeBool(32, false);
static LaiType_Integer builtinTypeI8(8, false);
static LaiType_Integer builtinTypeS8(8, true);
static LaiType_Integer builtinTypeI32(32, false);
static LaiType_Integer builtinTypeS32(32, true);
static LaiType_Integer builtinTypeI64(64, false);
static LaiType_Integer builtinTypeS64(64, true);
static LaiType_Float builtinTypeF32(32);
static LaiType_Float builtinTypeF64(64);
//////////////////////////////////////

LaiType *parseBuiltinType(Segment identifier)
{
    if (identifier.equals("bool"))
    {
        return &builtinTypeBool;
    }
    if (identifier.equals("i8"))
    {
        return &builtinTypeI8;
    }
    if (identifier.equals("s8"))
    {
        return &builtinTypeS8;
    }
    if (identifier.equals("i32"))
    {
        return &builtinTypeI32;
    }
    if (identifier.equals("s32"))
    {
        return &builtinTypeS32;
    }
    if (identifier.equals("i64"))
    {
        return &builtinTypeI64;
    }
    if (identifier.equals("s64"))
    {
        return &builtinTypeS64;
    }
    if (identifier.equals("f32"))
    {
        return &builtinTypeF32;
    }
    if (identifier.equals("f64"))
    {
        return &builtinTypeF64;
    }
    return nullptr;
}