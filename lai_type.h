#pragma once

#include <vector>

#include "misc_types.h"

struct LaiType
{
    enum struct Tag
    {
        UNKNOWN, // ERROR?
        FLOAT,
        INTEGER,
        POINTER,
        FUNCTION,
        ARRAY,
    };
    
    Tag tag;
    uint8_t byte_size;
};
struct LaiType_Unknown : LaiType
{
    LaiType_Unknown() { tag = Tag::UNKNOWN; };
};
struct LaiType_Number : LaiType
{
    // @TODO do we need this number, or can we use byte_size
    uint8_t bit_size = 0;
};
struct LaiType_Float : LaiType_Number
{
    LaiType_Float() { tag = Tag::FLOAT; };
    LaiType_Float(uint8_t bit_size)
    {
        tag = Tag::FLOAT;
        this->bit_size = bit_size;
        this->byte_size = bit_size / 8;
    };
};
struct LaiType_Integer : LaiType_Number
{
    LaiType_Integer() { tag = Tag::INTEGER; };
    LaiType_Integer(uint8_t bit_size, bool isSigned)
    {
        tag = Tag::INTEGER;
        this->bit_size = bit_size;
        this->byte_size = bit_size / 8;
        this->isSigned = isSigned;
    };
    
    bool isSigned = false;
};
struct LaiType_Function : LaiType
{
    LaiType_Function() { tag = Tag::FUNCTION; };
    
    std::vector<LaiType *> parameters = {};
    LaiType *returnType = nullptr;
};
struct LaiType_Pointer : LaiType
{
    LaiType_Pointer() { tag = Tag::POINTER; };
    
    LaiType *pointeeType = nullptr;
};
struct LaiType_Array : LaiType
{
    LaiType_Array() { tag = Tag::ARRAY; };
    
    LaiType *memberType = nullptr;
    uint32_t size = 0;
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