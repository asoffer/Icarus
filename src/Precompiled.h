#ifndef ICARUS_PRECOMPILED_H
#define ICARUS_PRECOMPILED_H

// Common standard headers
#include <iostream>
#include <string>
#include <map>
#include <set>
#include <vector>
#include <queue>
#include <stack>
#include <sstream>
#include <fstream>

// TODO Figure out what you need from this.
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#include "util/pstr.h"

// Each time a file is imported, it will be added to the queue. We then parse
// each file off the queue until the queue is empty. We avoid circular calls by
// checking if the map is already filled before parsing.
extern std::queue<std::string> file_queue;

namespace debug {
extern bool timer;
extern bool parser;
extern bool parametric_struct;
extern bool ct_eval;
} // namespace debug


#include "ConstantsAndEnums.h"

#define NOT_YET assert(false && "Not yet implemented")
#define UNREACHABLE assert(false && "Unreachable code-path")

#ifdef DEBUG
#define AT(access) .at((access))
#else
#define AT(access) [(access)]
#endif


namespace llvm {
class Value;
class Function;
} // namespace llvm

namespace AST {
struct Node;
struct Expression;
struct Identifier;
struct TokenNode;
struct Declaration;
struct Binop;
struct Statements;
struct FunctionLiteral;
} // namespace AST

struct Type;
struct Primitive;
struct Array;
struct Tuple;
struct Pointer;
struct Function;
struct Enum;
struct Struct;
struct ParamStruct;
struct TypeVariable;
struct RangeType;
struct SliceType;

struct Scope;
struct BlockScope;
struct FnScope;

using NPtrVec = std::vector<AST::Node *>;

struct Cursor {
  Cursor() : offset(0), line_num(0), file_name("") {}

  pstr line;
  size_t offset;
  size_t line_num;
  const char *file_name;

  // Get the character that the cursor is currently pointing to
  inline char &operator*(void) const { return *(line.ptr + offset); }
};

#include "ErrorLog.h"

namespace IR {
struct Func;
struct Block;
struct LocalStack;
struct StackFrame;

enum class ValType : char {
  B, C, I, R, U, T, F, CStr, Block, Reg, Arg, StackAddr, FrameAddr, HeapAddr, GlobalAddr, U16, U32,
  GlobalCStr, Null
};

struct Value {
  union {
    bool as_bool;
    char as_char;
    long as_int;
    double as_real;
    uint16_t as_uint16;
    uint32_t as_uint32;
    size_t as_uint;
    Type *as_type;
    Func *as_func;
    char *as_cstr;
    size_t as_reg;
    size_t as_arg;
    Block *as_block;
    size_t as_stack_addr, as_frame_addr, as_global_addr;
    void *as_heap_addr;
    Type *as_null;
    size_t as_global_cstr;
  };

  ValType flag;

  explicit Value(bool b) : as_bool(b), flag(ValType::B) {}
  explicit Value(char c) : as_char(c), flag(ValType::C) {}
  explicit Value(long n) : as_int(n), flag(ValType::I) {}
  explicit Value(double d) : as_real(d), flag(ValType::R) {}
  explicit Value(size_t n) : as_uint(n), flag(ValType::U) {}
  explicit Value(Type *t) : as_type(t), flag(ValType::T) {}
  explicit Value(Func *f) : as_func(f), flag(ValType::F) {}
  explicit Value(char *p) : as_cstr(p), flag(ValType::CStr) {}
  explicit Value(Block *b) : as_block(b), flag(ValType::Block) {}
  explicit Value(uint16_t n) : as_uint16(n), flag(ValType::U16) {}
  explicit Value(uint32_t n) : as_uint32(n), flag(ValType::U32) {}

  Value() : flag(ValType::Reg) {}

  static Value GlobalCStr(size_t n);
  static Value None();
  static Value StackAddr(size_t n);
  static Value Null(Type *t);
  static Value Reg(size_t n);
  static Value FrameAddr(size_t n);
  static Value CreateGlobal();
  static Value HeapAddr(void *ptr);
  static Value Arg(size_t n);
};

// For std::map<>s
inline bool operator<(const Value &lhs, const Value &rhs) {
  return lhs.as_type < rhs.as_type;
}
inline bool operator==(const Value &lhs, const Value &rhs) {
  return lhs.as_type == rhs.as_type;
}
inline bool operator!=(const Value &lhs, const Value &rhs) {
  return !(lhs == rhs);
}

std::ostream &operator<<(std::ostream &os, const Value &value);

} // namespace IR

namespace Language {
extern size_t precedence(Operator op);

// Using masks to make determination node types easier. Starting masks in the
// 8th bit, leaves bits 0-7 for standard enumeration. This is safe because we
// will never have more than 128 NodeTypes in a given section.

enum NodeType : unsigned int {
  bof           = 1u << 0,
  eof           = 1u << 1,
  newline       = 1u << 3,
  prog          = 1u << 4,
  stmts         = 1u << 5,
  if_stmt       = 1u << 6,
  one_stmt      = 1u << 7,
  expr          = 1u << 8,
  fn_expr       = 1u << 9,
  l_paren       = 1u << 10,
  r_paren       = 1u << 11,
  l_bracket     = 1u << 12,
  r_bracket     = 1u << 13,
  l_brace       = 1u << 14,
  r_brace       = 1u << 15,
  semicolon     = 1u << 16,
  hashtag       = 1u << 17,
  kw_expr_block = 1u << 18,
  kw_if         = 1u << 19,
  kw_else       = 1u << 20,
  kw_block      = 1u << 21,
  kw_struct     = 1u << 22,

  op_l          = 1u << 23,
  op_b          = 1u << 24,
  colon         = 1u << 25,
  eq            = 1u << 26,
  comma         = 1u << 27,
  op_bl         = 1u << 28,
  dots          = 1u << 29,
  op_lt         = 1u << 30,
  fn_arrow      = 1u << 31,
};

constexpr unsigned int OP_ =
    op_l | op_b | colon | eq | comma | op_bl | dots | op_lt | fn_arrow;

} // namespace Language

inline size_t MoveForwardToAlignment(size_t ptr, size_t alignment) {
  return ((ptr - 1) | (alignment - 1)) + 1;
}

#include "IR/IR.h"
#include "AST/AST.h"

#endif // ICARUS_PRECOMPILED_H
