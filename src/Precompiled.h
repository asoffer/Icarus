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

using i16 = int16_t;
static_assert(sizeof(i16) == 2, "");
using i32 = int32_t;
static_assert(sizeof(i32) == 4, "");
using i64 = int64_t;
static_assert(sizeof(i64) == 8, "");
using u16 = uint16_t;
static_assert(sizeof(u16) == 2, "");
using u32 = uint32_t;
static_assert(sizeof(u32) == 4, "");
using u64 = uint64_t;
static_assert(sizeof(u64) == 8, "");
namespace debug {
extern bool timer;
extern bool parser;
extern bool ct_eval;
} // namespace debug


#include "util/pstr.h"
#include "util/timer.h"

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
struct Scope_Type;

struct Scope;
struct BlockScope;
struct FnScope;

using NPtrVec = std::vector<AST::Node *>;

struct SourceFile {
  SourceFile(const std::string &file_name = "")
      : name(file_name), ast(nullptr), ifs(name, std::ifstream::in) {}
  ~SourceFile() { ifs.close(); }

  std::string name;
  std::vector<pstr> lines;
  AST::Statements *ast;
  std::ifstream ifs;
};

struct Cursor;
#include "ErrorLog.h"

struct Cursor {
  Cursor() : offset(0), line_num(0), source_file(nullptr) {}

  pstr line;
  size_t offset;
  size_t line_num;
  SourceFile *source_file;

  std::string file_name() const { return source_file->name; }

  // Get the character that the cursor is currently pointing to
  char &operator*(void) const { return *(line.ptr + offset); }

  void MoveToNextLine() {
    assert(source_file);
    assert(!source_file->ifs.eof());
    std::string temp;
    std::getline(source_file->ifs, temp);

    // Check for null characters in line
    size_t line_length = temp.size();
    for (size_t i = 0; i < line_length; ++i) {
      if (temp[i] == '\0') {
        temp[i] = ' ';
        ErrorLog::NullCharInSrc(*this);
      } else if (temp[i] < (char)9 ||
                 ((char)13 < temp[i] && temp[i] < (char)32) ||
                 temp[i] == (char)127) { // Non-graphic characters
        temp[i] = ' ';
        ErrorLog::NonGraphicCharInSrc(*this);
      }
    }

    offset = 0;
    line   = pstr(temp.c_str());

    ++line_num;
    source_file->lines.push_back(line);
  }

  void SkipToEndOfLine() {
    while (**this != '\0') { ++offset; }
  }

  void BackUp() {
    // You can't back up to a previous line.
    assert(offset > 0);
    --offset;
  }

  void Increment() {
    if (**this != '\0') {
      ++offset;
    } else {
      MoveToNextLine();
    }
  }
};

namespace IR {
struct Block;
struct LocalStack;
struct StackFrame;
struct Func;
} // namespace IR

#include "IR/Val.h"
#include "IR/Loc.h"

namespace IR {
enum class ValType : char {
  Val, CStr, Block, Loc,  HeapAddr, ExtFn, GlobalCStr, Error, None
};

struct Value {
  union {
    IR::Val *as_val;
    IR::Loc *as_loc;
    const char *as_ext_fn;
    char *as_cstr;
    Block *as_block;
    void *as_heap_addr;
    size_t as_global_cstr;
    size_t as_blah;
  };

  ValType flag;

  explicit Value(char *p) : as_cstr(p), flag(ValType::CStr) {}
  explicit Value(Block *b) : as_block(b), flag(ValType::Block) {}

  Value() : flag(ValType::None) {}
  ~Value();
  Value(const Value &v);
  Value &operator=(const Value &v);

  static Value GlobalCStr(size_t n);
  static Value None();
  static Value Error();
  static Value CreateGlobal();
  static Value HeapAddr(void *ptr);
  static Value ExtFn(const char *name);

#define VAL_MACRO(TypeName, type_name, cpp_type)                               \
  static Value TypeName(cpp_type x);
#include "config/val.conf"
#undef VAL_MACRO

  static Value Reg(size_t n);
  static Value Arg(size_t n);
  static Value StackAddr(size_t n);
  static Value FrameAddr(size_t n);

};
inline size_t to_num(ValType v) {
  switch (v) {
  case ValType::Val: return 0;
  case ValType::CStr: return 1;
  case ValType::Block: return 2;
  case ValType::Loc: return 3;
  case ValType::HeapAddr: return 4;
  case ValType::ExtFn: return 5;
  case ValType::GlobalCStr: return 6;
  case ValType::Error: return 7;
  case ValType::None: return 8;
  }
}
inline bool operator<(const Value &lhs, const Value &rhs) {
  if (lhs.flag != rhs.flag) { return to_num(lhs.flag) < to_num(rhs.flag); }
  switch (lhs.flag) {
  case ValType::Val:
    return ArbitraryOrdering(lhs.as_val, rhs.as_val) == Order::Less;
  case ValType::Loc:
    return ArbitraryOrdering(lhs.as_loc, rhs.as_loc) == Order::Less;
  case ValType::CStr: return lhs.as_cstr < rhs.as_cstr;
  case ValType::Block: return lhs.as_block < rhs.as_block;
  case ValType::HeapAddr: return lhs.as_heap_addr < rhs.as_heap_addr;
  case ValType::ExtFn:  return lhs.as_ext_fn < rhs.as_ext_fn;
  case ValType::GlobalCStr: return lhs.as_global_cstr < rhs.as_global_cstr;
  case ValType::Error:
  case ValType::None: return false;
  }
}

inline bool operator==(const Value &lhs, const Value &rhs) {
  if (lhs.flag != rhs.flag) { return false; }
  switch (lhs.flag) {
  case ValType::Val:
    return ArbitraryOrdering(lhs.as_val, rhs.as_val) == Order::Equal;
  case ValType::Loc:
    return ArbitraryOrdering(lhs.as_loc, rhs.as_loc) == Order::Equal;
  default: return lhs.as_blah == rhs.as_blah;
  }
}
inline bool operator!=(const Value &lhs, const Value &rhs) {
  return !(lhs == rhs);
}

std::ostream &operator<<(std::ostream &os, const Value &value);

} // namespace IR

namespace Language {
extern size_t precedence(Operator op);

enum NodeType : unsigned int {
  bof           = 1u << 0,
  eof           = 1u << 1,
  newline       = 1u << 3,
  prog          = 1u << 4,
  stmts         = 1u << 5,
  if_stmt       = 1u << 6,
  braced_stmts  = 1u << 7,
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

struct NNT {
  NNT() = default;
  AST::Node *node              = nullptr;
  Language::NodeType node_type = Language::bof;
  NNT(AST::Node *n, Language::NodeType nt) : node(n), node_type(nt) {}
  static NNT Invalid() {
    // Name of this function is clearer than just using default constructor
    return NNT();
  }
};
inline bool operator==(NNT lhs, NNT rhs) {
  return lhs.node == rhs.node && lhs.node_type == rhs.node_type;
}
inline bool operator!=(NNT lhs, NNT rhs) { return (lhs == rhs); }

#include "IR/IR.h"
#include "AST/AST.h"

#endif // ICARUS_PRECOMPILED_H
