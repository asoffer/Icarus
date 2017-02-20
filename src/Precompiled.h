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
#include <memory>

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

template <typename T, typename... Args> std::unique_ptr<T> make_unique(Args&&... args) {
  return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
}

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
struct ScopeLiteral;
struct CodeBlock;
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

  static Cursor Behind(const Cursor &cursor, u64 dist){
    assert(cursor.offset >= dist);
      Cursor result = cursor;
      result.offset -= dist;
      return result;
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

inline bool operator<(const Value &lhs, const Value &rhs) {
  if (lhs.flag != rhs.flag) { return static_cast<int>(lhs.flag) < static_cast<int>(rhs.flag); }
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

enum NodeType : u32 {
  bof            = 1ull << 0,
  eof            = 1ull << 1,
  newline        = 1ull << 3,
  prog           = 1ull << 4,
  stmts          = 1ull << 5,
  braced_stmts   = 1ull << 6,
  expr           = 1ull << 7,
  fn_expr        = 1ull << 8,
  l_paren        = 1ull << 9,
  r_paren        = 1ull << 10,
  l_bracket      = 1ull << 11,
  r_bracket      = 1ull << 12,
  l_brace        = 1ull << 13,
  r_brace        = 1ull << 14,
  semicolon      = 1ull << 15,
  hashtag        = 1ull << 16,
  kw_expr_block  = 1ull << 17,
  kw_else        = 1ull << 18,
  kw_block       = 1ull << 19,
  kw_struct      = 1ull << 20,
  l_double_brace = 1ull << 21,
  r_double_brace = 1ull << 22,

  op_l     = 1ull << 23,
  op_b     = 1ull << 24,
  colon    = 1ull << 25,
  eq       = 1ull << 26,
  comma    = 1ull << 27,
  op_bl    = 1ull << 28,
  dots     = 1ull << 29,
  op_lt    = 1ull << 30,
  fn_arrow = 1ull << 31,
};

constexpr unsigned int OP_ =
    op_l | op_b | colon | eq | comma | op_bl | dots | op_lt | fn_arrow;

} // namespace Language

inline size_t MoveForwardToAlignment(size_t ptr, size_t alignment) {
  return ((ptr - 1) | (alignment - 1)) + 1;
}

struct NNT {
  NNT() = default;
  std::unique_ptr<AST::Node> node = nullptr;
  Language::NodeType node_type = Language::bof;
  NNT(const Cursor &cursor, const std::string &token, Language::NodeType nt);

  NNT(std::unique_ptr<AST::Node> n, Language::NodeType nt)
      : node(std::move(n)), node_type(nt) {}
  static NNT Invalid() {
    // Name of this function is clearer than just using default constructor
    return NNT();
  }
};
inline bool operator==(const NNT& lhs, const NNT& rhs) {
  return lhs.node.get() == rhs.node.get() && lhs.node_type == rhs.node_type;
}
inline bool operator!=(const NNT& lhs, const NNT& rhs) { return (lhs == rhs); }

#include "IR/IR.h"
#include "AST/AST.h"

#endif // ICARUS_PRECOMPILED_H
