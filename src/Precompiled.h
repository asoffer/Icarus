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
struct EnumLiteral;
struct StructLiteral;
struct ParametricStructLiteral;
} // namespace AST

struct Type;
struct Primitive;
struct Array;
struct Tuple;
struct Pointer;
struct Function;
struct Enumeration;
struct Structure;
struct ParametricStructure;
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

namespace Context {
// TODO make to use types of the right size.
union Value {
  bool as_bool;
  char as_char;
  long as_int;
  double as_real;
  size_t as_uint;
  char *as_cstr;
  void *as_null;
  Type *as_type;
  AST::Expression *as_expr;

  Value(std::nullptr_t = nullptr) { as_null = nullptr; }
  explicit Value(bool b) { as_bool = b; }
  explicit Value(char c) { as_char = c; }
  explicit Value(long n) { as_int = n; }
  explicit Value(double d) { as_real = d; }
  explicit Value(size_t n) { as_uint = n; }
  explicit Value(Type *t) { as_type = t; }
  explicit Value(char *c_str) { as_cstr = c_str; }
  explicit Value(AST::Expression *e) { as_expr = e; }
};

inline bool operator==(const Value &lhs, const Value &rhs) {
  return lhs.as_real == rhs.as_real;
}

inline bool operator!=(const Value &lhs, const Value &rhs) {
  return !(lhs == rhs);
}

// For std::map<>s
inline bool operator<(const Value &lhs, const Value &rhs) {
  return lhs.as_real < rhs.as_real;
}
} // namespace Context

#include "ErrorLog.h"

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
