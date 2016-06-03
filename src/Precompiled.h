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

// Each time a file is imported, it will be added to the queue. We then parse
// each file off the queue until the queue is empty. We avoid circular calls by
// checking if the map is already filled before parsing.
extern std::queue<std::string> file_queue;

#include "ConstantsAndEnums.h"

#define NOT_YET assert(false && "Not yet implemented")
#define UNREACHABLE assert(false && "Unreachable code-ptah")

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
struct QuantumType;
struct RangeType;
struct SliceType;

struct Scope;
struct BlockScope;
struct FnScope;

using NPtrVec = std::vector<AST::Node *>;

struct TokenLocation {
  std::string file;
  size_t line_num;
  char offset;

  TokenLocation() : file(""), line_num(0), offset(0){};
};

namespace Context {
// TODO make to use types of the right size.
union Value {
  bool as_bool;
  char as_char;
  long as_int;
  double as_real;
  size_t as_uint;
  char *as_str;
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
  explicit Value(char *c_str) { as_str = c_str; }
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
// Using masks to make determination node types easier. Starting masks in the
// 8th bit, leaves bits 0-7 for standard enumeration. This is safe because we
// will never have more than 128 NodeTypes in a given section.

constexpr int OP_ = 1 << 6;

enum NodeType : char {
  unknown,
  bof,
  eof,
  newline,
  comment,
  prog,
  stmts,
  if_stmt,
  one_stmt,

  expr,
  fn_expr,

  l_paren,
  r_paren,
  l_bracket,
  r_bracket,
  l_brace,
  r_brace,

  semicolon,
  hashtag,
  kw_expr_block,
  kw_if,
  kw_else,
  kw_block,
  kw_struct,

  op_l = OP_,
  op_b, colon, eq, comma,
  op_bl,
  dots,
  op_lt,
  fn_arrow,
};

inline bool is_operator(NodeType t) { return (t & OP_) != 0; }
} // namespace Language

#include "IR/IR.h"
#include "AST/AST.h"

#endif // ICARUS_PRECOMPILED_H
