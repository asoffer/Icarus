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

// Each time a file is imported, it will be added to the queue. We then parse
// each file off the queue until the queue is empty. We avoid circular calls by
// checking if the map is already filled before parsing.
extern std::queue<std::string> file_queue;

#include "ConstantsAndEnums.h"

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
struct DependentType;
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

#include "Context.h"
#include "ErrorLog.h"

#include "DependencyFunctions.h"

namespace Language {
// Using masks to make determination node types easier. Starting masks in the
// 8th bit, leaves bits 0-7 for standard enumeration. This is safe because we
// will never have more than 128 NodeTypes in a given section.

constexpr int OP_ = 1 << 6;

enum NodeType : char {
  keep_current,
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
  op_b, comma,
  op_bl,
  dots,
  op_lt,
  fn_arrow,
};

inline bool is_operator(NodeType t) { return (t & OP_) != 0; }
} // namespace Language

#include "AST.h"

#endif // ICARUS_PRECOMPILED_H
