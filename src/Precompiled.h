#ifndef ICARUS_PRECOMPILED_H
#define ICARUS_PRECOMPILED_H

#include <iostream>
#include <string>
#include <map>
#include <set>
#include <vector>
#include <queue>
#include <type_traits>
#include <stack>
#include <sstream>
#include <fstream>

// Each time a file is imported, it will be added to the queue. We then parse
// each file off the queue until the queue is empty. We avoid circular calls by
// checking if the map is already filled before parsing.
extern std::queue<std::string> file_queue;

namespace llvm {
class Value;
} // namespace llvm

namespace AST {
enum class DeclType { Std, Infer, In, Tick };

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

struct Scope;
struct BlockScope;
struct FnScope;

using NPtrVec = std::vector<AST::Node *>;

#include "Context.h"

enum class ParserMode { Good, BadLine, BadBlock, BadFile, Done, Same };

namespace Time {
  using Eval = int;
  constexpr Eval either  = 0x0;
  constexpr Eval compile = 0x1;
  constexpr Eval run     = 0x2;
  constexpr Eval mixed   = 0x3;
  constexpr Eval error   = 0x7;
}  // namespace Time

namespace Dependency {
enum Flag : char {
  unseen    = 0x00,
  type_seen = 0x01,
  val_seen  = 0x04,
  tv_seen   = 0x05,
  type_done = 0x02,
  val_done  = 0x08,
  tv_done   = 0x0a
};

// This is terribly wasteful due to poor alignment.
// Maybe a bottleneck for large programs but probably not.
// In any event, for your own pride you should pack these neater.
struct PtrWithTorV {
  PtrWithTorV() = delete;
  PtrWithTorV(AST::Node *ptr, bool torv) : ptr_(ptr), torv_(torv) {}
  AST::Node *ptr_;
  bool torv_; // true => type, false => value
};

extern void type_type(AST::Node *depender, AST::Node *dependee);
extern void type_value(AST::Node *depender, AST::Node *dependee);
extern void value_type(AST::Node *depender, AST::Node *dependee);
extern void value_value(AST::Node *depender, AST::Node *dependee);

extern void traverse_from(PtrWithTorV);

extern void record(AST::Node *node);
extern void mark_as_done(AST::Node *e);
extern void add_to_table(AST::Node *depender);
extern void assign_order();
extern void rebuild_already_seen();
extern void write_graphviz();
} // namespace Dependency

namespace Language {
// Using masks to make determination node types easier. Starting masks in the
// 8th bit, leaves bits 0-7 for standard enumeration. This is safe because we
// will never have more than 128 NodeTypes in a given section.
constexpr int MASK_binary_operator      = 1 << 8;
constexpr int MASK_left_unary_operator  = 1 << 9;
constexpr int MASK_right_unary_operator = 1 << 9;
constexpr int MASK_expression           = 1 << 10;

constexpr int MASK_operator =
    MASK_binary_operator | MASK_left_unary_operator | MASK_right_unary_operator;

enum NodeType {
  unknown,
  bof,
  eof,
  newline,
  comment,
  identifier,

  // Literals
  int_literal,
  uint_literal,
  real_literal,
  char_literal,
  string_literal,
  type_literal,
  fn_literal,

  key_value_pair,
  key_value_pair_list,
  fn_expression,
  scope,
  DECL_LIST,
  statements,
  missing_newline_statements,

  // Parens, braces, and brackets
  left_paren,
  right_paren,
  left_brace,
  right_brace,
  left_bracket,
  right_bracket,
  semicolon,

  // TODO use RESERVED_MACRO to generate these.
  // Figure out the right way to do that given that they may need different
  // masks
  // Reserved words
  // TODO why are ascii, input, ord not an expression?
  reserved_break,
  reserved_if,
  reserved_else,
  reserved_case,
  reserved_for,
  reserved_enum,
  reserved_while,
  reserved_continue,
  reserved_ascii,
  reserved_ord,
  reserved_import,
  reserved_string,
  reserved_alloc,
  reserved_struct,
  reserved_repeat,
  reserved_restart,
  reserved_input,
  hashtag,

  STMT_FOR,
  STMT_WHILE,
  STMT_IF,
  STMT_IF_ELSE,
  STMT_JUMP,
  STMT_ASSIGN,

  STMT_DECL_STD,
  STMT_DECL_INFER,
  STMT_DECL_GENERATE,
  DECL_IN,
  DECL_IN_LIST,

  // BEGIN USING MASKS

  // binary operators
  generic_operator = MASK_binary_operator,

  // Note: reserved_in is also a decl operator, but it's not an expression so
  // we must keep it separate.
  DECL_OPERATOR_STD,
  DECL_OPERATOR_INFER,

  assign_operator,
  fn_arrow,
  binary_boolean_operator,
  bool_operator,
  comma,
  dot,
  rocket_operator,
  reserved_in,

  // left unary operators
  reserved_return = MASK_left_unary_operator,
  reserved_print,
  reserved_free,
  dereference,
  not_operator,

  // unary operator + binary operator
  indirection = MASK_binary_operator | MASK_left_unary_operator,
  negation,
  dots,

  // expressions
  expression = MASK_expression,
  reserved_true,
  reserved_false,
  reserved_null,
  reserved_type,

  // expression + binary operator
  DECL_OPERATOR_GENERATE = MASK_binary_operator | MASK_expression,
};

enum class Terminal {
  Ord,
  ASCII,
  Alloc,
  Input,
  Return,
  Else,
  True,
  False,
  Null,
  Char,
  Int,
  Real,
  Type,
  UInt,
  StringLiteral
};

enum class Operator {
#define OPERATOR_MACRO(name, symbol, prec, assoc) name,
#include "config/operator.conf"
#undef OPERATOR_MACRO
};

inline bool is_operator(NodeType t) { return (t & MASK_operator) != 0; }
} // namespace Language

#include "AST.h"

// Constants for associativity
constexpr size_t left_assoc = 0;
constexpr size_t right_assoc = 1;
constexpr size_t non_assoc = 2;
constexpr size_t chain_assoc = 3;
constexpr size_t assoc_mask = 3;

#undef VIRTUAL_METHODS_FOR_NODES
#undef EXPR_FNS
#undef ENDING
#undef OVERRIDE

#endif // ICARUS_PRECOMPILED_H
