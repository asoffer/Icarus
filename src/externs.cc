#include "type/type.h"
#include "ast/ast.h"
#include "ir/ir.h"
#include "scope.h"
#include "util/timer.h"
#include <cstring>
Timer timer;

namespace Language {
// Associativity stored in the lowest two bits.
size_t precedence(Operator op) {
  switch (op) {
#define OPERATOR_MACRO(name, symbol, prec, assoc)                              \
  case Operator::name: return (((prec) << 2) + (assoc));
#include "config/operator.conf"
#undef OPERATOR_MACRO
  default: UNREACHABLE;
  }
}
} // namespace Language

// Debug flags and their default values
namespace debug {
bool parser  = false;
bool timer   = false;
bool ct_eval = false;
} // namespace debug

std::string Escape(char c) {
  if (c == '\n') { return "\\n"; }
  if (c == '\r') { return "\\r"; }
  if (c == '\t') { return "\\t"; }
  if (c < 32) { return "\\" + std::to_string(c); }
  return std::string(1, c);
}

namespace Hashtag {
static std::vector<const char *> table = {"const"};

size_t GetOrFailValue(const std::string &tag) {
  for (size_t i = 0; i < table.size(); ++i) {
    if (tag == table[i]) { return i; }
  }

  return FAIL;
}

size_t Get(const std::string &tag) {
  for (size_t i = 0; i < table.size(); ++i) {
    if (tag == table[i]) { return i; }
  }

  size_t result = table.size();
  char *new_tag = new char[tag.size() + 1];
  strcpy(new_tag, tag.c_str());
  table.push_back(new_tag);

  ASSERT(table[result] == new_tag, "");

  return result;
}
} // namespace Hashtag

IR::Val PtrCallFix(IR::Val v) {
  ASSERT(v.type->is<Pointer>(), "");
  if (ptr_cast<Pointer>(v.type)->pointee->is_big()) {
    return v;
 } else {
   return IR::Load(v);
 }
}
AST::FunctionLiteral *GetFunctionLiteral(AST::Expression *expr) {
  if (expr->is<AST::FunctionLiteral>()) {
    return (AST::FunctionLiteral *)expr;

  } else if (expr->is<AST::Identifier>()) {
    auto id = (AST::Identifier *)expr;
    ASSERT(id->decl->IsInferred(), "");
    return GetFunctionLiteral(id->decl->init_val.get());

  } else if (expr->is<AST::Declaration>()) {
    auto decl = (AST::Declaration *)expr;
    ASSERT(decl->IsInferred(), "");
    return GetFunctionLiteral(decl->init_val.get());
  } else if (expr->is<AST::Binop>()) {
    NOT_YET;
  } else {
    UNREACHABLE;
  }
}
