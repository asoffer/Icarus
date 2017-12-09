#include "type/type.h"
#include "ast/ast.h"
#include "ir/ir.h"
#include "scope.h"
#include "util/timer.h"
#include <cstring>
Timer timer;

// Debug flags and their default values
namespace debug {
bool parser        = false;
bool timer         = false;
bool ct_eval       = false;
bool no_validation = false;
} // namespace debug

std::string Escape(char c) {
  if (c == '\n') { return "\\n"; }
  if (c == '\r') { return "\\r"; }
  if (c == '\t') { return "\\t"; }
  if (c < 32) { return "\\" + std::to_string(c); }
  return std::string(1, c);
}

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
    NOT_YET();
  } else {
    UNREACHABLE();
  }
}
