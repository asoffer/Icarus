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

IR::Val PtrCallFix(IR::Val v) { return v.type->is_big() ? v : IR::Load(v); }

Type *GetFunctionTypeReferencedIn(Scope *scope, const std::string &fn_name,
                                  Type *input_type) {
  for (auto scope_ptr = scope; scope_ptr; scope_ptr = scope_ptr->parent) {
    auto id_ptr = scope_ptr->IdentifierHereOrNull(fn_name);
    if (!id_ptr) { continue; }

    if (!id_ptr->type) {
      // NOTE: IdentifierHereOrNull always returns the identifier bound to the
      // declaration, so if the type isn't specified, we need to actually verify
      // the type of it's declaration.
      ASSERT(id_ptr->decl, "");
      id_ptr->decl->verify_types();
      ASSERT(id_ptr->type, "");
    }

    if (id_ptr->type->is_function()) {
      auto fn_type = (Function *)id_ptr->type;
      if (fn_type->input == input_type) { return fn_type; }

    } else {
      UNREACHABLE;
    }
  }
  return nullptr;
}

IR::Val GetFuncReferencedIn(Scope *scope, const std::string &fn_name,
                              Function *fn_type) {
  Scope *scope_ptr = scope;
  AST::Declaration *decl;

  decl = scope->DeclReferencedOrNull(fn_name, fn_type);

  for (; scope_ptr; scope_ptr = scope_ptr->parent) {
    decl = scope_ptr->DeclHereOrNull(fn_name, fn_type);
    if (decl) { break; }
  }

  if (!decl) { return IR::Val::None(); }

  if(decl->addr == IR::Val::None()) {
    if (decl->init_val->is_function_literal()) {
      auto old_func = IR::Func::Current;
      auto old_block = IR::Block::Current;

      decl->addr = decl->init_val->EmitIR();
      decl->addr.as_func->name = Mangle(fn_type, decl->identifier, scope_ptr);

      IR::Func::Current  = old_func;
      IR::Block::Current = old_block;
    } else {
      NOT_YET;
    }
  }

  ASSERT(decl->addr != IR::Val::None(), "");
  return IR::Load(decl->addr);
}

AST::FunctionLiteral *GetFunctionLiteral(AST::Expression *expr) {
  if (expr->is_function_literal()) {
    return (AST::FunctionLiteral *)expr;

  } else if (expr->is_identifier()) {
    auto id = (AST::Identifier *)expr;
    ASSERT(id->decl->IsInferred(), "");
    return GetFunctionLiteral(id->decl->init_val);

  } else if (expr->is_declaration()) {
    auto decl = (AST::Declaration *)expr;
    ASSERT(decl->IsInferred(), "");
    return GetFunctionLiteral(decl->init_val);
  } else if (expr->is_binop()) {
    NOT_YET;
  } else {
    UNREACHABLE;
  }
}
