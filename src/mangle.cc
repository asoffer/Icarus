#include "type/type.h"
#include "scope.h"
#include "ast/ast.h"

std::string Mangle(const Type *t, bool prefix) {
  if (t->is<Primitive>()) {
    if (t == Bool) { return "b"; }
    if (t == Char) { return "c"; }
    if (t == Int) { return "i"; }
    if (t == Real) { return "r"; }
    if (t == Uint) { return "u"; }
    if (t == Void) { return "v"; }
    if (t == Type_) { return "t"; }
    if (t == String) { return "s"; }
    UNREACHABLE(*t);
  }

  std::stringstream ss;
  if (prefix) ss << "_Z";

  if (t->is<Array>()) {
    auto array_type = (const Array *)t;
    ss << 'A' << (array_type->fixed_length ? array_type->len : 0)
       << Mangle(array_type->data_type, false);

  } else if (t->is<Pointer>()) {
    ss << "P" << Mangle(((const Pointer *)t)->pointee, false);

  } else if (t->is<Struct>()) {
    auto struct_type = (const Struct *)t;
    ss << "S" << struct_type->bound_name.size() << struct_type->bound_name;

  } else if (t->is<Function>()) {
    // TODO treat as function pointer?
    ss << "F" << t->as<Function>().input.size();
    for (Type *in : t->as<Function>().input) { ss << Mangle(in, false); }
  } else {
    ss << t->to_string();
  }

  return ss.str();
}

// TODO Mangle could just take a declaration and the type could be pulled out.
std::string Mangle(const Function *f, AST::Expression *expr, Scope *) {
  std::string name =
      expr->is<AST::Identifier>() ? ((AST::Identifier *)expr)->token : "";

  std::stringstream ss;
  ss << "_Z";

  ss << "F" << name.size() << name << f->input.size();
  for (Type *in : f->input) { ss << Mangle(in, false); }
  return ss.str();
}
