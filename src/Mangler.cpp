#include <string>
#include <sstream>

#include "Type.h"

std::string Mangle(const Type *t, bool prefix) {
  if (t->is_primitive()) {
    if (t == Bool) {
      return "b";
    } else if (t == Char) {
      return "c";
    } else if (t == Int) {
      return "i";
    } else if (t == Real) {
      return "r";
    } else if (t == Uint) {
      return "u";
    } else if (t == Void) {
      return "v";
    } else {
      assert(false && "Invalid type name to be mangled");
    }
  }

  std::stringstream ss;
  if (prefix) ss << "_Z";

  if (t->is_array()) {
    ss << "A0" << Mangle(static_cast<const Array *>(t)->data_type, false);

  } else if (t->is_pointer()) {
    ss << "P" << Mangle(static_cast<const Pointer *>(t)->pointee, false);

  } else if (t->is_struct()) {
    auto struct_type = static_cast<const Structure *>(t);
    ss << "S" << struct_type->bound_name.size() << struct_type->bound_name;

  } else if (t->is_function()) {
    // TODO treat as function pointer?
    ss << "F" << Mangle(static_cast<const Function *>(t)->input, false);

  } else {
    ss << t->to_string();
  }

  return ss.str();
}

std::string Mangle(const Function *f, AST::Expression *expr,
                   Scope *starting_scope) {
  auto name = expr->token();
  if (expr->is_identifier()) {
    auto id = static_cast<AST::Identifier *>(expr);

    if (id->decls.size() == 1) {
      for (const auto &tag : id->decls[0]->hashtags) {
        if (tag == "cstdlib") { return name; }
      }
    }
  }

  if ((name == "main" && f == Func(Void, Void)) || f->time() == Time::compile) {
    return name;
  }

  std::stringstream ss;
  ss << "_Z";

  // Use scopes in name mangling.
  // For now we're just concatenating pointers to the scopes which is really
  // ugly and makes the ABI impossible. But for now it uniques things correctly.
  // TODO To fix this, we need a way to assign names to scopes.
  auto scope = starting_scope ? starting_scope : expr->scope_;

  while (scope != Scope::Global) {
    ss << "X" << scope->name.size() << scope->name;
    scope = scope->parent;
  }

  ss << "F" << name.size() << name;
  ss << Mangle(f->input, false);
  return ss.str();
}
