#include "ast/declaration.h"

#include <sstream>
#include "ast/function_literal.h"
#include "backend/eval.h"
#include "ir/compiled_fn.h"
#include "misc/module.h"
#include "type/cast.h"
#include "type/typed_value.h"

namespace ast {

std::string Declaration::to_string(size_t n) const {
  std::stringstream ss;
  ss << id_;
  if (type_expr) {
    ss << (const_ ? " :: " : ": ") << type_expr->to_string(n);
    if (init_val) { ss << " = " << init_val->to_string(n); }
  } else {
    if (init_val) {
      ss << (const_ ? " ::= " : " := ") << init_val->to_string(n);
    }
  }

  return ss.str();
}

}  // namespace ast
