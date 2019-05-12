#include "ast/call.h"

#include <sstream>

#include "absl/container/flat_hash_map.h"
#include "ast/block_literal.h"
#include "ast/builtin_fn.h"
#include "ast/dispatch_table.h"
#include "ast/function_literal.h"
#include "ast/struct_literal.h"
#include "ast/unop.h"
#include "backend/eval.h"
#include "core/fn_params.h"
#include "core/scope.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "ir/compiled_fn.h"
#include "ir/phi.h"
#include "type/array.h"
#include "type/function.h"
#include "type/generic_struct.h"
#include "type/pointer.h"
#include "type/tuple.h"
#include "type/variant.h"

namespace ast {
std::string Call::to_string(size_t n) const {
  std::stringstream ss;
  ss << fn_->to_string(n) << "(";
  bool seen_one = false;
  args_.ApplyWithIndex(
      [&](auto &&index, std::unique_ptr<Expression> const &expr) {
        ss << (seen_one ? ", " : "");
        if constexpr (!std::is_same_v<std::decay_t<decltype(index)>, size_t>) {
          ss << index << " = ";
        }
        ss << expr->to_string(n);
        seen_one = true;
      });
  ss << ")";
  return ss.str();
}

}  // namespace ast
