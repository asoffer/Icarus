#include "ast/switch.h"

#include <numeric>
#include <sstream>

#include "absl/container/flat_hash_set.h"
#include "base/util.h"
#include "ir/components.h"
#include "ir/compiled_fn.h"
#include "ir/phi.h"
#include "type/cast.h"
#include "type/pointer.h"
#include "type/type.h"

namespace ast {
std::string Switch::to_string(size_t n) const {
  std::stringstream ss;
  ss << "switch ";
  if (expr_) { ss << "(" << expr_->to_string(n) << ") {\n"; }
  for (const auto &[body, cond] : cases_) {
    ss << std::string((n + 1) * 2, ' ') << body->to_string(n + 1) << " when "
       << cond->to_string(n + 1) << "\n";
  }
  ss << std::string(2 * n, ' ') << "}";
  return ss.str();
}

}  // namespace ast
