#include "ast/scope_node.h"


#include <sstream>
#include "ast/access.h"
#include "ast/block_literal.h"
#include "ast/block_node.h"
#include "core/fn_args.h"
#include "ast/function_literal.h"
#include "ast/identifier.h"
#include "ast/scope_literal.h"
#include "backend/eval.h"
#include "base/util.h"
#include "ir/components.h"
#include "ir/compiled_fn.h"
#include "misc/context.h"
#include "core/scope.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/type.h"

namespace ast {
using ::matcher::InheritsFrom;

std::string ScopeNode::to_string(size_t n) const {
  std::stringstream ss;
  ss << name_->to_string(n) << " ";
  if (!args_.empty()) { ss << "(" << args_.to_string() << ") "; }
  for (auto const &block : blocks_) { ss << block.to_string(n); }
  return ss.str();
}

}  // namespace ast
