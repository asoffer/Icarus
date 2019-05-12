#include "ast/chainop.h"

#include <numeric>

#include "core/fn_args.h"
#include "ast/overload_set.h"
#include "ir/compiled_fn.h"
#include "ir/phi.h"
#include "misc/context.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/pointer.h"
#include "type/struct.h"
#include "type/tuple.h"

namespace ast {

std::string ChainOp::to_string(size_t n) const {
  std::stringstream ss;
  ss << "(";
  for (size_t i = 0; i < ops.size(); ++i) {
    ss << exprs[i]->to_string(n);
    switch (ops[i]) {
      case frontend::Operator::Or: ss << " | "; break;
      case frontend::Operator::Xor: ss << " ^ "; break;
      case frontend::Operator::And: ss << " & "; break;
      case frontend::Operator::Lt: ss << " < "; break;
      case frontend::Operator::Le: ss << " <= "; break;
      case frontend::Operator::Eq: ss << " == "; break;
      case frontend::Operator::Ne: ss << " != "; break;
      case frontend::Operator::Ge: ss << " >= "; break;
      case frontend::Operator::Gt: ss << " > "; break;
      default: UNREACHABLE();
    }
  }
  ss << exprs.back()->to_string(n) << ")";
  return ss.str();
}

}  // namespace ast
