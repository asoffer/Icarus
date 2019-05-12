#include "ast/binop.h"

#include "ast/comma_list.h"
#include "core/fn_args.h"
#include "ast/overload_set.h"
#include "backend/eval.h"
#include "ir/components.h"
#include "ir/compiled_fn.h"
#include "ir/phi.h"
#include "misc/context.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/struct.h"
#include "type/tuple.h"
#include "type/variant.h"

namespace ir {
RegisterOr<type::Type const *> Tup(
    std::vector<RegisterOr<type::Type const *>> const &entries);
}  // namespace ir

namespace ast {

std::string Binop::to_string(size_t n) const {
  std::stringstream ss;
  ss << "(" << lhs->to_string(n) << ")";
  switch (op) {
    case frontend::Operator::Arrow: ss << " -> "; break;
    case frontend::Operator::Add: ss << " + "; break;
    case frontend::Operator::Sub: ss << " - "; break;
    case frontend::Operator::Mul: ss << " * "; break;
    case frontend::Operator::Div: ss << " / "; break;
    case frontend::Operator::Mod: ss << " % "; break;
    case frontend::Operator::Assign: ss << " = "; break;
    case frontend::Operator::OrEq: ss << " |= "; break;
    case frontend::Operator::XorEq: ss << " ^= "; break;
    case frontend::Operator::AndEq: ss << " &= "; break;
    case frontend::Operator::AddEq: ss << " += "; break;
    case frontend::Operator::SubEq: ss << " -= "; break;
    case frontend::Operator::MulEq: ss << " *= "; break;
    case frontend::Operator::DivEq: ss << " /= "; break;
    case frontend::Operator::ModEq: ss << " %= "; break;
    case frontend::Operator::When: ss << " when "; break;
    default: UNREACHABLE();
  }
  ss << "(" << rhs->to_string(n) << ")";

  return ss.str();
}

}  // namespace ast
