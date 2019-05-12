#include "ast/unop.h"

#include "ast/overload_set.h"
#include "ast/terminal.h"
#include "backend/eval.h"
#include "core/fn_args.h"
#include "ir/components.h"
#include "ir/compiled_fn.h"
#include "misc/context.h"

namespace ast {

std::string Unop::to_string(size_t n) const {
  if (op == frontend::Operator::TypeOf) {
    return "(" + operand->to_string(n) + "):?";
  }

  std::stringstream ss;
  switch (op) {
    case frontend::Operator::Which: ss << "which "; break;
    case frontend::Operator::Mul: ss << "*"; break;
    case frontend::Operator::And: ss << "&"; break;
    case frontend::Operator::Sub: ss << "-"; break;
    case frontend::Operator::Not: ss << "!"; break;
    case frontend::Operator::At: ss << "@"; break;
    case frontend::Operator::Eval: ss << "$"; break;
    case frontend::Operator::Needs: ss << "needs "; break;
    case frontend::Operator::Ensure: ss << "ensure "; break;
    case frontend::Operator::Expand: ss << "<< "; break;
    case frontend::Operator::BufPtr: ss << "[*]"; break;
    case frontend::Operator::Copy: ss << "copy "; break;
    case frontend::Operator::Move: ss << "move "; break;
    default: { UNREACHABLE(); }
  }

  ss << operand->to_string(n);
  return ss.str();
}

}  // namespace ast
