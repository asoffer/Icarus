#include "ast/repeated_unop.h"

#include "ast/block_literal.h"
#include "ast/call.h"
#include "ast/function_literal.h"
#include "ast/overload_set.h"
#include "backend/eval.h"
#include "core/fn_args.h"
#include "core/scope.h"
#include "ir/compiled_fn.h"
#include "misc/context.h"

namespace ast {
using ::matcher::InheritsFrom;

std::string RepeatedUnop::to_string(size_t n) const {
  switch (op_) {
    case frontend::Operator::Jump: return "jump " + args_.to_string(n);
    case frontend::Operator::Return: return "return " + args_.to_string(n);
    case frontend::Operator::Yield: return "yield " + args_.to_string(n);
    case frontend::Operator::Print: return "print " + args_.to_string(n);
    default: { UNREACHABLE(); }
  }
}

RepeatedUnop::RepeatedUnop(TextSpan const &text_span) {
  span = args_.span = text_span;
}
}  // namespace ast
