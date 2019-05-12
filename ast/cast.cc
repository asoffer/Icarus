#include "ast/cast.h"

#include "core/fn_args.h"
#include "ast/overload_set.h"
#include "backend/eval.h"
#include "ir/cmd.h"
#include "misc/context.h"
#include "type/cast.h"
#include "type/tuple.h"

namespace ast {

std::string Cast::to_string(size_t n) const {
  return "(" + expr_->to_string(n) + ") as (" + type_->to_string(n) + ")";
}

}  // namespace ast
