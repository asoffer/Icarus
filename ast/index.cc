#include "ast/index.h"

#include "backend/eval.h"
#include "ir/components.h"
#include "ir/str.h"
#include "core/arch.h"
#include "misc/context.h"
#include "type/array.h"
#include "type/cast.h"
#include "type/pointer.h"
#include "type/tuple.h"

namespace ast {

std::string Index::to_string(size_t n) const {
  return lhs_->to_string(n) + "[" + rhs_->to_string(n) + "]";
}

}  // namespace ast
