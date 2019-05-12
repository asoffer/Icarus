#include "ast/import.h"

#include <filesystem>

#include "ast/overload_set.h"
#include "backend/eval.h"
#include "base/guarded.h"
#include "misc/context.h"
#include "type/primitive.h"

namespace ast {
std::string Import::to_string(size_t n) const {
  return "import " + operand_->to_string(n);
}

}  // namespace ast
