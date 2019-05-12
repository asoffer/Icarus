#include "ast/array_type.h"

#include <sstream>
#include "ir/cmd.h"

namespace ast {
std::string ArrayType::to_string(size_t n) const {
  ASSERT(length_ != nullptr);
  std::stringstream ss;
  ss << "[" << length_->to_string(n) << "; " << data_type_->to_string(n) << "]";
  return ss.str();
}

}  // namespace ast
