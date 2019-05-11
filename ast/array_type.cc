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

void ArrayType::DependentDecls(DeclDepGraph *g,
                               Declaration *d) const {
  length_->DependentDecls(g, d);
  data_type_->DependentDecls(g, d);
}

bool ArrayType::InferType(type::Type const *t, InferenceState *state) const {
  auto *a = t->if_as<type::Array>();
  return a && data_type_->InferType(a->data_type, state);
}

ir::Results ArrayType::EmitIr(Context *ctx) {
  return ir::Results{
      ir::Array(length_->EmitIr(ctx).get<int64_t>(0),
                data_type_->EmitIr(ctx).get<type::Type const *>(0))};
}

}  // namespace ast
