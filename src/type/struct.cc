#include "type.h"

#include "../ast/ast.h"
#include "../context.h"
#include "../ir/func.h"

std::vector<IR::Val> Evaluate(AST::Expression *expr);

const Struct::Field *Struct::field(const std::string &name) const {
  auto iter = field_indices_.find(name);
  if (iter == field_indices_.end()) { return nullptr; }
  return &fields_[iter->second];
}
