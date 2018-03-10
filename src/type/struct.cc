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

bool operator==(const Struct &lhs, const Struct &rhs) {
  if (lhs.fields_.size() != rhs.fields_.size()) { return false; }
  for (size_t i = 0; i < lhs.fields_.size(); ++i) {
    if (lhs.fields_[i].name != rhs.fields_[i].name) { return false; }
    // TODO won't work for struct subfields? intern it!
    if (lhs.fields_[i].type != rhs.fields_[i].type) { return false; }
    // TODO default values!
  }
  return true;
}
