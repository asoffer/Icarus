#include "type.h"

#include "../ast/ast.h"
#include "../ir/func.h"

std::vector<IR::Val> Evaluate(AST::Expression *expr);

void Struct::CompleteDefinition() {
  if (completed_) { return; }
  if (!field_num_to_name.empty()) { return; }

  for (size_t i = 0; i < decls.size(); ++i) {
    decls[i]->Validate();

    Type *decl_type;
    if (decls[i]->type_expr) {
      if (decls[i]->type_expr->type == Err ||
          decls[i]->type_expr->type == Void) {
        decl_type = Err;
      } else {
        decl_type =
            std::get<Type *>(Evaluate(decls[i]->type_expr.get())[0].value);
      }
    } else {
      decl_type = decls[i]->init_val->type;
    }

    insert_field(decls[i]->identifier->token, decl_type,
                 decls[i]->init_val.get());
  }
  completed_ = true;
}

Type *Struct::field(const std::string &name) const {
  auto iter = field_name_to_num.find(name);
  return (iter == field_name_to_num.end()) ? nullptr
                                           : field_type AT(iter->second);
}

size_t Struct::field_num(const std::string &name) const {
  auto iter = field_name_to_num.find(name);
  ASSERT(iter != field_name_to_num.end(), "");
  return iter->second;
}

void Struct::insert_field(const std::string &name, Type *ty,
                             AST::Expression *init_val) {
  auto next_num           = field_num_to_name.size();
  field_name_to_num[name] = next_num;
  field_num_to_name.push_back(name);
  field_type.push_back(ty);

  ASSERT_EQ(field_name_to_num.size(), field_num_to_name.size());
  ASSERT_EQ(field_num_to_name.size(), field_type.size());

  // By default, init_val is nullptr;
  init_values.emplace_back(init_val);
}
