#ifndef ICARUS_UNITY
#include "Type.h"
#endif

extern IR::Value Evaluate(AST::Expression *expr);

Struct::Struct(const std::string &name)
    : type_scope(new Scope), bound_name(name), field_offsets(1, 0),
      creator(nullptr), init_func(nullptr), assign_func(nullptr),
      destroy_func(nullptr), completed_(false) {}

void Struct::CompleteDefinition() {
  if (completed_) { return; }
  if (!field_num_to_name.empty()) { return; }

  for (size_t i = 0; i < decls.size(); ++i) {
    decls[i]->verify_types();

    Type *decl_type;
    if (decls[i]->type_expr) {
      if (decls[i]->type_expr->type == Err ||
          decls[i]->type_expr->type == Void ||
          decls[i]->type_expr->type->is_parametric_struct()) {
        decl_type = Err;
      } else {
        decl_type = Evaluate(decls[i]->type_expr).as_val->GetType();
      }
    } else {
      decl_type = decls[i]->init_val->type;
    }

    insert_field(decls[i]->identifier->token, decl_type, decls[i]->init_val);
  }
  completed_ = true;
}

bool Struct::private_has_vars() {
  CompleteDefinition();
  for (auto ft: field_type) {
    if (ft->has_vars()) { return true; }
  }
  return false;
}

size_t Struct::bytes() const {
  const_cast<Struct *>(this)->CompleteDefinition();
  size_t num_bytes = 0;
  for (auto ft : field_type) {
    num_bytes += ft->bytes();
    num_bytes = MoveForwardToAlignment(num_bytes, ft->alignment());
  }

  return MoveForwardToAlignment(num_bytes, alignment());
}

size_t Struct::alignment() const {
  const_cast<Struct *>(this)->CompleteDefinition();
  size_t alignment_val = 0;
  for (auto ft : field_type) {
    auto a = ft->alignment();
    if (alignment_val <= a) { alignment_val = a; }
  }
  return alignment_val;
}

Type *Struct::field(const std::string &name) const {
  auto iter = field_name_to_num.find(name);
  return (iter == field_name_to_num.end()) ? nullptr
                                           : field_type.at(iter->second);
}

size_t Struct::field_num(const std::string &name) const {
  auto iter = field_name_to_num.find(name);
  assert(iter != field_name_to_num.end());
  return iter->second;
}

void Struct::insert_field(const std::string &name, Type *ty,
                             AST::Expression *init_val) {

  // TODO what if ty->alignment() == 0?
  size_t last_field_offset = field_offsets.back();
  size_t next_offset = MoveForwardToAlignment(
      last_field_offset + (field_type.empty() ? 0 : field_type.back()->bytes()),
      ty->alignment());
  field_offsets.push_back(next_offset);


  auto next_num           = field_num_to_name.size();
  field_name_to_num[name] = next_num;
  field_num_to_name.push_back(name);
  field_type.push_back(ty);

  { // Check sizes align
    size_t size1 = field_name_to_num.size();
    size_t size2 = field_num_to_name.size();
    size_t size3 = field_type.size();
    assert(size1 == size2 && size2 == size3 &&
           "Size mismatch in struct database");
  }

  // By default, init_val is nullptr;
  init_values.emplace_back(init_val);
}

std::string Struct::to_string() const { return bound_name; }

Struct Struct::Anon(const std::set<AST::Declaration *> &declarations) {
  static int counter = 0;
  Struct result("anon.struct." + std::to_string(counter++));
  for (auto decl : declarations) {
    result.insert_field(decl->identifier->token, decl->type, nullptr);
  }
  result.CompleteDefinition();
  return result;
}
