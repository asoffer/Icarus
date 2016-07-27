#ifndef ICARUS_UNITY
#include "Type.h"
#endif

namespace cstdlib {
extern llvm::Constant *malloc();
extern llvm::Constant *free();
} // namespace cstdlib

// TODO make bytes() and alignment() platform specific
size_t Pointer::bytes() const { return 8; }
size_t Function::bytes() const { return 8; }
size_t TypeVariable::bytes() const { return 0; } // TODO should be uncallable

size_t Array::bytes() const {
  if (!fixed_length) { return 16; }
  auto size = MoveForwardToAlignment(data_type->bytes() * len, alignment());
  // TODO fix this hack that forces arrays to take of at least a byte so we
  // don't have overlapping structs. this is only an issue because we index
  // allocations by their stack location. we need to stop doing that.
  return size ? size : 1;
}

size_t Structure::bytes() const {
  assert(ast_expression);
  ast_expression->CompleteDefinition();
  size_t num_bytes = 0;
  for (auto ft : field_type) {
    num_bytes += ft->bytes();
    num_bytes = MoveForwardToAlignment(num_bytes, ft->alignment());
  }

  return MoveForwardToAlignment(num_bytes, alignment());
}

size_t Pointer::alignment() const { return 8; }
size_t Function::alignment() const { return 8; }
size_t TypeVariable::alignment() const { return 0; } // TODO should be uncallable

size_t Array::alignment() const {
  return fixed_length ? data_type->alignment() : 8;
}

size_t ParametricStructure::bytes() const { NOT_YET; }
size_t ParametricStructure::alignment() const { NOT_YET; }
size_t Tuple::bytes() const { NOT_YET; }
size_t Tuple::alignment() const { NOT_YET; }
size_t SliceType::bytes() const { NOT_YET; }
size_t SliceType::alignment() const { NOT_YET; }
size_t RangeType::bytes() const { NOT_YET; }
size_t RangeType::alignment() const { NOT_YET; }

size_t Structure::alignment() const {
  assert(ast_expression);
  ast_expression->CompleteDefinition();
  size_t alignment_val = 0;
  for (auto ft : field_type) {
    auto a = ft->alignment();
    if (alignment_val <= a) { alignment_val = a; }
  }
  return alignment_val;
}

Array::Array(Type *t)
    : init_func(nullptr), repr_func(nullptr), destroy_func(nullptr),
      data_type(t), len(0), fixed_length(false) {
  dimension =
      data_type->is_array() ? 1 + ((Array *)data_type)->dimension : 1;
  has_vars = data_type->has_vars;
}

Array::Array(Type *t, size_t l)
    : init_func(nullptr), repr_func(nullptr), destroy_func(nullptr),
      data_type(t), len(l), fixed_length(true) {
  dimension = data_type->is_array() ? 1 + ((Array *)data_type)->dimension : 1;
  has_vars = data_type->has_vars;
}

Tuple::Tuple(const std::vector<Type *> &entries) : entries(entries) {
  for (const auto &entry : entries) {
    if (has_vars) break;
    has_vars = entry->has_vars;
  }
}

Pointer::Pointer(Type *t) : pointee(t) { has_vars = pointee->has_vars; }

Function::Function(Type *in, Type *out) : input(in), output(out) {
  has_vars = input->has_vars || output->has_vars;
}

ParametricStructure::ParametricStructure(const std::string &name,
                                         AST::ParametricStructLiteral *expr)
    : ast_expression(expr), bound_name(name) {}

// Create a opaque struct
Structure::Structure(const std::string &name, AST::StructLiteral *expr)
    : ast_expression(expr), bound_name(name), field_offsets(1, 0),
      creator(nullptr), init_func(nullptr), assign_func(nullptr),
      destroy_func(nullptr) {}

Type *Structure::field(const std::string &name) const {
  auto iter = field_name_to_num.find(name);
  return (iter == field_name_to_num.end()) ? nullptr
                                           : field_type.at(iter->second);
}

size_t Structure::field_num(const std::string &name) const {
  auto iter = field_name_to_num.find(name);
  assert(iter != field_name_to_num.end());
  return iter->second;
}

void Structure::set_name(const std::string &name) {
  bound_name = name;
  if (name == "string") { String = this; }
}

void ParametricStructure::set_name(const std::string &name) {
  bound_name = name;
  assert(ast_expression);
}

std::ostream &operator<<(std::ostream &os, const Type &t) {
  return os << t.to_string();
}

Type::operator llvm::Type *() const {
  generate_llvm();
  return llvm_type;
}

Function::operator llvm::FunctionType *() const {
  generate_llvm();
  return (llvm::FunctionType *)llvm_type;
}

void Structure::insert_field(const std::string &name, Type *ty,
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

  has_vars |= ty->has_vars;
}

bool Type::is_big() const { return is_array() || is_struct() || is_function(); }
bool Type::stores_data() const {
  return this != Type_ && !is_function() && !is_type_variable();
}

std::ostream &operator<<(std::ostream &os, const Type *&t) { return os << *t; }
