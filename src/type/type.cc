#include "type.h"
#include "../scope.h"

std::string Primitive::to_string() const {
  const char *str;
  switch (type_) {
  case PrimType::Err: str     = "Err"; break;
  case PrimType::Unknown: str = "???"; break;
  case PrimType::Bool: str    = "bool"; break;
  case PrimType::Char: str    = "char"; break;
  case PrimType::Code: str    = "code"; break;
  case PrimType::Int: str     = "int"; break;
  case PrimType::Real: str    = "real"; break;
  case PrimType::Type: str    = "type"; break;
  case PrimType::Uint: str    = "uint"; break;
  case PrimType::Void: str    = "void"; break;
  case PrimType::NullPtr: str = "null"; break;
  case PrimType::String: str  = "string"; break;
  }
  return str;
}

Array::Array(Type *t) : data_type(t), len(0), fixed_length(false) {
  dimension = data_type->is<Array>() ? 1 + ((Array *)data_type)->dimension : 1;
}

Array::Array(Type *t, size_t l) : data_type(t), len(l), fixed_length(true) {
  dimension = data_type->is<Array>() ? 1 + ((Array *)data_type)->dimension : 1;
}

Tuple::Tuple(std::vector<Type *> entries) : entries(std::move(entries)) {}

Pointer::Pointer(Type *t) : pointee(t) {}

Function::Function(Type *in, Type *out) : input(in), output(out) {}

std::ostream &operator<<(std::ostream &os, const Type &t) {
  return os << t.to_string();
}

std::ostream &operator<<(std::ostream &os, const Type *&t) { return os << *t; }

// TODO mess around to see the performance characteristics. Maybe a flat map is
// better?
template <typename Key, typename Val>
using TypeContainer = std::unordered_map<Key, Val>;

static TypeContainer<Type*, std::unordered_map<size_t, Array>> fixed_arrays_;
Array *Arr(Type *t, size_t len) {
  return &fixed_arrays_[t].emplace(len, Array(t, len)).first->second;
}
static TypeContainer<Type*, Array> arrays_;
Array *Arr(Type *t) { return &arrays_.emplace(t, Array(t)).first->second; }

static std::map<std::vector<Type*>, Tuple> tuples_;
Type *Tup(std::vector<Type *> types) {
  if (types.empty()) { return Void; }
  if (types.size() == 1) { return types.front(); }
  return &tuples_.emplace(types, Tuple(types)).first->second;
}

static TypeContainer<Type*, Pointer> pointers_;
Pointer *Ptr(Type *t) {
  return &pointers_.emplace(t, Pointer(t)).first->second;
}

static TypeContainer<Type *, TypeContainer<Type *, Function>> funcs_;
Function *Func(Type *in, Type *out) {
  return &funcs_[in].emplace(out, Function(in, out)).first->second;
}

Function *Func(std::vector<Type *> in, Type *out) {
  return Func(Tup(std::move(in)), out);
}
Function *Func(Type *in, std::vector<Type *> out) {
  return Func(in, Tup(std::move(out)));
}
Function *Func(std::vector<Type *> in, std::vector<Type *> out) {
  return Func(Tup(std::move(in)), Tup(std::move(out)));
}

static TypeContainer<Type*, RangeType> ranges_;
RangeType *Range(Type *t) {
  return &ranges_.emplace(t, RangeType(t)).first->second;
}

static TypeContainer<Array*, SliceType> slices_;
SliceType *Slice(Array *a) {
  return &slices_.emplace(a, SliceType(a)).first->second;
}

static TypeContainer<Type*, Scope_Type> scopes_;
Scope_Type *ScopeType(Type *t) {
  return &scopes_.emplace(t, Scope_Type(t)).first->second;
}
