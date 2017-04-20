#include "type.h"

#define PRIMITIVE_MACRO(GlobalName, EnumName, name)                            \
  Type *GlobalName = new Primitive(PrimType::EnumName);
#include "../config/primitive.conf"
#undef PRIMITIVE_MACRO

std::map<const char *, Type *> PrimitiveTypes{
#define PRIMITIVE_MACRO(GlobalName, EnumName, name)                            \
  { #name, GlobalName },
#include "../config/primitive.conf"
#undef PRIMITIVE_MACRO
};

static std::vector<Array *> array_types_;
static std::vector<Tuple *> tuple_types_;
static std::vector<Pointer *> pointer_types_;
static std::vector<Function *> fn_types_;
static std::map<Type *, RangeType *> ranges_;
static std::map<AST::Identifier *, TypeVariable *> vars_;
static std::map<Array *, SliceType *> slices_;
static std::map<Type *, Scope_Type *> scopes_;

Array *Arr(Type *t, size_t len) {
  for (auto arr : array_types_){
    if (arr->fixed_length && arr->len == len && arr->data_type == t) {
      return arr;
    }
  }

  auto arr_type = new Array(t, len);
  array_types_.push_back(arr_type);
  return arr_type;
}

Array *Arr(Type *t) {
  for (auto arr : array_types_){
    if (!arr->fixed_length && arr->data_type == t) { return arr; }
  }

  auto arr_type = new Array(t);
  array_types_.push_back(arr_type);
  return arr_type;
}

Tuple *Tup(const std::vector<Type *> &types) {
  for (auto tuple_type : tuple_types_) {
    if (tuple_type->entries == types) { return tuple_type; }
  }

  auto tuple_type = new Tuple(types);
  tuple_types_.push_back(tuple_type);

  return tuple_type;
}

Pointer *Ptr(Type *t) {
  for (const auto &ptr : pointer_types_) {
    if (ptr->pointee == t) return ptr;
  }

  auto ptr_type = new Pointer(t);
  pointer_types_.push_back(ptr_type);
  return ptr_type;
}

Function *Func(Type *in, Type *out) {
  for (const auto &fn_type : fn_types_) {
    if (fn_type->input != in) { continue; }
    if (fn_type->output != out) { continue; }
    return fn_type;
  }

  auto fn_type = new Function(in, out);
  fn_types_.push_back(fn_type);
  return fn_type;
}

Function *Func(std::vector<Type *> in, Type *out) {
  switch (in.size()) {
  case 0: return Func(Void, out);
  case 1: return Func(in.front(), out);
  default: return Func(Tup(in), out);
  }
}

Function *Func(Type *in, std::vector<Type *> out) {
  switch (out.size()) {
  case 0: return Func(in, Void);
  case 1: return Func(in, out.front());
  default: return Func(in, Tup(out));
  }
}

Function *Func(std::vector<Type *> in, std::vector<Type *> out) {
  switch (in.size()) {
  case 0: return Func(Void, out);
  case 1: return Func(in.front(), out);
  default: return Func(Tup(in), out);
  }
}

TypeVariable *TypeVar(AST::Identifier *id, AST::Expression *test) {
  // These won't be leaked, but they aren't uniqued.
  auto iter = vars_.find(id);
  if (iter != vars_.end()) return iter->second;

  return vars_[id] = new TypeVariable(id, test);
}

RangeType *Range(Type *t) {
  auto iter = ranges_.find(t);
  if (iter != ranges_.end()) return iter->second;

  return ranges_[t] = new RangeType(t);
}

SliceType *Slice(Array *a) {
  auto iter = slices_.find(a);
  if (iter != slices_.end()) return iter->second;

  return slices_[a] = new SliceType(a);
}

Scope_Type *ScopeType(Type *t) {
  auto iter = scopes_.find(t);
  if (iter != scopes_.end()) return iter->second;
  return scopes_[t] = new Scope_Type(t);
}
