#ifndef ICARUS_UNITY
#include "Type.h"
#endif

Type *Error, *Unknown, *NullPtr, *Bool, *Char, *Int, *Real, *Type_, *Uint,
    *Void, *RawPtr, *String;

namespace TypeSystem {
std::map<std::string, Type *> Literals;

// TODO is this even necessary?
Type *get(const std::string &name) {
  auto enum_ptr = Enum(name);
  if (enum_ptr) return enum_ptr;

  auto struct_ptr = Struct(name);
  assert(struct_ptr && "No struct found matching this name");
  return struct_ptr;
}

void initialize() {
  // TODO do we need to pair of strings and their types?
  Literals["bool"] = Bool = new Primitive(Primitive::TypeEnum::Bool);
  Literals["char"] = Char = new Primitive(Primitive::TypeEnum::Char);
  Literals["int"] = Int = new Primitive(Primitive::TypeEnum::Int);
  Literals["real"] = Real = new Primitive(Primitive::TypeEnum::Real);
  Literals["type"] = Type_ = new Primitive(Primitive::TypeEnum::Type);
  Literals["uint"] = Uint = new Primitive(Primitive::TypeEnum::Uint);
  Literals["void"] = Void = new Primitive(Primitive::TypeEnum::Void);

  Unknown = new Primitive(Primitive::TypeEnum::Unknown);
  NullPtr = new Primitive(Primitive::TypeEnum::NullPtr);
  Error   = new Primitive(Primitive::TypeEnum::Error);
  RawPtr  = Ptr(Char);

  RawPtr->generate_llvm();
}

static std::vector<Array *> array_types_;
static std::vector<Tuple *> tuple_types_;
static std::vector<Pointer *> pointer_types_;
static std::vector<Function *> fn_types_;
static std::map<Type *, RangeType *> ranges_;
static std::map<std::string, Enumeration *> enum_types_;
static std::map<AST::Identifier *, TypeVariable *> vars_;
static std::map<std::string, Structure *> struct_types_;
static std::map<std::string, ParametricStructure *> param_struct_types_;
static std::vector<QuantumType *> quant_;
static std::map<Array *, SliceType *> slices_;

void GenerateLLVM() {
  for (auto t : array_types_) t->generate_llvm();
  for (auto t : tuple_types_) t->generate_llvm();
  for (auto t : pointer_types_) t->generate_llvm();
  for (auto t : fn_types_) { t->generate_llvm(); }
  for (auto kv : struct_types_) kv.second->generate_llvm();
}
} // namespace TypeSystem

Array *Arr(Type *t, size_t len) {
  for (auto arr : TypeSystem::array_types_)
    if (arr->fixed_length && arr->len == len && arr->data_type == t) return arr;

  auto arr_type = new Array(t, len);
  TypeSystem::array_types_.push_back(arr_type);
  return arr_type;
}

Array *Arr(Type *t) {
  for (auto arr : TypeSystem::array_types_)
    if (!arr->fixed_length && arr->data_type == t) return arr;

  auto arr_type = new Array(t);
  TypeSystem::array_types_.push_back(arr_type);
  return arr_type;
}

Tuple *Tup(const std::vector<Type *> &types) {
  for (auto tuple_type : TypeSystem::tuple_types_) {
    if (tuple_type->entries == types) return tuple_type;
  }

  auto tuple_type = new Tuple(types);
  TypeSystem::tuple_types_.push_back(tuple_type);

  return tuple_type;
}

Pointer *Ptr(Type *t) {
  for (const auto &ptr : TypeSystem::pointer_types_) {
    if (ptr->pointee == t) return ptr;
  }

  auto ptr_type = new Pointer(t);
  TypeSystem::pointer_types_.push_back(ptr_type);
  return ptr_type;
}

Function *Func(Type *in, Type *out) {
  for (const auto &fn_type : TypeSystem::fn_types_) {
    if (fn_type->input != in) continue;
    if (fn_type->output != out) continue;
    return fn_type;
  }

  auto fn_type = new Function(in, out);
  TypeSystem::fn_types_.push_back(fn_type);
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

Enumeration *Enum(const std::string &name, const AST::EnumLiteral *e) {
  auto iter = TypeSystem::enum_types_.find(name);
  if (iter != TypeSystem::enum_types_.end()) return iter->second;

  // If you don't provide something to create it with,
  // it's just meant to be a check for existance
  // TODO merge this with Contexts
  if (e == nullptr) return nullptr;

  auto enum_type                       = new Enumeration(name, e);
  return TypeSystem::enum_types_[name] = enum_type;
}

Structure *Struct(const std::string &name, AST::StructLiteral *t) {
  auto iter = TypeSystem::struct_types_.find(name);
  if (iter != TypeSystem::struct_types_.end()) return iter->second;

  // If you don't provide something to create it with,
  // it's just meant to be a check for existance
  // TODO merge this with Contexts
  if (t == nullptr) return nullptr;

  auto struct_type = new Structure(name, t);
  t->value = Context::Value(struct_type);

  return TypeSystem::struct_types_[name] = struct_type;
}

ParametricStructure *ParamStruct(const std::string &name,
                                 AST::ParametricStructLiteral *t) {
  auto iter = TypeSystem::param_struct_types_.find(name);
  if (iter != TypeSystem::param_struct_types_.end()) return iter->second;

  // If you don't provide something to create it with,
  // it's just meant to be a check for existance
  // TODO merge this with Contexts
  if (t == nullptr) return nullptr;

  auto param_struct_type = new ParametricStructure(name, t);

  return TypeSystem::param_struct_types_[name] = param_struct_type;
}

TypeVariable *TypeVar(AST::Identifier *id, AST::Expression *test) {
  // These won't be leaked, but they aren't uniqued.
  auto iter = TypeSystem::vars_.find(id);
  if (iter != TypeSystem::vars_.end()) return iter->second;

  return TypeSystem::vars_[id] = new TypeVariable(id, test);
}

QuantumType *Quantum(const std::vector<Type *> &vec) {
  TypeSystem::quant_.push_back(new QuantumType(vec));
  return TypeSystem::quant_.back();
}

RangeType *Range(Type *t) {
  auto iter = TypeSystem::ranges_.find(t);
  if (iter != TypeSystem::ranges_.end()) return iter->second;

  return TypeSystem::ranges_[t] = new RangeType(t);
}

SliceType *Slice(Array *a) {
  auto iter = TypeSystem::slices_.find(a);
  if (iter != TypeSystem::slices_.end())return iter->second;

  return TypeSystem::slices_[a] = new SliceType(a);
}
