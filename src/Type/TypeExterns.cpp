#ifndef ICARUS_UNITY
#include "Type/Type.h"
#endif

Type *Err, *Unknown, *NullPtr, *Bool, *Char, *Int, *Real, *Type_, *Uint, *Void,
    *RawPtr, *String, *Uint16, *Uint32;

namespace TypeSystem {
std::map<const char *, Type *> Literals;

void Initialize() {
  // TODO do we need to pair of strings and their types?
  Literals["bool"] = Bool = new Primitive(PrimType::Bool);
  Literals["char"] = Char = new Primitive(PrimType::Char);
  Literals["int"] = Int = new Primitive(PrimType::Int);
  Literals["real"] = Real = new Primitive(PrimType::Real);
  Literals["type"] = Type_ = new Primitive(PrimType::Type);
  Literals["uint"] = Uint = new Primitive(PrimType::Uint);
  Literals["void"] = Void = new Primitive(PrimType::Void);

  Uint16  = new Primitive(PrimType::Uint16);
  Uint32  = new Primitive(PrimType::Uint32);
  Unknown = new Primitive(PrimType::Unknown);
  NullPtr = new Primitive(PrimType::NullPtr);
  Err     = new Primitive(PrimType::Err);
  RawPtr  = Ptr(Char);

  RawPtr->generate_llvm();
}

static std::vector<Array *> array_types_;
static std::vector<Tuple *> tuple_types_;
static std::vector<Pointer *> pointer_types_;
static std::vector<Function *> fn_types_;
static std::map<Type *, RangeType *> ranges_;
static std::map<AST::Identifier *, TypeVariable *> vars_;
static std::map<AST::StructLiteral *, Structure *> struct_types_;
static std::map<Array *, SliceType *> slices_;

// TODO Not sure this is necessary
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

Structure *Struct(const std::string &name, AST::StructLiteral *t) {
  auto iter = TypeSystem::struct_types_.find(t);
  if (iter != TypeSystem::struct_types_.end()) return iter->second;

  // If you don't provide something to create it with,
  // it's just meant to be a check for existance
  // TODO merge this with Contexts
  if (t == nullptr) return nullptr;

  auto struct_type = new Structure(name, t);
  t->value = IR::Value(struct_type);

  return TypeSystem::struct_types_[t] = struct_type;
}

TypeVariable *TypeVar(AST::Identifier *id, AST::Expression *test) {
  // These won't be leaked, but they aren't uniqued.
  auto iter = TypeSystem::vars_.find(id);
  if (iter != TypeSystem::vars_.end()) return iter->second;

  return TypeSystem::vars_[id] = new TypeVariable(id, test);
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
