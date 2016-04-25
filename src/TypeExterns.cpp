#include "Type.h"

TypePtr Error, Unknown, NullPtr, Bool, Char, Int, Real, Type_, Uint, Void,
    RawPtr, String;

namespace TypeSystem {
std::map<std::string, TypePtr> Literals;
std::map<Language::Operator, std::vector<Function *>> operator_table;

// TODO is this even necessary?
TypePtr get(const std::string &name) {
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
  RawPtr.get->generate_llvm();

  operator_table[Language::Operator::Arrow] = {Func({Type_, Type_}, Type_)};

  operator_table[Language::Operator::OrEq]  = {Func({Bool, Bool}, Void)};
  operator_table[Language::Operator::XorEq] = {Func({Bool, Bool}, Void)};
  operator_table[Language::Operator::AndEq] = {Func({Bool, Bool}, Void)};

  operator_table[Language::Operator::AddEq] = {Func({Int, Int}, Void),
                                               Func({Uint, Uint}, Void),
                                               Func({Real, Real}, Void)};

  operator_table[Language::Operator::SubEq] = {Func({Int, Int}, Void),
                                               Func({Uint, Uint}, Void),
                                               Func({Real, Real}, Void)};

  operator_table[Language::Operator::MulEq] = {Func({Int, Int}, Void),
                                               Func({Uint, Uint}, Void),
                                               Func({Real, Real}, Void)};

  operator_table[Language::Operator::DivEq] = {Func({Int, Int}, Void),
                                               Func({Uint, Uint}, Void),
                                               Func({Real, Real}, Void)};

  operator_table[Language::Operator::ModEq] = {Func({Int, Int}, Void),
                                               Func({Uint, Uint}, Void),
                                               Func({Real, Real}, Void)};

  operator_table[Language::Operator::Or]  = {Func({Bool, Bool}, Bool)};
  operator_table[Language::Operator::Xor] = {Func({Bool, Bool}, Bool)};
  operator_table[Language::Operator::And] = {Func({Bool, Bool}, Bool)};

  operator_table[Language::Operator::LT] = {Func({Int, Int}, Bool),
                                            Func({Uint, Uint}, Bool),
                                            Func({Real, Real}, Bool)};

  operator_table[Language::Operator::LE] = {Func({Int, Int}, Bool),
                                            Func({Uint, Uint}, Bool),
                                            Func({Real, Real}, Bool)};

  operator_table[Language::Operator::EQ] = {
      Func({Bool, Bool}, Bool), Func({Char, Char}, Bool),
      Func({Int, Int}, Bool),   Func({Uint, Uint}, Bool),
      Func({Real, Real}, Bool), Func({Type_, Type_}, Bool)};

  operator_table[Language::Operator::NE] = {
      Func({Bool, Bool}, Bool), Func({Char, Char}, Bool),
      Func({Int, Int}, Bool),   Func({Uint, Uint}, Bool),
      Func({Real, Real}, Bool), Func({Type_, Type_}, Bool)};

  operator_table[Language::Operator::GE] = {Func({Int, Int}, Bool),
                                            Func({Uint, Uint}, Bool),
                                            Func({Real, Real}, Bool)};

  operator_table[Language::Operator::GT] = {Func({Int, Int}, Bool),
                                            Func({Uint, Uint}, Bool),
                                            Func({Real, Real}, Bool)};

  operator_table[Language::Operator::Add] = {Func({Int, Int}, Int),
                                             Func({Uint, Uint}, Uint),
                                             Func({Real, Real}, Real)};

}

// TODO make this lookup better
TypePtr get_operator(Language::Operator op, TypePtr signature) {
  auto operator_set = operator_table[op];
  for (const auto &fn : operator_set) {
    if (signature == fn->input) { return fn->output; }
  }
  return nullptr;
}

static std::vector<Array *> array_types_;
static std::vector<Tuple *> tuple_types_;
static std::vector<Pointer *> pointer_types_;
static std::vector<Function *> fn_types_;
static std::map<TypePtr, RangeType *> ranges_;
static std::map<std::string, Enumeration *> enum_types_;
static std::vector<DependentType *> dep_types_;
static std::map<AST::Identifier *, TypeVariable *> vars_;
static std::map<std::string, Structure *> struct_types_;
static std::map<std::string, ParametricStructure *> param_struct_types_;
static std::vector<QuantumType *> quant_;

void GenerateLLVM() {
  for (auto t : array_types_) t->generate_llvm();
  for (auto t : tuple_types_) t->generate_llvm();
  for (auto t : pointer_types_) t->generate_llvm();
  for (auto t : fn_types_) t->generate_llvm();
  for (auto kv : struct_types_) kv.second->generate_llvm();
  for (auto kv : struct_types_)
    kv.second->ast_expression->build_llvm_internals();
}

} // namespace TypeSystem

Array *Arr(TypePtr t) {
  for (auto arr : TypeSystem::array_types_)
    if (arr->data_type == t) return arr;

  auto arr_type = new Array(t);
  TypeSystem::array_types_.push_back(arr_type);
  return arr_type;
}

Tuple *Tup(const std::vector<TypePtr> &types) {
  for (auto tuple_type : TypeSystem::tuple_types_) {
    if (tuple_type->entries == types) return tuple_type;
  }

  auto tuple_type = new Tuple(types);
  TypeSystem::tuple_types_.push_back(tuple_type);

  return tuple_type;
}

Pointer *Ptr(TypePtr t) {
  for (const auto &ptr : TypeSystem::pointer_types_) {
    if (ptr->pointee == t) return ptr;
  }

  auto ptr_type = new Pointer(t);
  TypeSystem::pointer_types_.push_back(ptr_type);
  return ptr_type;
}

Function *Func(TypePtr in, TypePtr out) {
  for (const auto &fn_type : TypeSystem::fn_types_) {
    if (fn_type->input != in) continue;
    if (fn_type->output != out) continue;
    return fn_type;
  }

  auto fn_type = new Function(in, out);
  TypeSystem::fn_types_.push_back(fn_type);
  return fn_type;
}

Function *Func(std::vector<TypePtr> in, TypePtr out) {
  switch (in.size()) {
  case 0: return Func(Void, out);
  case 1: return Func(in.front(), out);
  default: return Func(Tup(in), out);
  }
}

Function *Func(TypePtr in, std::vector<TypePtr> out) {
  switch (out.size()) {
  case 0: return Func(in, Void);
  case 1: return Func(in, out.front());
  default: return Func(in, Tup(out));
  }
}

Function *Func(std::vector<TypePtr> in, std::vector<TypePtr> out) {
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
  t->type_value = struct_type;

  return TypeSystem::struct_types_[name] = struct_type;
}

ParametricStructure *ParamStruct(const std::string &name,
                                 AST::StructLiteral *t) {
  auto iter = TypeSystem::param_struct_types_.find(name);
  if (iter != TypeSystem::param_struct_types_.end()) return iter->second;

  // If you don't provide something to create it with,
  // it's just meant to be a check for existance
  // TODO merge this with Contexts
  if (t == nullptr) return nullptr;

  auto param_struct_type = new ParametricStructure(name, t);

  return TypeSystem::param_struct_types_[name] = param_struct_type;
}

// TODO take in a vector of context values instead
DependentType *DepType(std::function<TypePtr(TypePtr)> fn) {
  // These won't be leaked, but they aren't uniqued.
  auto dep_type = new DependentType(fn);
  TypeSystem::dep_types_.push_back(dep_type);
  return dep_type;
}

TypeVariable *TypeVar(AST::Identifier *id, AST::Expression *test) {
  // These won't be leaked, but they aren't uniqued.
  auto iter = TypeSystem::vars_.find(id);
  if (iter != TypeSystem::vars_.end()) return iter->second;

  return TypeSystem::vars_[id] = new TypeVariable(id, test);
}

QuantumType *Quantum(const std::vector<TypePtr>& vec) {
  TypeSystem::quant_.push_back(new QuantumType(vec));
  return TypeSystem::quant_.back();
}

RangeType *Range(TypePtr t) {
  // These won't be leaked, but they aren't uniqued.
  auto iter = TypeSystem::ranges_.find(t);
  if (iter != TypeSystem::ranges_.end()) return iter->second;

  return TypeSystem::ranges_[t] = new RangeType(t);
}

