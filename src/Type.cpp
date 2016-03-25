#include "Type.h"
#include "TypePtr.h"
#include "AST.h"

#ifdef DEBUG
#define AT(access) .at( (access) )
#else
#define AT(access) [ (access) ]
#endif

extern llvm::Module* global_module;

namespace cstdlib {
extern llvm::Constant *malloc();
} // namespace cstdlib

namespace data {
extern llvm::Value *const_uint(size_t n);
extern llvm::Constant *str(const std::string &s);
} // namespace data

size_t Type::bytes() const {
  return (llvm_type == nullptr) ? 0 : data_layout->getTypeStoreSize(llvm_type);
}

TypePtr::TypePtr(Type *t) : get(t) {}

std::string TypePtr::to_string() const { return get->to_string(); }

bool TypePtr::is_primitive() const { return get->is_primitive(); }
bool TypePtr::is_array() const { return get->is_array(); }
bool TypePtr::is_tuple() const { return get->is_tuple(); }
bool TypePtr::is_pointer() const { return get->is_pointer(); }
bool TypePtr::is_function() const { return get->is_function(); }
bool TypePtr::is_struct() const { return get->is_struct(); }
bool TypePtr::is_enum() const { return get->is_enum(); }
bool TypePtr::is_dependent_type() const { return get->is_dependent_type(); }
bool TypePtr::is_type_variable() const { return get->is_type_variable(); }
bool TypePtr::is_parametric_struct() const {
  return get->is_parametric_struct();
}

bool TypePtr::is_big() const { return get->is_big(); }
bool TypePtr::stores_data() const { return get->stores_data(); }

TypePtr::operator bool() const { return get != nullptr; }

TypePtr::operator llvm::Type *() const {
  get->generate_llvm();
  return get->llvm_type;
}

TypePtr &TypePtr::operator=(const TypePtr &t) {
  get = t.get;
  return *this;
}

bool operator==(TypePtr lhs, TypePtr rhs) { return lhs.get == rhs.get; }

Primitive::Primitive(Primitive::TypeEnum pt) : type_(pt), repr_fn_(nullptr) {
  switch (type_) {
    case Primitive::TypeEnum::Bool:
      llvm_type = llvm::Type::getInt1Ty(llvm::getGlobalContext());   break;
    case Primitive::TypeEnum::Char:
      llvm_type = llvm::Type::getInt8Ty(llvm::getGlobalContext());   break;
    case Primitive::TypeEnum::Int:
      llvm_type = llvm::Type::getInt32Ty(llvm::getGlobalContext());  break;
    case Primitive::TypeEnum::Real:
      llvm_type = llvm::Type::getDoubleTy(llvm::getGlobalContext()); break;
    case Primitive::TypeEnum::Uint:
      llvm_type = llvm::Type::getInt32Ty(llvm::getGlobalContext());  break;
    case Primitive::TypeEnum::Void:
      llvm_type = llvm::Type::getVoidTy(llvm::getGlobalContext());   break;
    default:
      llvm_type = nullptr;
  }
}

Array::Array(TypePtr t)
    : init_fn_(nullptr), uninit_fn_(nullptr), repr_fn_(nullptr), data_type(t) {
  dimension = data_type.is_array()
                  ? 1 + static_cast<Array *>(data_type.get)->dimension
                  : 1;

  std::vector<llvm::Type *> init_args(dimension + 1, Uint);
  init_args[0] = *Ptr(this);
  has_vars     = data_type.get->has_vars;
}

Tuple::Tuple(const std::vector<TypePtr> &entries) : entries(entries) {
  for (const auto &entry : entries) {
    if (has_vars) break;
    has_vars = entry.get->has_vars;
  }
}

Pointer::Pointer(TypePtr t) : pointee(t) { has_vars = pointee.get->has_vars; }

Function::Function(TypePtr in, TypePtr out) : input(in), output(out) {
  has_vars = input.get->has_vars || output.get->has_vars;
}

Enumeration::Enumeration(const std::string& name,
    const AST::EnumLiteral* enumlit) : bound_name(name), string_data(nullptr) {
  llvm_type = Uint;

  llvm::IRBuilder<> bldr(llvm::getGlobalContext());

  // TODO Use bldr to create a global array of enum_size char ptrs

  auto num_members = enumlit->members.size();
  std::vector<llvm::Constant*> enum_str_elems(num_members, nullptr);

  size_t i = 0;
  for (const auto& idstr : enumlit->members) {
    int_values[idstr] = i;

  auto enum_str = new llvm::GlobalVariable(
      *global_module,
      /*        Type = */ llvm::ArrayType::get(Char, idstr.size() + 1),
      /*  isConstant = */ true,
      /*     Linkage = */ llvm::GlobalValue::PrivateLinkage,
      /* Initializer = */ llvm::ConstantDataArray::getString(
          llvm::getGlobalContext(), idstr, true),
      /*        Name = */ idstr);
  enum_str->setAlignment(1);
  enum_str_elems[i] = llvm::ConstantExpr::getGetElementPtr(
      llvm::ArrayType::get(Char, idstr.size() + 1), enum_str,
      {data::const_uint(0), data::const_uint(0)});

  ++i;
  }
  string_data = new llvm::GlobalVariable(
      *global_module,
      /*        Type = */ llvm::ArrayType::get(*Ptr(Char), num_members),
      /*  isConstant = */ false,
      /*     Linkage = */ llvm::GlobalValue::ExternalLinkage,
      /* Initializer = */ llvm::ConstantArray::get(
          llvm::ArrayType::get(*Ptr(Char), num_members), enum_str_elems),
      /*        Name = */ bound_name + ".name.array");
}

ParametricStructure::ParametricStructure(const std::string &name,
                                         AST::StructLiteral *expr)
    : ast_expression(expr), bound_name(name) {}

// Create a opaque struct
Structure::Structure(const std::string &name, AST::StructLiteral *expr)
    : ast_expression(expr), bound_name(name), init_fn_(nullptr),
      uninit_fn_(nullptr), print_fn_(nullptr) {}

TypePtr Structure::field(const std::string& name) const {
  return field_type AT(field_name_to_num AT(name));
}


llvm::Value* Structure::field_num(const std::string& name) const {
  auto num = field_name_to_num AT(name);
  auto t = field_type AT(num);
  assert(!t.is_function() && t != Type_ && "Invalid data field");

  return data::const_uint(field_num_to_llvm_num AT(num));
}

size_t Enumeration::get_index(const std::string& str) const {
  auto iter = int_values.find(str);
  if (iter == int_values.end()) {
    assert(false && "Invalid enumeration value");
  } else {
    return iter->second;
  }
}

llvm::Value* Enumeration::get_value(const std::string& str) const {
  return data::const_uint(get_index(str));
}

bool Array::requires_uninit() const { return true; }
bool Structure::requires_uninit() const {
  for (const auto t : field_type) {
    if (t.get->requires_uninit()) return true;
  }
  return false;
}

void Structure::set_name(const std::string& name) {
  bound_name = name;
  if (name == "string") {
    String = this;
  }
}

std::ostream& operator<<(std::ostream& os, const Type& t) {
  return os << t.to_string();
}

Type::operator llvm::Type *() const {
  generate_llvm();
  return llvm_type;
}

Function::operator llvm::FunctionType *() const {
  generate_llvm();
  return static_cast<llvm::FunctionType *>(llvm_type);
}

void Structure::insert_field(const std::string &name, TypePtr ty,
                             AST::Expression *init_val) {
  auto next_num = field_num_to_name.size();
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

  if (!ty.is_function() && ty != Type_) {
    size_t next_llvm                = field_num_to_llvm_num.size();
    field_num_to_llvm_num[next_num] = next_llvm;
  }

  // By default, init_val is nullptr;
  init_values.emplace_back(init_val);

  has_vars |= ty.get->has_vars;
}

bool Type::is_big() const { return is_array() || is_struct(); }
bool Type::stores_data() const {
  return this != Type_.get && !is_function() && !is_dependent_type() &&
         !is_type_variable();
}

std::ostream &operator<<(std::ostream &os, const TypePtr &t) {
  return os << *t.get;
}
