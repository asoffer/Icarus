#include "Type.h"
#include "AST.h"

#ifdef DEBUG
#define AT(access) .at( (access) )
#else
#define AT(access) [ (access) ]
#endif

extern llvm::Module* global_module;

namespace cstdlib {
  extern llvm::Constant* malloc();
}  // namespace cstdlib

namespace data {
  extern llvm::Value* const_uint(size_t n);
  extern llvm::Constant* str(const std::string& s);
}  // namespace data


size_t Type::bytes() const {
  return (llvm_type == nullptr)
    ? 0 : data_layout->getTypeStoreSize(llvm_type);
}


// CONSTRUCTORS
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

Array::Array(Type* t) :
  init_fn_(nullptr), uninit_fn_(nullptr), repr_fn_(nullptr), data_type(t)
{
  llvm_type = llvm::PointerType::getUnqual(t->llvm_type);
  dimension = data_type->is_array()
                  ? 1 + static_cast<Array *>(data_type)->dimension
                  : 1;

  std::vector<llvm::Type *> init_args(dimension + 1, *Uint);
  init_args[0] = *Ptr(this);
  has_vars     = data_type->has_vars;
}

Tuple::Tuple(const std::vector<Type *> &entries) : entries(entries) {
  for (const auto &entry : entries) {
    if (has_vars) break;
    has_vars = entry->has_vars;
  }
}

Pointer::Pointer(Type *t) : pointee(t) {
  llvm_type = llvm::PointerType::getUnqual(*pointee);
  has_vars  = pointee->has_vars;
}

Function::Function(Type* in, Type* out) : input(in), output(out) {
  std::vector<llvm::Type *> llvm_in;
  llvm::Type *llvm_out = *Void;

  has_vars = input->has_vars || output->has_vars;

  if (input->is_tuple()) {
    auto in_tup = static_cast<Tuple *>(input);
    for (auto t : in_tup->entries) {
      if (!t->add_llvm_input(llvm_in)) {
        llvm_type = nullptr;
        return;
      }
    }
  } else {
    if (!input->add_llvm_input(llvm_in)) {
      llvm_type = nullptr;
      return;
    }
  }

  if (output->is_tuple()) {
    auto out_tup = static_cast<Tuple *>(output);
    for (auto t : out_tup->entries) {
      if (!Ptr(t)->add_llvm_input(llvm_in)) {
        llvm_type = nullptr;
        return;
      }
    }
  } else if (output->is_enum() || output->is_array() ||
             output->is_primitive()) {
    llvm_out = *output;
    if (llvm_out == nullptr) {
      llvm_type = nullptr;
      return;
    }

  } else {
    if (!Ptr(output)->add_llvm_input(llvm_in)) {
      llvm_type = nullptr;
      return;
    }
  }

  llvm_type = llvm::FunctionType::get(llvm_out, llvm_in, false);
}

Enumeration::Enumeration(const std::string& name,
    const AST::EnumLiteral* enumlit) : bound_name(name), string_data(nullptr) {
  llvm_type = *Uint;

  llvm::IRBuilder<> bldr(llvm::getGlobalContext());

  // TODO Use bldr to create a global array of enum_size char ptrs

  auto num_members = enumlit->members.size();
  std::vector<llvm::Constant*> enum_str_elems(num_members, nullptr);

  size_t i = 0;
  for (const auto& idstr : enumlit->members) {
    int_values[idstr] = data::const_uint(i);

  auto enum_str = new llvm::GlobalVariable(
      *global_module,
      /*        Type = */ llvm::ArrayType::get(*Char, idstr.size() + 1),
      /*  isConstant = */ true,
      /*     Linkage = */ llvm::GlobalValue::PrivateLinkage,
      /* Initializer = */ llvm::ConstantDataArray::getString(
          llvm::getGlobalContext(), idstr, true),
      /*        Name = */ idstr);
  enum_str->setAlignment(1);
  enum_str_elems[i] = llvm::ConstantExpr::getGetElementPtr(
      llvm::ArrayType::get(*Char, idstr.size() + 1), enum_str,
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

Structure::Structure(const std::string& name, AST::TypeLiteral* expr) :
  ast_expression(expr), bound_name(name), init_fn_(nullptr),
  uninit_fn_(nullptr), print_fn_(nullptr)
{
  auto struct_type = llvm::StructType::create(global_module->getContext());
  struct_type->setName(bound_name);
  llvm_type = struct_type;

  for (const auto& field : fields) {
    if (has_vars) break;
    has_vars = field.second->has_vars;
  }
}

Type* Structure::field(const std::string& name) const {
  auto iter = fields.cbegin();
  while (iter != fields.end()) {
    if (iter->first == name) {
      return iter->second;
    }
    ++iter;
  }
  return nullptr;
}

llvm::Value* Structure::field_num(const std::string& name) const {
  size_t i = 0;
  auto iter = fields.cbegin();
  while (iter != fields.cend()) {
    if (iter->first == name) {
      return data::const_uint(i);
    }
    ++iter; ++i;
  }
  return nullptr;
}

llvm::Value* Enumeration::get_value(const std::string& str) const {
  auto iter = int_values.find(str);
  return (iter == int_values.end()) ? nullptr : iter->second;
}

bool Array::requires_uninit() const { return true; }
bool Structure::requires_uninit() const {
  for (const auto field : fields) {
    if (field.second->requires_uninit()) {
      return true;
    }
  }
  return false;
}

void Structure::set_name(const std::string& name) {
  bound_name = name;
  static_cast<llvm::StructType*>(llvm_type)->setName(bound_name);
  if (name == "string") {
    String = this;
  }
}

std::ostream& operator<<(std::ostream& os, const Type& t) {
  return os << t.to_string();
}
