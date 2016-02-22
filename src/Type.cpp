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
  return (llvm_type_ == nullptr)
    ? 0 : data_layout->getTypeStoreSize(llvm_type_);
}


// CONSTRUCTORS
Primitive::Primitive(Primitive::TypeEnum pt) : type_(pt), repr_fn_(nullptr) {
  switch (type_) {
    case Primitive::TypeEnum::Bool:
      llvm_type_ = llvm::Type::getInt1Ty(llvm::getGlobalContext());   break;
    case Primitive::TypeEnum::Char:
      llvm_type_ = llvm::Type::getInt8Ty(llvm::getGlobalContext());   break;
    case Primitive::TypeEnum::Int:
      llvm_type_ = llvm::Type::getInt32Ty(llvm::getGlobalContext());  break;
    case Primitive::TypeEnum::Real:
      llvm_type_ = llvm::Type::getDoubleTy(llvm::getGlobalContext()); break;
    case Primitive::TypeEnum::Uint:
      llvm_type_ = llvm::Type::getInt32Ty(llvm::getGlobalContext());  break;
    case Primitive::TypeEnum::Void:
      llvm_type_ = llvm::Type::getVoidTy(llvm::getGlobalContext());   break;
    default:
      llvm_type_ = nullptr;
  }
}

Array::Array(Type* t) :
  init_fn_(nullptr), uninit_fn_(nullptr), repr_fn_(nullptr), type_(t)
{
  llvm_type_ = llvm::PointerType::getUnqual(t->llvm());
  dim_ = 1 + ((data_type()->is_array())
      ? static_cast<Array*>(data_type())->dim() : 0);

  std::vector<llvm::Type*> init_args(dim_ + 1, *Uint);
  init_args[0] = *Ptr(this);
  has_vars_ = type_->has_variables();
}

Tuple::Tuple(const std::vector<Type*>& types) : entry_types_(types) {
  for (const auto& entry : entry_types_) {
    if (has_vars_) break;
    has_vars_ = entry->has_variables();
  }
}

Pointer::Pointer(Type* t) : pointee_type_(t) {
  llvm_type_ = llvm::PointerType::getUnqual(*pointee_type());
  has_vars_ = pointee_type_->has_variables();
}

Function::Function(Type* in, Type* out) : input(in), output(out) {
  std::vector<llvm::Type *> llvm_in;
  llvm::Type *llvm_out = *Void;

  has_vars_ = input->has_variables() || output->has_variables();

  if (input->is_tuple()) {
    auto in_tup = static_cast<Tuple *>(input);
    for (auto t : in_tup->entry_types()) {
      if (!t->add_llvm_input(llvm_in)) {
        llvm_type_ = nullptr;
        return;
      }
    }
  } else {
    if (!input->add_llvm_input(llvm_in)) {
      llvm_type_ = nullptr;
      return;
    }
  }

  if (output->is_tuple()) {
    auto out_tup = static_cast<Tuple *>(output);
    for (auto t : out_tup->entry_types()) {
      if (!Ptr(t)->add_llvm_input(llvm_in)) {
        llvm_type_ = nullptr;
        return;
      }
    }
  } else if (output->is_enum() || output->is_array() ||
             output->is_primitive()) {
    llvm_out = *output;
    if (llvm_out == nullptr) {
      llvm_type_ = nullptr;
      return;
    }

  } else {
    if (!Ptr(output)->add_llvm_input(llvm_in)) {
      llvm_type_ = nullptr;
      return;
    }
  }

  llvm_type_ = llvm::FunctionType::get(llvm_out, llvm_in, false);
}

Enumeration::Enumeration(const std::string& name,
    const AST::EnumLiteral* enumlit) : bound_name(name), string_data(nullptr) {
  llvm_type_ = *Uint;

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
  llvm_type_ = struct_type;

  for (const auto& field : fields) {
    if (has_vars_) break;
    has_vars_ = field.second->has_variables();
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
  static_cast<llvm::StructType*>(llvm_type_)->setName(bound_name);
  if (name == "string") {
    String = this;
  }
}

std::ostream& operator<<(std::ostream& os, const Type& t) {
  return os << t.to_string();
}
