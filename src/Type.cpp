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
Type::Type() : assign_fn_(nullptr) {}

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
}

Tuple::Tuple(const std::vector<Type*>& types) : entry_types_(types) {}

Pointer::Pointer(Type* t) : pointee_type_(t) {
  llvm_type_ = llvm::PointerType::getUnqual(*pointee_type());
}

Function::Function(Type* in, Type* out) : input_type_(in), output_type_(out) {
  std::vector<llvm::Type*> llvm_in;
  llvm::Type* llvm_out = *Void;

  if (input_type_->is_tuple()) {
    auto in_tup = static_cast<Tuple*>(input_type_);
    for (auto t : in_tup->entry_types()) {
      if (! t->add_llvm_input(llvm_in)) {
        llvm_type_ = nullptr; return;
      }
    }
  } else {
    if (! input_type_->add_llvm_input(llvm_in)) {
      llvm_type_ = nullptr; return;
    }
  }

  if (output_type_->is_tuple()) {
    auto out_tup = static_cast<Tuple*>(output_type_);
    for (auto t : out_tup->entry_types()) {
      if (! Ptr(t)->add_llvm_input(llvm_in)) {
        llvm_type_ = nullptr; return;
      }
    }
  } else if (output_type_->is_enum() || output_type_->is_array()
      || output_type_->is_primitive()) {
    llvm_out = *output_type_;
    if (llvm_out == nullptr) {
      llvm_type_ = nullptr; return;
    }

  } else {
    if (! Ptr(output_type_)->add_llvm_input(llvm_in)) {
      llvm_type_ = nullptr; return;
    }
  }

  llvm_type_ = llvm::FunctionType::get(llvm_out, llvm_in, false);
}

Enumeration::Enumeration(const std::string& name,
    const AST::EnumLiteral* enumlit) : name_(name) {
  llvm_type_ = *Uint;

  llvm::IRBuilder<> bldr(llvm::getGlobalContext());
  // size_t enum_size = enumlit->vals_.size();

  // TODO Use bldr to create a global array of enum_size char ptrs

  size_t i = 0;
  for (const auto& idstr : enumlit->vals_) {
    intval_[idstr] = data::const_uint(i);
    // llvm::Value* print_str = data::str(idstr);
    // TODO add print_str to the global array
    ++i;
  }
}

Structure::Structure(const std::string& name, const std::vector<DeclPtr>& decls)
  : name_(name), init_fn_(nullptr), uninit_fn_(nullptr), print_fn_(nullptr)
{
  for (const auto& decl : decls) {
    if (decl->type_is_inferred()) {
      // TODO
    } else {
      fields_.emplace_back(decl->identifier_string(), decl->interpret_as_type());
    }
  }

  llvm::StructType* struct_type =
    llvm::StructType::create(global_module->getContext());
  struct_type->setName(name);

  size_t num_fields = fields_.size();
  std::vector<llvm::Type*> llvm_fields(num_fields, nullptr);
  for (size_t i = 0; i < num_fields; ++i) {
    llvm_fields[i] = fields_[i].second->llvm();
  }

  struct_type->setBody(std::move(llvm_fields), /* isPacked = */ false);
  llvm_type_ = struct_type;
}

Type* Structure::field(const std::string& name) const {
  auto iter = fields_.cbegin();
  while (iter != fields_.end()) {
    if (iter->first == name) {
      return iter->second;
    }
    ++iter;
  }
  return nullptr;
}

llvm::Value* Structure::field_num(const std::string& name) const {
  size_t i = 0;
  auto iter = fields_.cbegin();
  while (iter != fields_.end()) {
    if (iter->first == name) {
      return data::const_uint(i);
    }
    ++iter; ++i;
  }
  return nullptr;
}


bool Array::requires_uninit() const { return true; }
bool Structure::requires_uninit() const {
  for (const auto field : fields_) {
    if (field.second->requires_uninit()) {
      return true;
    }
  }
  return false;
}

std::ostream& operator<<(std::ostream& os, const Type& t) {
  return os << t.to_string();
}
