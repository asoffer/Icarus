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

std::map<Language::Operator, std::map<Type*, Type*>> Type::op_map_ = {};

Function* Type::get_function(std::vector<Type*> in, Type* out) {
  return get_function(get_tuple(in), out);
}

Function* Type::get_function(Type* in, Type* out) {
  for (const auto& fn_type : Function::fn_types_) {
    if (fn_type->input_type_ != in) continue;
    if (fn_type->output_type_ != out) continue;
    return fn_type;
  }

  auto fn_type = new Function(in, out);
  Function::fn_types_.push_back(fn_type);
  return fn_type;
}

Type* Type::get_tuple(const std::vector<Type*>& types) {
  for (const auto& tuple_type : Tuple::tuple_types_) {
    if (tuple_type->entry_types_ == types) return tuple_type;
  }

  auto tuple_type = new Tuple(types);
  Tuple::tuple_types_.push_back(tuple_type);
  return tuple_type;
}

size_t Type::bytes() const {
  return (llvm_type_ == nullptr)
    ? 0 : data_layout->getTypeStoreSize(llvm_type_);
}

Type* Type::get_array(Type* t) {
  for (const auto& arr : Array::array_types_) {
    if (arr->type_ == t) return arr;
  }

  auto arr_type = new Array(t);
  Array::array_types_.push_back(arr_type);
  return arr_type;
}


std::vector<Pointer*> Pointer::pointer_types_;
std::vector<Array*> Array::array_types_;
std::vector<Tuple*> Tuple::tuple_types_;
std::vector<Function*> Function::fn_types_;


std::map<std::string, UserDefined*> UserDefined::lookup_;
std::map<std::string, Enum*> Enum::lookup_;

Type* Type::get_string() {
  auto iter = UserDefined::lookup_.find("string");
  return (iter == UserDefined::lookup_.end()) ? nullptr : iter->second;
}

Function::Function(Type* in, Type* out) : input_type_(in), output_type_(out) {
  bool llvm_null = false;
  std::vector<llvm::Type*> input_list;
  if (input_type_->is_tuple()) {
    auto input_tuple = static_cast<Tuple*>(input_type_);
    auto len = input_tuple->size();
    input_list.resize(len, nullptr);

    size_t i = 0;
    // TODO robustify this.
    // What if tuple element is a tuple?
    // What if tuple element is a function?
    // ...
    for (const auto& input : input_tuple->entry_types_) {
      input_list[i] = (input->is_function() || input->is_user_defined())
        ? *Ptr(input)
        : *input;
      if (input_list[i] == nullptr) {
        llvm_null = true;
        break;
      }
      ++i;
    }
  } else if (input_type_->is_function() || input_type_->is_user_defined()) {
    input_list.push_back(*Ptr(input_type_));

  } else if (!input_type_->is_void()) {
    if (input_type_->llvm() == nullptr) {
      llvm_null = true;
    } else {
      input_list.push_back(*input_type_);
    }
  } else {
    input_list.clear();
  }

  if (output_type_->is_user_defined()) {
    input_list.push_back(*Ptr(output_type_));
    llvm_type_ = llvm::FunctionType::get(*Void, input_list, false);
    return;
  }

  llvm::Type* llvm_output = output_type_->is_function()
    ? *Ptr(output_type_)
    : *output_type_;

  if (llvm_output == nullptr) {
    llvm_null = true;
  }

  // Boolean parameter 'false' designates that this function is not variadic.
  llvm_type_ = llvm_null
    ? nullptr
    : llvm::FunctionType::get(llvm_output, input_list, false);
}

Type* Type::get_type_from_identifier(const std::string& name) {
  auto enum_iter = Enum::lookup_.find(name);
  if (enum_iter != Enum::lookup_.end()) return enum_iter->second;

  auto udef_iter = UserDefined::lookup_.find(name);
  if (udef_iter != UserDefined::lookup_.end()) return udef_iter->second;

#ifdef DEBUG
  std::cerr << "FATAL: No type found matching " << name << std::endl;
#endif

  return nullptr;
}

Type* Type::get_enum(const std::string& name) {
  return Enum::lookup_ AT(name);
}

Type* Type::get_user_defined(const std::string& name) {
  return UserDefined::lookup_ AT(name);
}

Enum* Type::make_enum(
    std::shared_ptr<AST::EnumLiteral> enumlit, const std::string& name) {

  auto iter = Enum::lookup_.find(name);
  if (iter != Enum::lookup_.end()) return iter->second;

  auto enum_type = new Enum(enumlit.get());
  return Enum::lookup_[name] = enum_type;
}

UserDefined* Type::make_user_defined(
    const std::vector<DeclPtr>& decls, const std::string& name) {

  auto iter = UserDefined::lookup_.find(name);
  if (iter != UserDefined::lookup_.end()) return iter->second;

  auto user_def_type = new UserDefined;

  for (const auto& decl : decls) {
    if (decl->type_is_inferred()) {
      // TODO
    } else {
      user_def_type->fields_.emplace_back(decl->identifier_string(),
          decl->interpret_as_type());
    }
  }

  llvm::StructType* struct_type =
    llvm::StructType::create(global_module->getContext());
  struct_type->setName(name);

  size_t num_fields = user_def_type->fields_.size();
  std::vector<llvm::Type*> llvm_fields(num_fields, nullptr);
  for (size_t i = 0; i < num_fields; ++i) {
    llvm_fields[i] = 
      user_def_type->fields_[i].second->llvm();
  }

  // The boolean parameter is 'isPacked'
  struct_type->setBody(std::move(llvm_fields), false);
  user_def_type->llvm_type_ = struct_type;

  return UserDefined::lookup_[name] = user_def_type;
}


UserDefined::UserDefined() :
  init_fn_(nullptr), uninit_fn_(nullptr), print_fn_(nullptr)
{
}

Array::Array(Type* t) :
  init_fn_(nullptr), uninit_fn_(nullptr), repr_fn_(nullptr), type_(t)
{
  llvm_type_ = llvm::PointerType::getUnqual(t->llvm());

  dim_ = 1 + ((data_type()->is_array()) ? static_cast<Array*>(data_type())->dim_ : 0);

  std::vector<llvm::Type*> init_args(dim_ + 1, Uint->llvm());
  init_args[0] = *Ptr(this);
}

Type* UserDefined::field(const std::string& name) const {
  auto iter = fields_.cbegin();
  while (iter != fields_.end()) {
    if (iter->first == name) {
      return iter->second;
    }
    ++iter;
  }
  return nullptr;
}

llvm::Value* UserDefined::field_num(const std::string& name) const {
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

Enum::Enum(AST::EnumLiteral* enumlit) {
  llvm_type_ = *Uint;

  llvm::IRBuilder<> bldr(llvm::getGlobalContext());
  // size_t enum_size = enumlit->vals_.size();

  // TODO Use bldr to create a global array of enum_size charptrs

  size_t i = 0;
  for (const auto& idstr : enumlit->vals_) {
    intval_[idstr] = data::const_uint(i);
    // llvm::Value* print_str = data::str(idstr);
    // TODO add print_str to the global array
    ++i;
  }
}

void Type::initialize_operator_table() {
  op_map_[Language::Operator::Arrow] = {
    { get_tuple({ Type_, Type_ }), Type_ },
  };

  op_map_[Language::Operator::OrEq] = {
    { get_tuple({ Bool, Bool }), Void },
  };

  op_map_[Language::Operator::XorEq] = {
    { get_tuple({ Bool, Bool }), Void },
  };

  op_map_[Language::Operator::AndEq] = {
    { get_tuple({ Bool, Bool }), Void },
  };

  op_map_[Language::Operator::AddEq] = {
    { get_tuple({ Int,  Int }), Void },
    { get_tuple({ Uint, Uint }), Void },
    { get_tuple({ Real, Real }), Void },
  };

  op_map_[Language::Operator::SubEq] = {
    { get_tuple({ Int,  Int }), Void },
    { get_tuple({ Uint, Uint }), Void },
    { get_tuple({ Real, Real }), Void },
  };

  op_map_[Language::Operator::MulEq] = {
    { get_tuple({ Int,  Int }), Void },
    { get_tuple({ Uint, Uint }), Void },
    { get_tuple({ Real, Real }), Void },
  };

  op_map_[Language::Operator::DivEq] = {
    { get_tuple({ Int,  Int }), Void },
    { get_tuple({ Uint, Uint }), Void },
    { get_tuple({ Real, Real }), Void },
  };

  op_map_[Language::Operator::ModEq] = {
    { get_tuple({ Int,  Int }), Void },
    { get_tuple({ Uint, Uint }), Void },
  };

  op_map_[Language::Operator::Or] = {
    { get_tuple({ Bool, Bool }), Bool },
  };

  op_map_[Language::Operator::Xor] = {
    { get_tuple({ Bool, Bool }), Bool },
  };

  op_map_[Language::Operator::And] = {
    { get_tuple({ Bool, Bool }), Bool },
  };

  op_map_[Language::Operator::LessThan] = {
    { get_tuple({ Int,  Int }), Bool },
    { get_tuple({ Uint, Uint }), Bool },
    { get_tuple({ Real, Real }), Bool },
  };

  op_map_[Language::Operator::LessEq] = {
    { get_tuple({ Int,  Int }), Bool },
    { get_tuple({ Uint, Uint }), Bool },
    { get_tuple({ Real, Real }), Bool },
  };

  op_map_[Language::Operator::Equal] = {
    { get_tuple({ Bool, Bool }), Bool },
    { get_tuple({ Char, Char }), Bool },
    { get_tuple({ Int,  Int }), Bool },
    { get_tuple({ Uint, Uint }), Bool },
    { get_tuple({ Real, Real }), Bool },
    { get_tuple({ Type_, Type_ }), Bool },
  };

  op_map_[Language::Operator::NotEqual] = {
    { get_tuple({ Bool, Bool }), Bool },
    { get_tuple({ Char, Char }), Bool },
    { get_tuple({ Int,  Int }), Bool },
    { get_tuple({ Uint, Uint }), Bool },
    { get_tuple({ Real, Real }), Bool },
    { get_tuple({ Type_, Type_ }), Bool },
  };

  op_map_[Language::Operator::GreaterEq] = {
    { get_tuple({ Int,  Int }), Bool },
    { get_tuple({ Uint, Uint }), Bool },
    { get_tuple({ Real, Real }), Bool },
  };

  op_map_[Language::Operator::GreaterThan] = {
    { get_tuple({ Int,  Int }), Bool },
    { get_tuple({ Uint, Uint }), Bool },
    { get_tuple({ Real, Real }), Bool },
  };

  op_map_[Language::Operator::Add] = {
    { get_tuple({ Int,  Int }), Int },
    { get_tuple({ Uint, Uint }), Uint },
    { get_tuple({ Real, Real }), Real },
  };

  op_map_[Language::Operator::Sub] = {
    { get_tuple({ Int,  Int }), Int },
    { get_tuple({ Uint, Uint }), Uint },
    { get_tuple({ Real, Real }), Real },
  };

  op_map_[Language::Operator::Mul] = {
    { get_tuple({ Int,  Int }), Int },
    { get_tuple({ Uint, Uint }), Uint },
    { get_tuple({ Real, Real }), Real },
  };

  op_map_[Language::Operator::Div] = {
    { get_tuple({ Int,  Int }), Int },
    { get_tuple({ Uint, Uint }), Uint },
    { get_tuple({ Real, Real }), Real },
  };

  op_map_[Language::Operator::Mod] = {
    { get_tuple({ Int,  Int }), Int },
    { get_tuple({ Uint, Uint }), Uint },
  };

  op_map_[Language::Operator::Not] = {
    { Bool, Bool },
  };
}

Type* Type::get_operator(Language::Operator op, Type* signature) {
  auto operator_set = op_map_[op];
  auto iter = operator_set.find(signature);
  return (iter != operator_set.end()) ? iter->second : nullptr;
}

namespace TypeSystem {
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

  void initialize() {
    // TODO do we need to pair of strings and their types?
    Literals["bool"] = Bool  = new Primitive(Primitive::TypeEnum::Bool);
    Literals["char"] = Char  = new Primitive(Primitive::TypeEnum::Char);
    Literals["int"]  = Int   = new Primitive(Primitive::TypeEnum::Int);
    Literals["real"] = Real  = new Primitive(Primitive::TypeEnum::Real);
    Literals["type"] = Type_ = new Primitive(Primitive::TypeEnum::Type);
    Literals["uint"] = Uint  = new Primitive(Primitive::TypeEnum::Uint);
    Literals["void"] = Void  = new Primitive(Primitive::TypeEnum::Void);

    Error   = new Primitive(Primitive::TypeEnum::Error);
    Unknown = new Primitive(Primitive::TypeEnum::Unknown);
    RawPtr = Ptr(Char);
  }
}  // namespace TypeSystem



bool Array::requires_uninit() const { return true; }
bool UserDefined::requires_uninit() const {
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
