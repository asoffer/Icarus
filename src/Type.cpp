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
}  // namespace data

#define PRIMITIVE_TYPE_MACRO(type)                               \
  Type* Type::get_##type () {                                    \
    return &(Primitive::primitive_types_[Primitive::t_##type]);  \
  }
#include "config/primitive_types.conf"

#undef PRIMITIVE_TYPE_MACRO

#define PRIMITIVE_TYPE_MACRO(type) { #type , Type::get_##type() },
std::map<std::string, Type*> Type::literals = {
#include "config/primitive_types.conf"
};
#undef PRIMITIVE_TYPE_MACRO

std::map<Language::Operator, std::map<Type*, Type*>> Type::op_map_ = {};

Primitive::Primitive(PrimitiveEnum pe) : repr_fn_(nullptr), prim_type_(pe) {
  if (llvm_types_[prim_type_] == nullptr) {
    switch (prim_type_) {
      case t_bool:
        llvm_types_[prim_type_] =
          llvm::Type::getInt1Ty(llvm::getGlobalContext());
        break;
      case t_char:
        llvm_types_[prim_type_] =
          llvm::Type::getInt8Ty(llvm::getGlobalContext());
        break;
      case t_int:
        llvm_types_[prim_type_] =
          llvm::Type::getInt32Ty(llvm::getGlobalContext());
        break;
      case t_real:
        llvm_types_[prim_type_] =
          llvm::Type::getDoubleTy(llvm::getGlobalContext());
        break;
      case t_uint: // make it unsigned
        llvm_types_[prim_type_] =
          llvm::Type::getInt32Ty(llvm::getGlobalContext());
        break;
      case t_void:
        llvm_types_[prim_type_] =
          llvm::Type::getVoidTy(llvm::getGlobalContext());
        break;
      default:
        llvm_types_[prim_type_] = nullptr;
        break;
    }
  }
  llvm_type_ = llvm_types_[prim_type_];
};

#define PRIMITIVE_TYPE_MACRO(type) Primitive(Primitive::t_##type),
Primitive Primitive::primitive_types_[ num_primitive_types_ ] = {
#include "config/primitive_types.conf"
};
#undef PRIMITIVE_TYPE_MACRO

// NOTE: Ideally, we'd pre-compute all of these. Unfortunately, this is not
// possible in the obvious way, due to the static initialization order fiasco.
//
// TODO On entry into main, allocate all of these, intsead of doing so in the
// Primitive type constructor
llvm::Type* Primitive::llvm_types_[ num_primitive_types_ ] = {
  nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr
};

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

Type* Type::get_pointer(Type* t) {
  for (const auto& ptr : Pointer::pointer_types_) {
    if (ptr->pointee_type_ == t) return ptr;
  }

  auto ptr_type = new Pointer(t);
  Pointer::pointer_types_.push_back(ptr_type);
  return ptr_type;
}

Type* Type::get_tuple(const std::vector<Type*>& types) {
  for (const auto& tuple_type : Tuple::tuple_types_) {
    if (tuple_type->entry_types_ == types) return tuple_type;
  }

  auto tuple_type = new Tuple(types);
  Tuple::tuple_types_.push_back(tuple_type);
  return tuple_type;
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
        ? get_pointer(input)->llvm()
        : input->llvm();
      if (input_list[i] == nullptr) {
        llvm_null = true;
        break;
      }
      ++i;
    }
  } else if (input_type_->is_function() || input_type_->is_user_defined()) {
    input_list.push_back(get_pointer(input_type_)->llvm());

  } else if (!input_type_->is_void()) {
    if (input_type_->llvm() == nullptr) {
      llvm_null = true;
    } else {
      input_list.push_back(input_type_->llvm());
    }
  }

  if (output_type_->is_user_defined()) {
    input_list.push_back(get_pointer(output_type_)->llvm());
    llvm_type_ = llvm::FunctionType::get(get_void()->llvm(), input_list, false);
    return;
  }

  auto llvm_output = output_type_->is_function()
    ? get_pointer(output_type_)->llvm()
    : output_type_->llvm();

  if (llvm_output == nullptr) {
    llvm_null = true;
  }

  // Boolean parameter 'false' designates that this function is not variadic.
  llvm_type_ = llvm_null
    ? nullptr
    : llvm::FunctionType::get(llvm_output, input_list, false);
}

Type* Type::get_user_defined(const std::string& name) {
  return UserDefined::lookup_ AT(name);
}

Type* Type::make_user_defined(
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


Array::Array(Type* t) : repr_fn_(nullptr), type_(t) {
  // TODO is the length ever part of the type?
  llvm_type_ = llvm::PointerType::getUnqual(t->llvm());

  dim_ = 1 + ((data_type()->is_array()) ? static_cast<Array*>(data_type())->dim_ : 0);

  std::vector<llvm::Type*> init_args(dim_ + 1, get_uint()->llvm());
  init_args[0] = get_pointer(this)->llvm();
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

void Type::initialize_operator_table() {
  op_map_[Language::Operator::Arrow] = {
    { get_tuple({ get_type(), get_type() }), get_type() },
  };

  op_map_[Language::Operator::OrEq] = {
    { get_tuple({ get_bool(), get_bool() }), get_void() },
  };

  op_map_[Language::Operator::XorEq] = {
    { get_tuple({ get_bool(), get_bool() }), get_void() },
  };

  op_map_[Language::Operator::AndEq] = {
    { get_tuple({ get_bool(), get_bool() }), get_void() },
  };

  op_map_[Language::Operator::AddEq] = {
    { get_tuple({ get_int(),  get_int() }), get_void() },
    { get_tuple({ get_uint(), get_uint() }), get_void() },
    { get_tuple({ get_real(), get_real() }), get_void() },
  };

  op_map_[Language::Operator::SubEq] = {
    { get_tuple({ get_int(),  get_int() }), get_void() },
    { get_tuple({ get_uint(), get_uint() }), get_void() },
    { get_tuple({ get_real(), get_real() }), get_void() },
  };

  op_map_[Language::Operator::MulEq] = {
    { get_tuple({ get_int(),  get_int() }), get_void() },
    { get_tuple({ get_uint(), get_uint() }), get_void() },
    { get_tuple({ get_real(), get_real() }), get_void() },
  };

  op_map_[Language::Operator::DivEq] = {
    { get_tuple({ get_int(),  get_int() }), get_void() },
    { get_tuple({ get_uint(), get_uint() }), get_void() },
    { get_tuple({ get_real(), get_real() }), get_void() },
  };

  op_map_[Language::Operator::ModEq] = {
    { get_tuple({ get_int(),  get_int() }), get_void() },
    { get_tuple({ get_uint(), get_uint() }), get_void() },
  };

  op_map_[Language::Operator::Or] = {
    { get_tuple({ get_bool(), get_bool() }), get_bool() },
  };

  op_map_[Language::Operator::Xor] = {
    { get_tuple({ get_bool(), get_bool() }), get_bool() },
  };

  op_map_[Language::Operator::And] = {
    { get_tuple({ get_bool(), get_bool() }), get_bool() },
  };

  op_map_[Language::Operator::LessThan] = {
    { get_tuple({ get_int(),  get_int() }), get_bool() },
    { get_tuple({ get_uint(), get_uint() }), get_bool() },
    { get_tuple({ get_real(), get_real() }), get_bool() },
  };

  op_map_[Language::Operator::LessEq] = {
    { get_tuple({ get_int(),  get_int() }), get_bool() },
    { get_tuple({ get_uint(), get_uint() }), get_bool() },
    { get_tuple({ get_real(), get_real() }), get_bool() },
  };

  op_map_[Language::Operator::Equal] = {
    { get_tuple({ get_bool(), get_bool() }), get_bool() },
    { get_tuple({ get_char(), get_char() }), get_bool() },
    { get_tuple({ get_int(),  get_int() }), get_bool() },
    { get_tuple({ get_uint(), get_uint() }), get_bool() },
    { get_tuple({ get_real(), get_real() }), get_bool() },
    { get_tuple({ get_type(), get_type() }), get_bool() },
  };

  op_map_[Language::Operator::NotEqual] = {
    { get_tuple({ get_bool(), get_bool() }), get_bool() },
    { get_tuple({ get_char(), get_char() }), get_bool() },
    { get_tuple({ get_int(),  get_int() }), get_bool() },
    { get_tuple({ get_uint(), get_uint() }), get_bool() },
    { get_tuple({ get_real(), get_real() }), get_bool() },
    { get_tuple({ get_type(), get_type() }), get_bool() },
  };

  op_map_[Language::Operator::GreaterEq] = {
    { get_tuple({ get_int(),  get_int() }), get_bool() },
    { get_tuple({ get_uint(), get_uint() }), get_bool() },
    { get_tuple({ get_real(), get_real() }), get_bool() },
  };

  op_map_[Language::Operator::GreaterThan] = {
    { get_tuple({ get_int(),  get_int() }), get_bool() },
    { get_tuple({ get_uint(), get_uint() }), get_bool() },
    { get_tuple({ get_real(), get_real() }), get_bool() },
  };

  op_map_[Language::Operator::Add] = {
    { get_tuple({ get_int(),  get_int() }), get_int() },
    { get_tuple({ get_uint(), get_uint() }), get_uint() },
    { get_tuple({ get_real(), get_real() }), get_real() },
  };

  op_map_[Language::Operator::Sub] = {
    { get_tuple({ get_int(),  get_int() }), get_int() },
    { get_tuple({ get_uint(), get_uint() }), get_uint() },
    { get_tuple({ get_real(), get_real() }), get_real() },
  };

  op_map_[Language::Operator::Mul] = {
    { get_tuple({ get_int(),  get_int() }), get_int() },
    { get_tuple({ get_uint(), get_uint() }), get_uint() },
    { get_tuple({ get_real(), get_real() }), get_real() },
  };

  op_map_[Language::Operator::Div] = {
    { get_tuple({ get_int(),  get_int() }), get_int() },
    { get_tuple({ get_uint(), get_uint() }), get_uint() },
    { get_tuple({ get_real(), get_real() }), get_real() },
  };

  op_map_[Language::Operator::Mod] = {
    { get_tuple({ get_int(),  get_int() }), get_int() },
    { get_tuple({ get_uint(), get_uint() }), get_uint() },
  };

  op_map_[Language::Operator::Not] = {
    { get_bool(), get_bool() },
  };
}

Type* Type::get_operator(Language::Operator op, Type* signature) {
  auto operator_set = op_map_[op];
  auto iter = operator_set.find(signature);
  return (iter != operator_set.end()) ? iter->second : nullptr;
}
