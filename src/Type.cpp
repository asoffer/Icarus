#include "Type.h"
#include "AST.h"

extern llvm::Module* global_module;

namespace cstdlib {
  extern llvm::Constant* malloc();
}  // namespace cstdlib

namespace data {
  extern llvm::Value* const_uint(size_t n);
}  // namespace data

#define MAKE_PRIMITIVE_TYPE_GETTER(type)                                 \
  Type* Type::get_##type () {                                            \
    return &(Primitive::primitive_types_[Primitive::t_##type]);          \
  }

MAKE_PRIMITIVE_TYPE_GETTER(type_error);
MAKE_PRIMITIVE_TYPE_GETTER(unknown);
MAKE_PRIMITIVE_TYPE_GETTER(bool);
MAKE_PRIMITIVE_TYPE_GETTER(char);
MAKE_PRIMITIVE_TYPE_GETTER(int);
MAKE_PRIMITIVE_TYPE_GETTER(real);
MAKE_PRIMITIVE_TYPE_GETTER(type);
MAKE_PRIMITIVE_TYPE_GETTER(uint);
MAKE_PRIMITIVE_TYPE_GETTER(void);

#undef MAKE_PRIMITIVE_TYPE_GETTER

std::map<std::string, Type*> Type::literals = {
  { "type_error", Type::get_type_error() },
  { "??",         Type::get_unknown() },
  { "bool",       Type::get_bool() },
  { "char",       Type::get_char() },
  { "int",        Type::get_int() },
  { "real",       Type::get_real() },
  { "type",       Type::get_type() },
  { "uint",       Type::get_uint() },
  { "void",       Type::get_void() }
};


Primitive::Primitive(PrimitiveEnum pe) : prim_type_(pe) {
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

Primitive Primitive::primitive_types_[ num_primitive_types_ ] = {
  Primitive(Primitive::t_type_error),
  Primitive(Primitive::t_unknown),
  Primitive(Primitive::t_bool),
  Primitive(Primitive::t_char),
  Primitive(Primitive::t_int),
  Primitive(Primitive::t_real),
  Primitive(Primitive::t_type),
  Primitive(Primitive::t_uint),
  Primitive(Primitive::t_void)
};

// NOTE: Ideally, we'd pre-compute all of these. Unfortunately, this is not
// possible in the obvious way, due to the static initialization order fiasco.
//
// TODO On entry into main, allocate all of these, intsead of doing so in the
// Primitive type constructor
llvm::Type* Primitive::llvm_types_[ num_primitive_types_ ] = {
  nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr
};

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


Type* Type::get_user_defined(const std::string& name) {
#ifdef DEBUG
  return UserDefined::lookup_.at(name);
#endif
  return UserDefined::lookup_[name];
}

void Type::make_user_defined(
    const std::vector<DeclPtr>& decls, const std::string& name) {

  auto iter = UserDefined::lookup_.find(name);
  if (iter != UserDefined::lookup_.end()) return;

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

  UserDefined::lookup_[name] = user_def_type;
}


Array::Array(Type* t) : type_(t) {
  // TODO is the length ever part of the type?
  llvm_type_ = llvm::PointerType::getUnqual(t->llvm());

  dim_ = 1 + ((data_type()->is_array()) ? static_cast<Array*>(data_type())->dim_ : 0);

  std::vector<llvm::Type*> init_args(dim_ + 1, get_uint()->llvm());
  init_args[0] = get_pointer(this)->llvm();

  // Don't even bother to build a FnScope
  init_fn_ = llvm::Function::Create(
      llvm::FunctionType::get(Type::get_void()->llvm(), init_args, false),
      llvm::Function::ExternalLinkage,
      "init." + to_string(), global_module);

  auto entry_block = llvm::BasicBlock::Create(
      llvm::getGlobalContext(), "entry", init_fn_);

  llvm::IRBuilder<> fn_bldr(llvm::getGlobalContext());
  fn_bldr.SetInsertPoint( entry_block );

  // TODO is there a cleaner notation for this? I hope so.
  // There's nothing problematic here, it's just ugly.
  std::vector<llvm::Value*> args;
  size_t arg_num = 0;
  for (auto& arg : init_fn_->args()) {
    arg.setName("arg" + std::to_string(arg_num));
    args.push_back(&arg);
  }
  auto store_ptr = args[0];
  auto len_val = args[1];

  auto bytes_per_elem = data::const_uint(data_type()->bytes());
  auto int_size = data::const_uint(Type::get_int()->bytes());
  auto bytes_needed = fn_bldr.CreateAdd(int_size, 
      fn_bldr.CreateMul(len_val, bytes_per_elem), "malloc_bytes");

  // Malloc call
  auto malloc_call = fn_bldr.CreateCall(cstdlib::malloc(), { bytes_needed });

  // Pointer to the length at the head of the array
  auto len_ptr = fn_bldr.CreateBitCast(malloc_call,
      get_pointer(get_int())->llvm(), "len_ptr");

  fn_bldr.CreateStore(len_val, len_ptr);

  // Pointer to the array data
  auto raw_data_ptr = fn_bldr.CreateGEP(Type::get_char()->llvm(),
      malloc_call, { int_size }, "raw_data_ptr");

  // Pointer to data cast
  auto ptr_type = Type::get_pointer(data_type())->llvm();
  auto data_ptr = fn_bldr.CreateBitCast(raw_data_ptr, ptr_type, "data_ptr");
  fn_bldr.CreateStore(data_ptr, store_ptr);

  // Loop through the array and initialize each input
  auto loop_block = llvm::BasicBlock::Create(
    llvm::getGlobalContext(), "loop", init_fn_);
  auto exit_block = llvm::BasicBlock::Create(
    llvm::getGlobalContext(), "exit", init_fn_);

  fn_bldr.CreateBr(loop_block);
  fn_bldr.SetInsertPoint(loop_block);

  llvm::PHINode* phi = fn_bldr.CreatePHI(get_uint()->llvm(), 2, "phi");
  phi->addIncoming(data::const_uint(0), entry_block);

  data_ptr = fn_bldr.CreateGEP(data_ptr, { phi });

  if (data_type()->init_fn_ == nullptr) {
    data_type()->initialize(fn_bldr, data_ptr);

  } else {
    std::vector<llvm::Value*> call_args = { data_ptr };
    auto iter = init_fn_->args().begin();
    ++iter;
    ++iter;
    while (iter != init_fn_->args().end()) {
      call_args.push_back(iter);
      ++iter;
    }
    
    fn_bldr.CreateCall(data_type()->init_fn_, call_args);
  }

  auto next_data = fn_bldr.CreateAdd(phi, data::const_uint(1));
  fn_bldr.CreateCondBr(fn_bldr.CreateICmpULT(next_data, len_val),
      loop_block, exit_block);
  phi->addIncoming(next_data, loop_block);

  fn_bldr.SetInsertPoint(exit_block);
  fn_bldr.CreateRetVoid();
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
