#include "Type.h"
#include "AST.h"
#include "ScopeDB.h"

using ::ScopeDB::Scope;

extern llvm::Module* global_module;
extern llvm::IRBuilder<> global_builder;

namespace cstdlib {
  extern llvm::Constant* malloc();

  extern llvm::Constant* putchar();
  extern llvm::Constant* printf();

  template<char C> llvm::Value* format() {
    std::string char_str(1, C);
    static llvm::Value* format_ =
      global_builder.CreateGlobalStringPtr("%" + char_str, "percent_" + char_str);

    return format_;
  }


}  // namespace cstdlib

namespace data {
  extern llvm::Value* const_int(size_t n, bool is_signed = false);
  extern llvm::Value* const_char(char c);
}  // namespace data

std::vector<Function*> Function::fn_types_;

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

std::vector<std::string> Type::type_strings = {
  "type_error", "??", "bool", "char", "int", "real", "type", "uint", "void" };
std::vector<size_t> Type::type_bytes = { 0, 0, 1, 1, 4, 8, 0, 4, 0 };


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

Type* Type::get_array(Type* t, int len) {
 for (const auto& arr : Array::array_types_) {
    if (arr->type_ == t && arr->len_ == len) return arr;
  }

  auto arr_type = new Array(t, len);
  Array::array_types_.push_back(arr_type);
  return arr_type;
}


std::vector<Pointer*> Pointer::pointer_types_;
std::vector<Array*> Array::array_types_;
std::vector<Tuple*> Tuple::tuple_types_;


llvm::Value* Array::make(llvm::IRBuilder<>& bldr, llvm::Value* runtime_len) {
  // NOTE: this cast is safe because len_ is -1 or positive. Moreover, if
  // len_ is -1, then the runtime length is what is used
  llvm::Value* len;
  if (len_ != -1) {
    len = data::const_int(static_cast<size_t>(len_));

  } else if (runtime_len == nullptr) {
    len = data::const_int(0);

  } else {
    len = runtime_len;
  }

  // Compute the amount of space to allocate

  auto bytes_per_elem = data::const_int(data_type()->bytes());
  auto int_size = data::const_int(Type::get_int()->bytes());
  auto zero = data::const_int(0);
  auto bytes_needed = bldr.CreateAdd(int_size, 
      bldr.CreateMul(len, bytes_per_elem), "malloc_bytes");

  // Malloc call
  auto malloc_call = bldr.CreateCall(cstdlib::malloc(), { bytes_needed });

  // Pointer to the length at the head of the array
  auto raw_len_ptr = bldr.CreateGEP(Type::get_char()->llvm(),
      malloc_call, { zero }, "array_len_raw");

  auto len_ptr = bldr.CreateBitCast(
      raw_len_ptr, Type::get_pointer(Type::get_int())->llvm(), "len_ptr");
    bldr.CreateStore(len, len_ptr);

  // Pointer to the array data
  auto raw_data_ptr = bldr.CreateGEP(Type::get_char()->llvm(),
      malloc_call, { int_size }, "array_idx_raw");
  
  // Pointer to data cast
  auto ptr_type = Type::get_pointer(data_type())->llvm();
  return bldr.CreateBitCast(raw_data_ptr, ptr_type, "array_ptr");
}

Type* Primitive::replace(Type* pattern, Type* replacement) {
  return (pattern == this) ? replacement : this;
}

Type* Function::replace(Type* pattern, Type* replacement) {
  return (pattern == this)
    ? replacement
    : Type::get_function(
        input_type_->replace(pattern, replacement),
        output_type_->replace(pattern, replacement));
}

Type* Pointer::replace(Type* pattern, Type* replacement) {
  return (pattern == this)
    ? replacement
    : Type::get_pointer(pointee_type_->replace(pattern, replacement));
}

Type* Tuple::replace(Type* pattern, Type* replacement) {
  if (pattern == this) return replacement;

  auto new_vec = std::vector<Type*>(entry_types_.size(), nullptr);
  for (size_t i = 0; i < entry_types_.size(); ++i) {
    new_vec[i] = entry_types_[i]->replace(pattern, replacement);
  }

  return Type::get_tuple(new_vec);
}

Type* Array::replace(Type* pattern, Type* replacement) {
  return (pattern == this)
    ? replacement
    : Type::get_array(type_->replace(pattern, replacement), len_);
}

llvm::Function* Primitive::print_function() {
  if (print_fn_ != nullptr) return print_fn_;

  // char doesn't require building a new function. We can just
  // call putchar outright.
  if (this == Type::get_char()) {
    return print_fn_ = static_cast<llvm::Function*>(cstdlib::putchar());
  }
  
  // Types require compile-time generated strings. Moreover the .llvm()
  // method returns a nullptr for type Types. Instead, we pass in its
  // string representation, but this means we must take 

  auto input_type = replace(Type::get_type(), Type::get_pointer(Type::get_char()));

  // Otherwise, actually write our own
  print_fn_ = llvm::Function::Create(
      llvm::FunctionType::get(
        Type::get_void()->llvm(), { input_type->llvm() }, false),
      llvm::Function::ExternalLinkage,
      "print." + to_string(), global_module);
  llvm::Value* val = print_fn_->args().begin();

  ScopeDB::Scope* fn_scope = ScopeDB::Scope::build();

  fn_scope->set_parent_function(print_fn_);
  fn_scope->set_return_type(Type::get_void());

  llvm::IRBuilder<>& bldr = fn_scope->builder();

  fn_scope->enter();
  if (this == Type::get_bool()) {
    auto true_str = bldr.CreateGlobalStringPtr("true");
    auto false_str = bldr.CreateGlobalStringPtr("false");

    auto true_block = llvm::BasicBlock::Create(
        llvm::getGlobalContext(), "true_block", print_fn_);
    auto false_block = llvm::BasicBlock::Create(
        llvm::getGlobalContext(), "false_block", print_fn_);
    auto merge_block = llvm::BasicBlock::Create(
        llvm::getGlobalContext(), "merge_block", print_fn_);

    // NOTE: Exactly one argument is provided always
    bldr.CreateCondBr(val, true_block, false_block);

    bldr.SetInsertPoint(true_block);
    bldr.CreateBr(merge_block);

    bldr.SetInsertPoint(false_block);
    bldr.CreateBr(merge_block);

    bldr.SetInsertPoint(merge_block);
    llvm::PHINode* phi_node =
      bldr.CreatePHI(llvm::Type::getInt8PtrTy(llvm::getGlobalContext()), 2, "merge");
    phi_node->addIncoming(true_str, true_block);
    phi_node->addIncoming(false_str, false_block);

    bldr.CreateCall(cstdlib::printf(), { cstdlib::format<'s'>(), phi_node });

  } else if (this == Type::get_int() || this == Type::get_uint()) {
    bldr.CreateCall(cstdlib::printf(), { cstdlib::format<'d'>(), val });

  } else if (this == Type::get_real()) {
    bldr.CreateCall(cstdlib::printf(), { cstdlib::format<'f'>(), val });

  } else if (this == Type::get_type()) {
    // To print a type we must first get it's string representation,
    // which is the job of the code generator. We assume that the value
    // passed in is already this string (as a global string pointer)

    bldr.CreateCall(cstdlib::printf(), { cstdlib::format<'s'>(), val });
  }

  fn_scope->exit();

  return print_fn_;
}

// There must be a better way to do this.
llvm::Function* Array::print_function() {
  if (print_fn_ != nullptr) return print_fn_;

  auto input_type = replace(Type::get_type(), Type::get_pointer(Type::get_char()));

  print_fn_ = llvm::Function::Create(
      llvm::FunctionType::get(
        Type::get_void()->llvm(), { input_type->llvm() }, false),
      llvm::Function::ExternalLinkage,
      "print." + to_string(), global_module);
  llvm::Value* val = print_fn_->args().begin();

  ScopeDB::Scope* fn_scope = ScopeDB::Scope::build();

  fn_scope->set_parent_function(print_fn_);
  fn_scope->set_return_type(Type::get_void());

  llvm::IRBuilder<>& bldr = fn_scope->builder();

  fn_scope->enter();
  bldr.CreateCall(cstdlib::putchar(), { data::const_char('[') });

  // TODO should we do this by building an AST and generating code from it?

  auto basic_ptr_type = Type::get_pointer(Type::get_char())->llvm();

  auto four = data::const_int(4, true);
  auto zero = data::const_int(0, true);
  auto neg_four = bldr.CreateSub(zero, four);

  auto raw_len_ptr = bldr.CreateGEP(
      bldr.CreateBitCast(val, basic_ptr_type),
      { neg_four }, "ptr_to_free");

  auto len_ptr = bldr.CreateBitCast(raw_len_ptr,
      Type::get_pointer(Type::get_int())->llvm());

  auto array_len = bldr.CreateLoad(bldr.CreateGEP(len_ptr, { zero }));

  auto non_empty_block = llvm::BasicBlock::Create(
      llvm::getGlobalContext(), "non_empty", print_fn_);
  auto cond_block = llvm::BasicBlock::Create(
      llvm::getGlobalContext(), "cond", print_fn_);
  auto loop_block = llvm::BasicBlock::Create(
      llvm::getGlobalContext(), "loop", print_fn_);
  auto land_block = llvm::BasicBlock::Create(
      llvm::getGlobalContext(), "land", print_fn_);
  auto finish_block = llvm::BasicBlock::Create(
      llvm::getGlobalContext(), "finish", print_fn_);


  bldr.CreateCondBr(bldr.CreateICmpEQ(array_len, data::const_int(0)),
      finish_block, non_empty_block);
  bldr.SetInsertPoint(non_empty_block);

  auto array_len_less_one = bldr.CreateSub(array_len, data::const_int(1));

  auto i_store = bldr.CreateAlloca(Type::get_int()->llvm(), nullptr, "i");
  bldr.CreateStore(zero, i_store);
  bldr.CreateBr(cond_block);

  bldr.SetInsertPoint(cond_block);
  auto i_val = bldr.CreateLoad(i_store);
  auto cmp_val = bldr.CreateICmpSLT(i_val, array_len_less_one);
  bldr.CreateCondBr(cmp_val, loop_block, land_block);
  bldr.SetInsertPoint(loop_block);

  auto elem_ptr = bldr.CreateGEP(val, { i_val });

  bldr.CreateCall(data_type()->print_function(), { bldr.CreateLoad(elem_ptr) });

  auto comma_space = global_builder.CreateGlobalStringPtr(", ");
  bldr.CreateCall(cstdlib::printf(), { cstdlib::format<'s'>(), comma_space });

  auto new_i_val = bldr.CreateAdd(i_val, data::const_int(1));
  bldr.CreateStore(new_i_val, i_store);
  bldr.CreateBr(cond_block);

  bldr.SetInsertPoint(land_block);
  auto last_elem = bldr.CreateGEP(val, { array_len_less_one });
  bldr.CreateCall(data_type()->print_function(), { bldr.CreateLoad(last_elem) });

  bldr.CreateBr(finish_block);

  bldr.SetInsertPoint(finish_block);
  bldr.CreateCall(cstdlib::putchar(), { data::const_char(']') });

  fn_scope->exit();

  return print_fn_;
}

llvm::Function* Function::print_function() {
  if (print_fn_ != nullptr) return print_fn_;

  auto fn_print_str = global_builder.CreateGlobalStringPtr(
      "<function " + to_string() + ">");

  auto input_type = replace(Type::get_type(), Type::get_pointer(Type::get_char()));

  // TODO
  print_fn_ = llvm::Function::Create(
      llvm::FunctionType::get(
        Type::get_void()->llvm(), { Type::get_pointer(input_type)->llvm() }, false),
      llvm::Function::ExternalLinkage,
      "print." + to_string(), global_module);

  ScopeDB::Scope* fn_scope = ScopeDB::Scope::build();

  fn_scope->set_parent_function(print_fn_);
  fn_scope->set_return_type(Type::get_void());

  llvm::IRBuilder<>& bldr = fn_scope->builder();

  fn_scope->enter();
  bldr.CreateCall(cstdlib::printf(), { cstdlib::format<'s'>(), fn_print_str });
  fn_scope->exit();

  return print_fn_;
}

// TODO complete these
llvm::Function* Pointer::print_function() { return nullptr; }
llvm::Function* Tuple::print_function() { return nullptr; }

Type::time_loc Primitive::type_time() const {
  // Type::compile_time has a value of 1, and either_time has a value of 0
  // so we can use the casts bool -> int -> time_loc
  return static_cast<Type::time_loc>(static_cast<int>(this == Type::get_type()));
}

Type::time_loc Array::type_time() const {
  // has_dynamic_length() will either be 0 or 2.
  // As a Type::time_loc object, that's either "either_time"
  // or "run_time" respectively.
  return static_cast<Type::time_loc>(
    static_cast<int>(data_type()->type_time())
    |
    (static_cast<int>(has_dynamic_length()) << 1));
}

Type::time_loc Function::type_time() const {
  return static_cast<Type::time_loc>(
      static_cast<int>(argument_type()->type_time())
      |
      static_cast<int>(return_type()->type_time()));
}

Type::time_loc Pointer::type_time() const {
  // It's not allowed to be a pointer to a compile-time type,
  // but we need not check this here. This will be checked by
  // verify_types().
  return Type::run_time;
}

Type::time_loc Tuple::type_time() const {
  int output = Type::either_time;
  for (auto t : tuple_types_) {
    output |= t->type_time();
  }

  return static_cast<Type::time_loc>(output);
}
