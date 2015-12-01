#include "Type.h"
#include "AST.h"

namespace cstdlib {
  extern llvm::Constant* malloc();
}  // namespace cstdlib

namespace data {
  extern llvm::Value* const_int(size_t n, bool is_signed = false);
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


llvm::Value* Array::make(llvm::IRBuilder<>& bldr, llvm::Value* len) {
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
