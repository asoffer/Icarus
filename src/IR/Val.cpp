#include "Val.h"
#include "Type/Type.h"

extern std::string Escape(char c);
extern llvm::IRBuilder<> builder;

namespace data {
extern llvm::Constant *null(const Type *t);
extern llvm::ConstantInt *const_bool(bool b);
extern llvm::ConstantInt *const_uint(size_t n);
extern llvm::ConstantInt *const_u16(uint16_t n);
extern llvm::ConstantInt *const_u32(uint32_t n);
extern llvm::ConstantInt *const_int(long n);
extern llvm::ConstantInt *const_char(char c);
extern llvm::ConstantFP *const_real(double d);
extern llvm::Constant *null_pointer(Type *t);
} // namespace data

namespace IR {
#define VAL_MACRO(TypeName, type_name, cpp_type)                               \
  cpp_type Val::Get##TypeName() const {                                        \
    assert(is_##type_name());                                                  \
    return ((TypeName##Val *)this)->val;                                       \
  }

#include "../config/val.conf"
#undef VAL_MACRO

llvm::Constant *BoolVal::llvm() { return data::const_bool(val); }
llvm::Constant *CharVal::llvm() { return data::const_char(val); }
llvm::Constant *RealVal::llvm() { return data::const_real(val); }
llvm::Constant *U16Val::llvm() { return data::const_u16(val); }
llvm::Constant *U32Val::llvm() { return data::const_u32(val); }
llvm::Constant *StringVal::llvm() { NOT_YET; }
llvm::Constant *TypeVal::llvm() {
  return reinterpret_cast<llvm::Constant *>(val);
}
llvm::Constant *IntVal::llvm() { return data::const_int(val); }
llvm::Constant *UintVal::llvm() { return data::const_uint(val); }
llvm::Constant *FuncVal::llvm() {
  // The field 'val' could be null, representing a default-initialized function.
  if (!val) { return data::null_pointer(Char); }
  auto ip = builder.saveIP();
  val->GenerateLLVM();
  builder.restoreIP(ip);

  assert(val->llvm_fn);
  return val->llvm_fn;
}
llvm::Constant *NullVal::llvm() { return data::null(val); }
llvm::Constant *ScopeVal::llvm() { UNREACHABLE; }
llvm::Constant *CodeVal::llvm() { UNREACHABLE; }

std::string BoolVal::to_string() const { return val ? "true" : "false"; }
std::string CharVal::to_string() const { return "'" + Escape(val) + "'"; }
std::string RealVal::to_string() const { return std::to_string(val); }
std::string U16Val::to_string() const { return std::to_string(val) + "u16"; }
std::string U32Val::to_string() const { return std::to_string(val) + "u32"; }
std::string TypeVal::to_string() const { return val->to_string(); }
std::string IntVal::to_string() const { return std::to_string(val); }
std::string StringVal::to_string() const { return val; }
std::string NullVal::to_string() const {
  return "null(" + val->to_string() + ")";
}
std::string UintVal::to_string() const { return std::to_string(val) + "u"; }
std::string FuncVal::to_string() const {
  std::stringstream ss;
  ss << "fn.";
  if (!val) {
    ss << "null";
  } else if (val->GetName() != "") {
    ss << val->GetName();
  } else {
    ss << val;
  }
  return ss.str();
}
std::string ScopeVal::to_string() const { return "scope"; }
std::string CodeVal::to_string() const { return "code"; }

Order ArbitraryOrdering(const Val *lhs, const Val *rhs) {
#define VAL_MACRO(TypeName, type_name, cpp_type)                               \
  if (lhs->is_##type_name() && rhs->is_##type_name()) {                        \
    if (((TypeName##Val *)lhs)->val < ((TypeName##Val *)rhs)->val) {           \
      return Order::Less;                                                      \
    }                                                                          \
    if (((TypeName##Val *)lhs)->val > ((TypeName##Val *)rhs)->val) {           \
      return Order::Greater;                                                   \
    }                                                                          \
    return Order::Equal;                                                       \
  }                                                                            \
  if (lhs->is_##type_name()) { return Order::Less; }                           \
  if (rhs->is_##type_name()) { return Order::Greater; }

#include "../config/val.conf"
#undef VAL_MACRO
  UNREACHABLE;
}

} // namespace IR
