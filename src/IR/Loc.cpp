#include "Loc.h"


namespace IR {
extern std::vector<llvm::Value *> LLVMGlobals;

#define LOC_MACRO(TypeName, type_name, cpp_type)                               \
  cpp_type Loc::Get##TypeName() const {                                        \
    assert(is_##type_name());                                                  \
    return ((TypeName *)this)->loc;                                            \
  }

  #include "../config/loc.conf"
#undef LOC_MACRO

std::string Arg::to_string() const { return "a." + std::to_string(loc); }
std::string Reg::to_string() const { return "r." + std::to_string(loc); }
std::string StackAddr::to_string() const { return "s." + std::to_string(loc); }
std::string FrameAddr::to_string() const { return "f." + std::to_string(loc); }
std::string GlobalAddr::to_string() const { return "g." + std::to_string(loc); }

llvm::Value *Arg::llvm(IR::Func *ir_fn,
                       const std::vector<llvm::Value *> &registers) const {
  auto arg_num = loc;
  auto iter = ir_fn->llvm_fn->args().begin();
  while (arg_num-- > 0) { iter++; }
  return iter;
}

llvm::Value *Reg::llvm(IR::Func *ir_fn,
                       const std::vector<llvm::Value *> &registers) const {
  return registers[loc];
}

llvm::Value *
StackAddr::llvm(IR::Func *ir_fn,
                const std::vector<llvm::Value *> &registers) const {
  ir_fn->dump();
  UNREACHABLE;
}

llvm::Value *
FrameAddr::llvm(IR::Func *ir_fn,
                const std::vector<llvm::Value *> &registers) const {
  assert(ir_fn->frame_map.find(loc) != ir_fn->frame_map.end());
  return ir_fn->frame_map[loc];
}

llvm::Value *
GlobalAddr::llvm(IR::Func *ir_fn,
                 const std::vector<llvm::Value *> &registers) const {
  return LLVMGlobals[loc];
}

Order ArbitraryOrdering(const Loc *lhs, const Loc *rhs) {
#define LOC_MACRO(TypeName, type_name, cpp_type)                               \
  if (lhs->is_##type_name() && rhs->is_##type_name()) {                        \
    if (((TypeName *)lhs)->loc < ((TypeName *)rhs)->loc) {                     \
      return Order::Less;                                                      \
    }                                                                          \
    if (((TypeName *)lhs)->loc < ((TypeName *)rhs)->loc) {                     \
      return Order::Greater;                                                   \
    }                                                                          \
    return Order::Equal;                                                       \
  }                                                                            \
  if (lhs->is_##type_name()) { return Order::Less; }                           \
  if (rhs->is_##type_name()) { return Order::Greater; }

#include "../config/loc.conf"
#undef LOC_MACRO
  UNREACHABLE;
}
} // namespace IR
