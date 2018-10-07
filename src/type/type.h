#ifndef ICARUS_TYPE_TYPE_H
#define ICARUS_TYPE_TYPE_H

#include <string>

#include "base/debug.h"
#include "base/util.h"
#include "ir/register.h"

struct Context;

#ifdef ICARUS_USE_LLVM
namespace llvm {
class Type;
class LLVMContext;
}  // namespace llvm
#endif  // ICARUS_USE_LLVM

struct Module;

namespace IR {
struct Val;
}  // namespace IR

#define TYPE_FNS(name)                                                         \
  name() = delete;                                                             \
  virtual ~name() {}                                                           \
  BASIC_METHODS

#define ENDING = 0
#define BASIC_METHODS_WITHOUT_LLVM                                             \
  virtual char *WriteTo(char *buf) const ENDING;                               \
  virtual size_t string_size() const ENDING;                                   \
  virtual void EmitAssign(const Type *from_type, IR::Val from,                 \
                          IR::Register to, Context *ctx) const ENDING;         \
  virtual void EmitInit(IR::Register reg, Context *ctx) const ENDING;          \
  virtual void EmitDestroy(IR::Register reg, Context *ctx) const ENDING;       \
  virtual IR::Val PrepareArgument(const Type *t, const IR::Val &val,           \
                                  Context *ctx) const ENDING;                  \
  virtual void EmitRepr(IR::Val const &id_val, Context *ctx) const ENDING;     \
  virtual Cmp Comparator() const ENDING

#ifdef ICARUS_USE_LLVM
#define BASIC_METHODS                                                          \
  BASIC_METHODS_WITHOUT_LLVM;                                                  \
  virtual llvm::Type *llvm(llvm::LLVMContext &) const ENDING
#else
#define BASIC_METHODS BASIC_METHODS_WITHOUT_LLVM
#endif

namespace type {
// Note: the order of these is meaningful and relied upon!
enum class Cmp : u8 { None, Equality, Order };

struct Type : public base::Cast<Type> {
 public:
  Type() {}
  virtual ~Type() {}
  BASIC_METHODS;

  std::string to_string() const {
    std::string result(string_size(), '\0');
    char *end_buf = WriteTo(result.data());
    ASSERT(static_cast<size_t>(end_buf - result.data()) == result.size());
    return result;
  }

  bool is_big() const;
  virtual bool needs_destroy() const { return false; }
};

const Type *Meet(const Type *lhs, const Type *rhs);
const Type *Join(const Type *lhs, const Type *rhs);
bool CanCastImplicitly(const type::Type *from, const type::Type *to);

void EmitCopyInit(const Type *from_type, const Type *to_type, IR::Val from_val,
                  IR::Register to_var, Context *ctx);
void EmitMoveInit(const Type *from_type, const Type *to_type, IR::Val from_val,
                  IR::Register to_var, Context *ctx);

const Type *Void();
extern Type const *Err, *Bool, *Char, *Int, *Real, *Code, *Type_, *NullPtr,
    *EmptyArray, *Generic, *Module, *Block, *OptBlock, *Interface;

template <typename T>
constexpr type::Type const *Get() {
  if constexpr (std::is_same_v<T, bool>) {
    return type::Bool;
  } else if constexpr (std::is_same_v<T, char>) {
    return type::Char;
  } else if constexpr (std::is_same_v<T, i32>) {
    return type::Int;
  } else if constexpr (std::is_same_v<T, double>) {
    return type::Real;
  } else if constexpr (std::is_same_v<
                           std::decay_t<decltype(*std::declval<T>())>,
                           type::Type>) {
    return type::Type_;
  } else if constexpr (std::is_same_v<
                           std::decay_t<decltype(*std::declval<T>())>,
                           ::Module>) {
    return type::Module;
  } else {
    NOT_YET();
  }
}

}  // namespace type

#undef ENDING
#define ENDING

#endif  // ICARUS_TYPE_TYPE_H
