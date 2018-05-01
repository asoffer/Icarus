#ifndef ICARUS_TYPE_TYPE_H
#define ICARUS_TYPE_TYPE_H

#include <string>

#include "base/debug.h"
#include "base/util.h"

#include "ir/val.h"

#ifdef ICARUS_USE_LLVM
namespace llvm {
class Type;
class LLVMContext;
} // namespace llvm
#endif // ICARUS_USE_LLVM

#define TYPE_FNS(name)                                                         \
  name() = delete;                                                             \
  virtual ~name() {}                                                           \
  BASIC_METHODS

// TODO pass by reference is probably what you want for IR::Val anyway and has
// the added benefit of allowing you to forward declare Val
#define ENDING = 0
#define BASIC_METHODS_WITHOUT_LLVM                                             \
  virtual char *WriteTo(char *buf) const ENDING;                               \
  virtual size_t string_size() const ENDING;                                   \
  virtual void EmitAssign(const Type *from_type, IR::Val from, IR::Val to,     \
                          Context *ctx) const ENDING;                          \
  virtual void EmitInit(IR::Val id_val, Context *ctx) const ENDING;            \
  virtual void EmitDestroy(IR::Val id_val, Context *ctx) const ENDING;         \
  virtual IR::Val EmitInitialValue(Context *ctx) const ENDING;                 \
  virtual IR::Val PrepareArgument(const Type *t, const IR::Val &val,           \
                                  Context *ctx) const ENDING;                  \
  virtual void EmitRepr(IR::Val id_val, Context *ctx) const ENDING;            \
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

void EmitCopyInit(const Type *from_type, const Type *to_type, IR::Val from_val,
                  IR::Val to_var, Context*ctx);
void EmitMoveInit(const Type *from_type, const Type *to_type, IR::Val from_val,
                  IR::Val to_var, Context*ctx);

extern Type *Err, *Bool, *Char, *Int, *Real, *Code, *Type_, *Void, *NullPtr,
       *String, *EmptyArray, *Generic;
} // namespace type

#undef ENDING
#define ENDING

#endif // ICARUS_TYPE_TYPE_H

