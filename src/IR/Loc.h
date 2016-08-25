#ifndef ICARUS_IR_LOC_H
#define ICARUS_IR_LOC_H

struct Type;
namespace IR {
// Forward declarations
#define LOC_MACRO(TypeName, type_name, cpp_type) struct TypeName;
#include "../config/loc.conf"
#undef LOC_MACRO

struct Func;

struct Loc {
  Loc() {}
  virtual ~Loc() {}
  virtual std::string to_string() const = 0;
  virtual llvm::Value *
  llvm(IR::Func *ir_fn, const std::vector<llvm::Value *> &registers) const = 0;
  virtual Loc *clone() const = 0;

#define LOC_MACRO(TypeName, type_name, cpp_type)                               \
  virtual bool is_##type_name() const { return false; }                        \
  static TypeName *Create##TypeName(cpp_type);                                 \
  cpp_type Get##TypeName() const;

  #include "../config/loc.conf"
#undef LOC_MACRO

protected:
  Loc(const Loc&v) = default;
};

Order ArbitraryOrdering(const Loc *lhs, const Loc *rhs);

#define LOC_MACRO(TypeName, type_name, cpp_type)                               \
  struct TypeName : Loc {                                                      \
    TypeName() = delete;                                                       \
    TypeName(cpp_type x) : loc(x) {}                                           \
    virtual TypeName *clone() const { return new TypeName(*this); }            \
    virtual ~TypeName() {}                                                     \
    bool is_##type_name() const { return true; }                               \
    std::string to_string() const;                                             \
    llvm::Value *llvm(IR::Func *ir_fn,                                         \
                      const std::vector<llvm::Value *> &registers) const;      \
                                                                               \
    cpp_type loc;                                                              \
  };

#include "../config/loc.conf"
#undef LOC_MACRO
} // namespace IR

#endif // ICARUS_IR_LOC_H
