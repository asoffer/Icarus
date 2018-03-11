#ifndef ICARUS_TYPE_TYPE_H
#define ICARUS_TYPE_TYPE_H

struct Type;
struct Struct;
struct Array;
struct Pointer;
struct Tuple;
struct Function;
struct RangeType;
struct SliceType;
struct Scope_Type;
struct Variant;

extern Type *Err, *Unknown, *Bool, *Char, *Int, *Real, *Code, *Type_, *Void,
    *NullPtr, *String, *EmptyArray, *Generic;

#include "../ast/ast.h"
#include "../base/debug.h"
#include "../base/types.h"
#include "../base/util.h"

#include <iosfwd>
#include <string>
#include <unordered_map>
#include <vector>

struct Context;

namespace IR {
struct Func;
} // namespace IR

namespace AST {
struct Declaration;
struct Expression;
struct Identifier;
} // namespace AST

#define ENDING = 0

// TODO names for argumentss of built-in functions (repr, assign, destroy, etc)
// should be better? Should be necessarily unnamed? Figure it out and be
// consistent.

#define BASIC_METHODS                                                          \
  virtual char *WriteTo(char *buf) const ENDING;                               \
  virtual size_t string_size() const ENDING;                                   \
  virtual void EmitAssign(const Type *from_type, IR::Val from, IR::Val to)     \
      const ENDING;                                                            \
  virtual void EmitInit(IR::Val id_val) const ENDING;                          \
  virtual void EmitDestroy(IR::Val id_val) const ENDING;                       \
  virtual IR::Val EmitInitialValue() const ENDING;                             \
  virtual IR::Val PrepareArgument(const Type *t, const IR::Val &val)           \
      const ENDING;                                                            \
  virtual void EmitRepr(IR::Val id_val) const ENDING

#define TYPE_FNS(name)                                                         \
  name() = delete;                                                             \
  virtual ~name() {}                                                           \
  BASIC_METHODS

struct Type : public base::Cast<Type> {
public:
  Type() {}
  virtual ~Type() {}
  BASIC_METHODS;

  static const Type *Meet(const Type *lhs, const Type *rhs);

  std::string to_string() const {
    std::string result(string_size(), '\0');
    char *end_buf = WriteTo(result.data());
    ASSERT_EQ(static_cast<size_t>(end_buf - result.data()), result.size());
    return result;
  }

  static void EmitMoveInit(const Type *from_type, const Type *to_type,
                           IR::Val from_val, IR::Val to_var);
  static void EmitCopyInit(const Type *from_type, const Type *to_type,
                           IR::Val from_val, IR::Val to_var);

  bool is_big() const { return is<Array>() || is<Struct>() || is<Variant>(); }
  virtual bool needs_destroy() const { return false; }
};

#undef ENDING
#define ENDING

enum class PrimType : char {
#define PRIMITIVE_MACRO(GlobalName, EnumName, name) EnumName,
#include "../config/primitive.conf"
#undef PRIMITIVE_MACRO
};

struct Primitive : public Type {
public:
  TYPE_FNS(Primitive);
  Primitive(PrimType pt) : type_(pt) {}

private:
  friend class Architecture;
  PrimType type_;

  mutable IR::Func *repr_func_ = nullptr;
};

struct Array : public Type {
  TYPE_FNS(Array);
  Array(const Type *t) : data_type(t), len(0), fixed_length(false) {}
  Array(const Type *t, size_t l) : data_type(t), len(l), fixed_length(true) {}

  static IR::Val Compare(const Array *lhs_type, IR::Val lhs_ir,
                         const Array *rhs_type, IR::Val rhs_ir, bool equality);

  virtual bool needs_destroy() const {
    return !fixed_length || data_type->needs_destroy();
  }

  mutable IR::Func *repr_func_ = nullptr, *init_func_ = nullptr;

  const Type *data_type;
  size_t len;
  bool fixed_length;

private:
  mutable std::unordered_map<const Array *, IR::Func *> assign_fns_;
  mutable IR::Func *destroy_func_ = nullptr;
};

struct Tuple : public Type {
  TYPE_FNS(Tuple);
  Tuple(std::vector<const Type *> entries) : entries(std::move(entries)) {}

  virtual bool needs_destroy() const {
    for (const Type *t : entries) {
      if (t->needs_destroy()) { return true; }
    }
    return false;
  }

  std::vector<const Type *> entries;
};

struct Pointer : public Type {
  TYPE_FNS(Pointer);
  Pointer(const Type *t) : pointee(t) {}
  const Type *pointee;
};

struct Function : public Type {
  TYPE_FNS(Function);
  Function(std::vector<const Type *> in, std::vector<const Type *> out)
      : input(in), output(out) {}

  const Function* ToIR() const;
  std::vector<const Type *> input, output;
};

struct Enum : public Type {
  TYPE_FNS(Enum);
  Enum(const std::string &name, std::vector<std::string> members, bool is_enum);

  size_t IntValueOrFail(const std::string &str) const;
  IR::Val EmitLiteral(const std::string &member_name) const;

  // TODO combine "members" and "int_values" to save the double allocation of
  // strings.
  std::string bound_name;
  std::vector<std::string> members_;
  std::unordered_map<std::string, size_t> int_values;
  bool is_enum_;
};

struct Struct : public Type {
  virtual ~Struct() {}
  BASIC_METHODS;

  struct Field {
    std::string_view name;
    const Type *type = nullptr;
    IR::Val init_val;
  };

  // Return the type of a field, or a nullptr if it doesn't exist
  const Field *field(const std::string &name) const;

  virtual bool needs_destroy() const {
    for (const auto &field : fields_) {
      if (field.type->needs_destroy()) { return true; }
    }
    return false;
  }

  const Type *finalize();

  std::vector<Field> fields_;
  std::unordered_map<std::string, size_t> field_indices_;

private:
  mutable IR::Func *init_func_ = nullptr, *assign_func = nullptr,
                   *destroy_func_ = nullptr, *repr_func_ = nullptr;
};

struct Variant : public Type {
  TYPE_FNS(Variant);
  Variant(std::vector<const Type *> variants)
      : variants_(std::move(variants)) {}
  size_t size() const { return variants_.size(); }

  std::vector<const Type *> variants_;
  private:
  mutable IR::Func *repr_func_ = nullptr;
};

struct RangeType : public Type {
  TYPE_FNS(RangeType);

  RangeType(const Type *t) : end_type(t) {}

  const Type *end_type;
};

struct SliceType : public Type {
  TYPE_FNS(SliceType);

  SliceType(const Array *a) : array_type(a) {}

  const Array *array_type;
};

struct Scope_Type : public Type {
  TYPE_FNS(Scope_Type);

  Scope_Type(const Type *t) : type_(t) {}
  const Type *type_;

  std::string bound_name;
};

inline std::ostream &operator<<(std::ostream &os, const Type &t) {
  return os << t.to_string();
}

#undef TYPE_FNS
#undef BASIC_METHODS
#undef ENDING

const Pointer *Ptr(const Type *t);
const Array *Arr(const Type *t);
const Array *Arr(const Type *t, size_t len);
const Type *Tup(std::vector<const Type *> types);
const Function *Func(const Type *in, const Type *out);
const Function *Func(std::vector<const Type *> in, const Type *out);
const Function *Func(const Type *in, std::vector<Type *> out);
const Function *Func(std::vector<const Type *> in,
                     std::vector<const Type *> out);
const RangeType *Range(const Type *t);
const SliceType *Slice(const Array *a);
const Scope_Type *ScopeType(const Type *t);
const Type *Var(std::vector<const Type *> variants);

IR::Val PtrCallFix(IR::Val v);

#endif // ICARUS_TYPE_TYPE_H
