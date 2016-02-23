#ifndef ICARUS_TYPE_H
#define ICARUS_TYPE_H

#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <set>

// TODO Figure out what you need from this.
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#include "Language.h"
#include "typedefs.h"
#include "Scope.h"
#include "TimeEval.h"

extern llvm::DataLayout *data_layout;

namespace AST {
struct Expression;
struct Declaration;
struct EnumLiteral;
struct TypeLiteral;
} // namespace AST

class Type;
class Primitive;
class Array;
class Tuple;
struct Pointer;
struct Function;
struct Enumeration;
struct Structure;
struct DependentType;
struct TypeVariable;

namespace TypeSystem {
void initialize();
extern std::map<std::string, Type *> Literals;
extern Type *get_operator(Language::Operator op, Type *signature);
extern Type *get(const std::string &name);
} // namespace TypeSystem

extern Primitive *Error;
extern Primitive *Unknown;
extern Primitive *Bool;
extern Primitive *Char;
extern Primitive *Int;
extern Primitive *Real;
extern Primitive *Type_;
extern Primitive *Uint;
extern Primitive *Void;
extern Primitive *NullPtr;
extern Pointer *RawPtr;
extern Structure *String;

extern Pointer *Ptr(Type *t);
extern Array *Arr(Type *t);
extern Tuple *Tup(const std::vector<Type *> &t);
extern Function *Func(Type *in, Type *out);
extern Function *Func(std::vector<Type *> in, Type *out);
extern Function *Func(Type *in, std::vector<Type *> out);
extern Function *Func(std::vector<Type *> in, std::vector<Type *> out);
extern Enumeration *Enum(const std::string &name,
                         const AST::EnumLiteral *e = nullptr);
extern Structure *Struct(const std::string &name,
                         AST::TypeLiteral *expr = nullptr);
extern DependentType *DepType(std::function<Type *(Type *)> fn);
extern TypeVariable *TypeVar(IdPtr id);

#include "typedefs.h"

#define ENDING = 0

#define BINARY_OPERATOR_MACRO(op, symbol, prec, assoc)                         \
  virtual llvm::Value *call_##op(llvm::IRBuilder<> &bldr, llvm::Value *lhs,    \
                                 llvm::Value *rhs) ENDING;

#define LEFT_UNARY_OPERATOR_MACRO(op)                                          \
  virtual llvm::Value *call_##op(llvm::IRBuilder<> &bldr,                      \
                                 llvm::Value *operand) ENDING;

#define BASIC_METHODS                                                          \
  virtual llvm::Function *assign() ENDING;                                     \
  virtual std::string to_string() const ENDING;                                \
  virtual Time::Eval time() const ENDING;                                      \
  virtual bool add_llvm_input(std::vector<llvm::Type *> &llvm_in) ENDING;      \
  virtual void call_init(llvm::IRBuilder<> &bldr, llvm::Value *var) ENDING;    \
  virtual void call_repr(llvm::IRBuilder<> &bldr, llvm::Value *val) ENDING;    \
  virtual void call_uninit(llvm::IRBuilder<> &bldr, llvm::Value *var) ENDING

#define TYPE_FNS(name, checkname)                                              \
  name() = delete;                                                             \
  virtual ~name() {}                                                           \
  virtual bool is_##checkname() const { return true; }                         \
  BASIC_METHODS

class Type {
public:
  Type() : assign_fn_(nullptr), has_vars_(false) {}
  virtual ~Type() {}
  BASIC_METHODS;

#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"

  friend class ::Array;

  virtual operator llvm::Type *() const { return llvm_type_; }

  size_t bytes() const;
  bool has_variables() const { return has_vars_; }

  // Note: this one is special. It functions identically to the rest, but
  // it's special in that it will return nullptr if you haven't imported the
  // string library. This should never come up, because it's only used to add
  // type to a string literal, and using a string literal should import
  // strings.
  static Type *get_string();

  static std::map<std::string, Type *> literals;

  virtual llvm::Value *allocate(llvm::IRBuilder<> &bldr) const {
    return bldr.CreateAlloca(*this);
  }

  virtual void call_print(llvm::IRBuilder<> &bldr, llvm::Value *val) {
    call_repr(bldr, val);
  }

  virtual llvm::Value *call_cast(llvm::IRBuilder<> &bldr, llvm::Value *val,
                                 Type *to_type) {
    return nullptr;
  }

  virtual bool requires_uninit() const { return false; }

  virtual bool is_primitive() const { return false; }
  virtual bool is_array() const { return false; }
  virtual bool is_tuple() const { return false; }
  virtual bool is_pointer() const { return false; }
  virtual bool is_function() const { return false; }
  virtual bool is_struct() const { return false; }
  virtual bool is_enum() const { return false; }
  virtual bool is_dependent_type() const { return false; }
  virtual bool is_type_variable() const { return false; }

  llvm::Type *llvm() const { return llvm_type_; }

  llvm::Function *assign_fn_;
  llvm::Type *llvm_type_;
  bool has_vars_;
};

#undef ENDING
#define ENDING

class Primitive : public Type {
public:
  TYPE_FNS(Primitive, primitive);
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"

  friend void TypeSystem::initialize();

  virtual void call_print(llvm::IRBuilder<> &bldr, llvm::Value *val);
  virtual llvm::Value *call_cast(llvm::IRBuilder<> &bldr, llvm::Value *val,
                                 Type *to_type);

private:
  enum class TypeEnum {
    Error,
    Unknown,
    Bool,
    Char,
    Int,
    Real,
    Type,
    Uint,
    Void,
    NullPtr
  };

  Primitive(TypeEnum pt);

  Primitive::TypeEnum type_;
  llvm::Function *repr_fn_;
};

class Array : public Type {
public:
  TYPE_FNS(Array, array);
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"

  friend Array *Arr(Type *);

  friend struct AST::Declaration;
  friend class Type;

  virtual bool requires_uninit() const;
  virtual llvm::Value *call_cast(llvm::IRBuilder<> &bldr, llvm::Value *val,
                                 Type *to_type);

  virtual Type *data_type() const { return type_; }
  virtual size_t dim() const { return dim_; }

  llvm::Function *initialize();
  llvm::Value *initialize_literal(llvm::IRBuilder<> &bldr,
                                  llvm::Value *runtime_len = nullptr);

private:
  Array(Type *t);

  // Not the length of the array, but the dimension. That is, it's how many
  // times you can access an element.
  size_t dim_;

  llvm::Function *init_fn_, *uninit_fn_, *repr_fn_;

  Type *type_;
};

class Tuple : public Type {
public:
  TYPE_FNS(Tuple, tuple);
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"

  friend Tuple *Tup(const std::vector<Type *> &);

  std::vector<Type *> entry_types() { return entry_types_; }

  // TODO requires_uninit()

  virtual llvm::Value *allocate(llvm::IRBuilder<> &bldr) const;
  virtual llvm::Value *call_cast(llvm::IRBuilder<> &bldr, llvm::Value *val,
                                 Type *to_type);

  size_t size() const { return entry_types_.size(); }

private:
  Tuple(const std::vector<Type *> &types);

  std::vector<Type *> entry_types_;
};

struct Pointer : public Type {
  TYPE_FNS(Pointer, pointer);
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"

  virtual llvm::Value *call_cast(llvm::IRBuilder<> &bldr, llvm::Value *val,
                                 Type *to_type);
  Pointer(Type *t);
  Type *pointee;
};

struct Function : public Type {
  TYPE_FNS(Function, function);
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"

  operator llvm::FunctionType *() const {
    return static_cast<llvm::FunctionType *>(llvm_type_);
  }

  virtual llvm::Value *allocate(llvm::IRBuilder<> &bldr) const;
  virtual llvm::Value *call_cast(llvm::IRBuilder<> &bldr, llvm::Value *val,
                                 Type *to_type);

  Function(Type *in, Type *out);
  Type *input, *output;
};

struct Enumeration : public Type {
  TYPE_FNS(Enumeration, enum);
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"

  llvm::Value *get_value(const std::string &str) const;

  Enumeration(const std::string &name, const AST::EnumLiteral *enumlit);

  std::string bound_name;
  llvm::Function *repr_fn_;
  std::map<std::string, llvm::Value *> int_values;
  llvm::GlobalVariable *string_data;
};

struct Structure : public Type {
  TYPE_FNS(Structure, struct);
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"

  Structure(const std::string &name, AST::TypeLiteral *expr);

  virtual bool requires_uninit() const;

  void set_name(const std::string &name);

  virtual llvm::Value *call_cast(llvm::IRBuilder<> &bldr, llvm::Value *val,
                                 Type *t);

  Type *field(const std::string &name) const;
  llvm::Value *field_num(const std::string &name) const;

  virtual void call_print(llvm::IRBuilder<> &bldr, llvm::Value *val);
  void set_print(llvm::Function *fn);

  AST::TypeLiteral *ast_expression;
  std::string bound_name;

  std::vector<std::pair<std::string, Type *>> fields;

private:
  llvm::Function *init_fn_, *uninit_fn_, *print_fn_;
};

struct DependentType : public Type {
  TYPE_FNS(DependentType, dependent_type);
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"

  DependentType(std::function<Type *(Type *)> fn) : func(fn) {}

  Type *operator()(Type *t) const { return func(t); }

  std::function<Type *(Type *)> func;
};

struct TypeVariable : public Type {
  TYPE_FNS(TypeVariable, type_variable);
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"

  TypeVariable(IdPtr id) : identifier(id) { has_vars_ = true; }

  IdPtr identifier;
};

std::ostream &operator<<(std::ostream &os, const Type &t);

#undef TYPE_FNS
#undef BASIC_METHODS
#undef LEFT_UNARY_OPERATOR_MACRO
#undef BINARY_OPERATOR_MACRO
#undef ENDING

#endif // ICARUS_TYPE_H
