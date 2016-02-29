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
#include "Scope.h"
#include "TimeEval.h"
#include "Context.h"

extern llvm::DataLayout *data_layout;

namespace AST {
struct Expression;
struct Declaration;
struct EnumLiteral;
struct TypeLiteral;
} // namespace AST

struct Type;
struct Primitive;
struct Array;
struct Tuple;
struct Pointer;
struct Function;
struct Enumeration;
struct Structure;
struct DependentType;
struct TypeVariable;
struct ForwardDeclaration;

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
extern TypeVariable *TypeVar(AST::Identifier *id);
extern ForwardDeclaration *FwdDecl(AST::Expression *expr);

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
  virtual void generate_llvm() const ENDING;                                   \
  virtual bool add_llvm_input(std::vector<llvm::Type *> &llvm_in) ENDING;      \
  virtual void call_init(llvm::IRBuilder<> &bldr, llvm::Value *var) ENDING;    \
  virtual void call_repr(llvm::IRBuilder<> &bldr, llvm::Value *val) ENDING;    \
  virtual void call_uninit(llvm::IRBuilder<> &bldr, llvm::Value *var) ENDING

#define TYPE_FNS(name, checkname)                                              \
  name() = delete;                                                             \
  virtual ~name() {}                                                           \
  virtual bool is_##checkname() const { return true; }                         \
  BASIC_METHODS

struct Type {
public:
  Type() : assign_fn_(nullptr), llvm_type(nullptr), has_vars(false) {}
  virtual ~Type() {}
  BASIC_METHODS;

#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"

  friend struct ::Array;

  virtual operator llvm::Type *() const;

  size_t bytes() const;

  // Note: this one is special. It functions identically to the rest, but
  // it's special in that it will return nullptr if you haven't imported the
  // string library. This should never come up, because it's only used to add
  // type to a string literal, and using a string literal should import
  // strings.
  static Type *get_string();

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

  llvm::Function *assign_fn_;
  mutable llvm::Type *llvm_type;
  bool has_vars;
};

#undef ENDING
#define ENDING

struct Primitive : public Type {
public:
  TYPE_FNS(Primitive, primitive);
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"

  virtual void call_print(llvm::IRBuilder<> &bldr, llvm::Value *val);
  virtual llvm::Value *call_cast(llvm::IRBuilder<> &bldr, llvm::Value *val,
                                 Type *to_type);

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

  Primitive::TypeEnum type_;
  llvm::Function *repr_fn_;

  Primitive(TypeEnum pt);
};

struct Array : public Type {
  TYPE_FNS(Array, array);
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"

  virtual bool requires_uninit() const;
  virtual llvm::Value *call_cast(llvm::IRBuilder<> &bldr, llvm::Value *val,
                                 Type *to_type);

  llvm::Function *initialize();
  llvm::Value *initialize_literal(llvm::IRBuilder<> &bldr,
                                  llvm::Value *runtime_len = nullptr);

  llvm::Function *init_fn_, *uninit_fn_, *repr_fn_;

  Type *data_type;

  // Not the length of the array, but the dimension. That is, it's how many
  // times you can access an element.
  size_t dimension;

  Array(Type *t);
};

struct Tuple : public Type {
  TYPE_FNS(Tuple, tuple);
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"

  // TODO requires_uninit()

  virtual llvm::Value *allocate(llvm::IRBuilder<> &bldr) const;
  virtual llvm::Value *call_cast(llvm::IRBuilder<> &bldr, llvm::Value *val,
                                 Type *to_type);

  Tuple(const std::vector<Type *> &types);

  std::vector<Type *> entries;
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

  operator llvm::FunctionType *() const;

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

  void insert_field(const std::string &name, Type *ty,
                    AST::Expression *init_val);

  // Field database info
  std::map<std::string, size_t> field_name_to_num;
  std::vector<std::string> field_num_to_name;
  std::vector<Type *> field_type;
  std::map<size_t, size_t> field_num_to_llvm_num;

  std::vector<AST::Expression *> init_values;


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

  TypeVariable(AST::Identifier *id) : identifier(id) { has_vars = true; }

  AST::Identifier *identifier;
};

struct ForwardDeclaration : public Type {
  TYPE_FNS(ForwardDeclaration, fwd_decl);
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"

  static std::vector<Type*> forward_declarations;

  ForwardDeclaration(AST::Expression* expr) : expr(expr) {
    index = forward_declarations.size();
    forward_declarations.push_back(nullptr);
  }

  size_t index;
  AST::Expression* expr;
};



std::ostream &operator<<(std::ostream &os, const Type &t);

#undef TYPE_FNS
#undef BASIC_METHODS
#undef LEFT_UNARY_OPERATOR_MACRO
#undef BINARY_OPERATOR_MACRO
#undef ENDING

#endif // ICARUS_TYPE_H
