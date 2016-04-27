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
#include "TypePtr.h"

extern llvm::IRBuilder<> builder;
extern llvm::DataLayout *data_layout;

namespace AST {
struct Expression;
struct Declaration;
struct EnumLiteral;
struct StructLiteral;
} // namespace AST

struct Type;
struct Primitive;
struct Array;
struct Tuple;
struct Pointer;
struct Function;
struct Enumeration;
struct Structure;
struct ParametricStructure;
struct DependentType;
struct TypeVariable;
struct QuantumType;
struct RangeType;

struct Scope;
// TODO this is not the right API for mangling.
extern std::string Mangle(const Type *t, bool prefix = true);
extern std::string Mangle(const Function *f, AST::Expression *expr,
                          Scope *starting_scope = nullptr);

namespace TypeSystem {
void initialize();
extern std::map<std::string, TypePtr> Literals;
extern TypePtr get_operator(Language::Operator op, TypePtr signature);
extern TypePtr get(const std::string &name);
} // namespace TypeSystem

extern TypePtr Error;
extern TypePtr Unknown;
extern TypePtr Bool;
extern TypePtr Char;
extern TypePtr Int;
extern TypePtr Real;
extern TypePtr Type_;
extern TypePtr Uint;
extern TypePtr Void;
extern TypePtr NullPtr;
extern TypePtr RawPtr;
extern TypePtr String;

extern Pointer *Ptr(TypePtr t);
extern Array *Arr(TypePtr t);
extern Tuple *Tup(const std::vector<TypePtr> &t);
extern Function *Func(TypePtr in, TypePtr out);
extern Function *Func(std::vector<TypePtr> in, TypePtr out);
extern Function *Func(TypePtr in, std::vector<TypePtr> out);
extern Function *Func(std::vector<TypePtr> in, std::vector<TypePtr> out);
extern Enumeration *Enum(const std::string &name,
                         const AST::EnumLiteral *e = nullptr);
extern Structure *Struct(const std::string &name,
                         AST::StructLiteral *expr = nullptr);
extern ParametricStructure *ParamStruct(const std::string &name,
                                        AST::StructLiteral *expr = nullptr);
extern DependentType *DepType(std::function<TypePtr(TypePtr)> fn);
extern TypeVariable *TypeVar(AST::Identifier *id, AST::Expression *test = nullptr);
extern QuantumType *Quantum(const std::vector<TypePtr>& vec);
extern RangeType *Range(TypePtr t);

#define ENDING = 0

#define BASIC_METHODS                                                          \
  virtual std::string to_string() const ENDING;                                \
  virtual Time::Eval time() const ENDING;                                      \
  virtual void generate_llvm() const ENDING;                                   \
  virtual bool add_llvm_input(std::vector<llvm::Type *> &llvm_in) ENDING;      \
  virtual void call_init(llvm::Value *var) ENDING;                             \
  virtual void call_repr(llvm::Value *val) ENDING                              \

#define TYPE_FNS(name, checkname)                                              \
  name() = delete;                                                             \
  virtual ~name() {}                                                           \
  virtual bool is_##checkname() const { return true; }                         \
  BASIC_METHODS

struct Type {
public:
  Type() : llvm_type(nullptr), has_vars(false) {}
  virtual ~Type() {}
  BASIC_METHODS;

  virtual operator llvm::Type *() const;

  size_t bytes() const;
  size_t alignment() const;

  // Assigns val to var. Assume that the types match appropriately. Depending on
  // the types, this will either simply be a store operation or a call to the
  // assignment function.
  void CallAssignment(Scope *scope, llvm::Value *val, llvm::Value *var);
  void CallDestroy(Scope *scope, llvm::Value *var);

  // Note: this one is special. It functions identically to the rest, but it's
  // special in that it will return nullptr if you haven't imported the string
  // library. This should never come up, because it's only used to add type to a
  // string literal, and using a string literal should import strings.
  static TypePtr get_string();

  virtual llvm::Value *allocate() const { return builder.CreateAlloca(*this); }

  virtual bool requires_uninit() const { return false; }

  virtual bool is_primitive() const { return false; }
  virtual bool is_array() const { return false; }
  virtual bool is_tuple() const { return false; }
  virtual bool is_pointer() const { return false; }
  virtual bool is_function() const { return false; }
  virtual bool is_struct() const { return false; }
  virtual bool is_parametric_struct() const { return false; }
  virtual bool is_enum() const { return false; }
  virtual bool is_dependent_type() const { return false; }
  virtual bool is_type_variable() const { return false; }
  virtual bool is_quantum() const { return false; }
  virtual bool is_range() const { return false; }

  virtual bool is_big() const;
  virtual bool stores_data() const;

  mutable llvm::Type *llvm_type;
  bool has_vars;
};

#undef ENDING
#define ENDING

struct Primitive : public Type {
public:
  TYPE_FNS(Primitive, primitive);

  enum class TypeEnum {
    Error, Unknown, Bool, Char, Int, Real, Type, Uint, Void, NullPtr
  };

  Primitive::TypeEnum type_;
  llvm::Function *repr_fn_;

  Primitive(TypeEnum pt);
};

struct Array : public Type {
  TYPE_FNS(Array, array);

  virtual bool requires_uninit() const;

  llvm::Function *initialize();
  llvm::Value *initialize_literal(llvm::Value *alloc, size_t len);
  llvm::Value *initialize_literal(llvm::Value *alloc, llvm::Value *len);

  virtual llvm::Value *allocate() const;

  llvm::Function *assign();
  llvm::Function *destroy();
  
  llvm::Function *init_fn_, *destroy_fn_, *repr_fn_, *assign_fn_;

  TypePtr data_type;

  // Not the length of the array, but the dimension. That is, it's how many
  // times you can access an element.
  size_t dimension;

  Array(TypePtr t);
};

struct Tuple : public Type {
  TYPE_FNS(Tuple, tuple);

  // TODO requires_uninit()

  virtual llvm::Value *allocate() const;

  Tuple(const std::vector<TypePtr> &types);

  std::vector<TypePtr> entries;
};

struct Pointer : public Type {
  TYPE_FNS(Pointer, pointer);

  Pointer(TypePtr t);
  TypePtr pointee;
};

struct Function : public Type {
  TYPE_FNS(Function, function);

  operator llvm::FunctionType *() const;

  virtual llvm::Value *allocate() const;

  Function(TypePtr in, TypePtr out);
  TypePtr input, output;
};

struct Enumeration : public Type {
  TYPE_FNS(Enumeration, enum);

  size_t get_index(const std::string &str) const;
  llvm::Value *get_value(const std::string &str) const;

  Enumeration(const std::string &name, const AST::EnumLiteral *enumlit);

  std::string bound_name;
  llvm::Function *repr_fn_;
  std::map<std::string, size_t> int_values;
  llvm::GlobalVariable *string_data;
};

struct Structure : public Type {
  TYPE_FNS(Structure, struct);

  Structure(const std::string &name, AST::StructLiteral *expr);

  virtual bool requires_uninit() const;

  void set_name(const std::string &name);

  TypePtr field(const std::string &name) const;
  llvm::Value *field_num(const std::string &name) const;

  AST::StructLiteral *ast_expression;
  std::string bound_name;

  void insert_field(const std::string &name, TypePtr ty,
                    AST::Expression *init_val);

  // Field database info
  std::map<std::string, size_t> field_name_to_num;
  std::vector<std::string> field_num_to_name;
  std::vector<TypePtr> field_type;
  std::map<size_t, size_t> field_num_to_llvm_num;

  std::vector<AST::Expression *> init_values;

  llvm::Function *assign();
  llvm::Function *destroy();

private:
  llvm::Function *init_fn_, *destroy_fn_, *assign_fn_;
};

struct ParametricStructure : public Type {
  TYPE_FNS(ParametricStructure, parametric_struct);

  ParametricStructure(const std::string &name, AST::StructLiteral *expr);

  void set_name(const std::string &name);

  AST::StructLiteral *ast_expression;
  std::string bound_name;
};

struct DependentType : public Type {
  TYPE_FNS(DependentType, dependent_type);

  DependentType(std::function<TypePtr(TypePtr)> fn) : func(fn) {}

  TypePtr operator()(TypePtr t) const { return func(t); }

  std::function<TypePtr(TypePtr)> func;
};

struct TypeVariable : public Type {
  TYPE_FNS(TypeVariable, type_variable);

  TypeVariable(AST::Identifier *id, AST::Expression *test)
      : identifier(id), test(test) {
    has_vars = true;
  }

  AST::Identifier *identifier;
  AST::Expression *test;
};

struct QuantumType : public Type {
  TYPE_FNS(QuantumType, quantum);

  QuantumType(const std::vector<TypePtr>& vec);
  // TODO maybe quantum types should only hold functions?
  std::vector<TypePtr> options;
};

struct RangeType : public Type {
  TYPE_FNS(RangeType, range);

  RangeType(TypePtr t) : end_type(t) { has_vars = end_type.get->has_vars; }

  TypePtr end_type;
};

std::ostream &operator<<(std::ostream &os, const Type &t);

#undef TYPE_FNS
#undef BASIC_METHODS
#undef ENDING

#endif // ICARUS_TYPE_H
