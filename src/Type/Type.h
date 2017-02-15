#ifndef ICARUS_TYPE_H
#define ICARUS_TYPE_H
#include "Scope.h"

extern std::vector<IR::Func *> implicit_functions;
// TODO this is not the right API for mangling.
extern std::string Mangle(const Type *t, bool prefix = true);
extern std::string Mangle(const Function *f, AST::Expression *expr,
                          Scope *starting_scope = nullptr);

extern Type *Err, *Unknown, *Bool, *Char, *Int, *Real, *Code_, *Type_, *Uint,
    *Void, *NullPtr, *RawPtr, *String, *U16, *U32;

extern Pointer *Ptr(Type *t);
extern Array *Arr(Type *t);
extern Array *Arr(Type *t, size_t len);
extern Tuple *Tup(const std::vector<Type *> &t);
extern Function *Func(Type *in, Type *out);
extern Function *Func(std::vector<Type *> in, Type *out);
extern Function *Func(Type *in, std::vector<Type *> out);
extern Function *Func(std::vector<Type *> in, std::vector<Type *> out);
extern TypeVariable *TypeVar(AST::Identifier *id,
                             AST::Expression *test = nullptr);
extern RangeType *Range(Type *t);
extern SliceType *Slice(Array *a);
extern Scope_Type *ScopeType(Type *t);

#define ENDING = 0

#define BASIC_METHODS                                                          \
  virtual std::string to_string() const ENDING;                                \
  virtual Time::Eval time() ENDING;                                            \
  virtual void generate_llvm() ENDING;                                         \
  virtual void EmitInit(IR::Value id_val) ENDING;                              \
  virtual void EmitDestroy(IR::Value id_val) ENDING;                           \
  virtual IR::Value EmitInitialValue() const ENDING;                           \
  virtual void EmitRepr(IR::Value id_val) ENDING;                              \
  virtual size_t bytes() const ENDING;                                         \
  virtual size_t alignment() const ENDING;                                     \
  virtual bool private_has_vars() ENDING

#define TYPE_FNS(name, checkname)                                              \
  name() = delete;                                                             \
  virtual ~name() {}                                                           \
  virtual bool is_##checkname() const { return true; }                         \
  BASIC_METHODS

struct Type {
public:
  Type() : llvm_type(nullptr), var_check(false) {}
  virtual ~Type() {}
  BASIC_METHODS;

  virtual operator llvm::Type *();

  size_t SpaceInArray() const {
    return MoveForwardToAlignment(bytes(), alignment());
  }

  // Assigns val to var. We need this to dispatch based on both the lhs and rhs
  // types. Assume that the types match appropriately. Depending on the types,
  // this will either simply be a store operation or a call to the assignment
  // function.
  static void CallAssignment(Scope *scope, Type *lhs_type, Type *rhs_type,
                             IR::Value from_val, IR::Value to_var);

  // Note: this one is special. It functions identically to the rest, but it's
  // special in that it will return nullptr if you haven't imported the string
  // library. This should never come up, because it's only used to add type to a
  // string literal, and using a string literal should import strings.
  static Type *get_string();

  virtual bool is_primitive() const { return false; }
#define PRIMITIVE_MACRO(GlobalName, EnumName, name)                            \
  virtual bool is_##name() const { return false; }
#include "../config/primitive.conf"
#undef PRIMITIVE_MACRO

  virtual bool is_array() const { return false; }
  virtual bool is_tuple() const { return false; }
  virtual bool is_pointer() const { return false; }
  virtual bool is_function() const { return false; }
  virtual bool is_struct() const { return false; }
  virtual bool is_parametric_struct() const { return false; }
  virtual bool is_enum() const { return false; }
  virtual bool is_type_variable() const { return false; }
  virtual bool is_range() const { return false; }
  virtual bool is_slice() const { return false; }
  virtual bool is_scope_type() const { return false; }
  virtual bool is_code_block() const { return this == Code_; }

  virtual bool is_big() const;
  virtual bool stores_data() const;

  mutable llvm::Type *llvm_type;
  mutable bool var_check;

  inline bool has_vars() {
    if (var_check) { return false; }
    var_check = true;
    bool result = private_has_vars();
    var_check = false;
    return result;
  }
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
  TYPE_FNS(Primitive, primitive);
  Primitive(PrimType pt);

#define PRIMITIVE_MACRO(GlobalName, EnumName, name)                            \
  virtual bool is_##name() const;
#include "../config/primitive.conf"
#undef PRIMITIVE_MACRO

private:
  PrimType type_;

  IR::Func *repr_func = nullptr;
};

struct Array : public Type {
  TYPE_FNS(Array, array);
  Array(Type *t, size_t l);

  llvm::Function *initialize();
  llvm::Value *initialize_literal(llvm::Value *alloc, llvm::Value *len);

  IR::Func *init_func, *repr_func, *destroy_func;

  Type *data_type;
  size_t len;
  bool fixed_length;

  // Not the length of the array, but the dimension. That is, it's how many
  // times you can access an element.
  size_t dimension;

  Array(Type *t);
};

struct Tuple : public Type {
  TYPE_FNS(Tuple, tuple);

  Tuple(const std::vector<Type *> &types);

  std::vector<Type *> entries;
};

struct Pointer : public Type {
  TYPE_FNS(Pointer, pointer);

  Pointer(Type *t);
  Type *pointee;
};

struct Function : public Type {
  TYPE_FNS(Function, function);

  operator llvm::FunctionType *();

  Function(Type *in, Type *out);
  Type *input, *output;
};

struct Enum : public Type {
  TYPE_FNS(Enum, enum);
  Enum(const std::string &name, const std::vector<std::string> &members);

  size_t IndexOrFail(const std::string &str) const;
  Type *ProxyType() const;
  IR::Value EmitLiteral(const std::string &member_name) const;

  std::string bound_name;
  std::vector<std::string> members;
  std::map<std::string, size_t> int_values;
  llvm::GlobalVariable *string_data;
};

struct Struct : public Type {
  TYPE_FNS(Struct, struct);

  Struct(const std::string &name);

  void EmitDefaultAssign(IR::Value to_var, IR::Value from_val);

  // Return the type of a field, or a nullptr if it doesn't exist
  Type *field(const std::string &name) const;

  size_t field_num(const std::string &name) const;

  void CompleteDefinition();

  Scope *type_scope;
  std::vector<AST::Declaration *> decls;

  std::string bound_name;

  void insert_field(const std::string &name, Type *ty,
                    AST::Expression *init_val);

  // Field database info
  std::map<std::string, size_t> field_name_to_num;
  std::vector<std::string> field_num_to_name;
  std::vector<Type *> field_type;
  std::vector<size_t> field_offsets;

  std::vector<AST::Expression *> init_values;
  ParamStruct *creator;

private:
  IR::Func *init_func, *assign_func, *destroy_func;
  bool completed_;
};

struct ParamStruct : public Type {
  TYPE_FNS(ParamStruct, parametric_struct);

  ParamStruct(const std::string &name,
                      const std::vector<AST::Declaration *> params,
                      const std::vector<AST::Declaration *> decls);

  IR::Func *IRFunc();

  std::string bound_name;
  Scope *type_scope;
  std::vector<AST::Declaration *> params, decls;
  std::map<std::vector<IR::Value>, Struct *> cache;
  std::map<Struct *, std::vector<IR::Value>> reverse_cache;

private:
  IR::Func *ir_func;
};

struct TypeVariable : public Type {
  TYPE_FNS(TypeVariable, type_variable);

  TypeVariable(AST::Identifier *id, AST::Expression *test)
      : identifier(id), test(test) {}

  AST::Identifier *identifier;
  AST::Expression *test;
};

struct RangeType : public Type {
  TYPE_FNS(RangeType, range);

  RangeType(Type *t) : end_type(t) {}

  Type *end_type;
};

struct SliceType : public Type {
  TYPE_FNS(SliceType, slice);

  SliceType(Array *a) : array_type(a) {}

  Array *array_type;
};

struct Scope_Type : public Type {
  TYPE_FNS(Scope_Type, scope_type);

  Scope_Type(Type *t) : type_(t) {}
  Type *type_;

  std::string bound_name;
};

std::ostream &operator<<(std::ostream &os, const Type &t);

#undef TYPE_FNS
#undef BASIC_METHODS
#undef ENDING

#endif // ICARUS_TYPE_H
