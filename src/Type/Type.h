#ifndef ICARUS_TYPE_H
#define ICARUS_TYPE_H

extern llvm::IRBuilder<> builder;

// TODO this is not the right API for mangling.
extern std::string Mangle(const Type *t, bool prefix = true);
extern std::string Mangle(const Function *f, AST::Expression *expr,
                          Scope *starting_scope = nullptr);

extern Type *Err, *Unknown, *Bool, *Char, *Int, *Real, *Type_, *Uint, *Void,
    *NullPtr, *RawPtr, *String;

extern Pointer *Ptr(Type *t);
extern Array *Arr(Type *t);
extern Array *Arr(Type *t, size_t len);
extern Tuple *Tup(const std::vector<Type *> &t);
extern Function *Func(Type *in, Type *out);
extern Function *Func(std::vector<Type *> in, Type *out);
extern Function *Func(Type *in, std::vector<Type *> out);
extern Function *Func(std::vector<Type *> in, std::vector<Type *> out);
extern Enumeration *Enum(const std::string &name,
                         const AST::EnumLiteral *e = nullptr);
extern Structure *Struct(const std::string &name,
                         AST::StructLiteral *expr = nullptr);
extern ParametricStructure *
ParamStruct(const std::string &name,
            AST::ParametricStructLiteral *expr = nullptr);
extern TypeVariable *TypeVar(AST::Identifier *id,
                             AST::Expression *test = nullptr);
extern RangeType *Range(Type *t);
extern SliceType *Slice(Array *a);

#define ENDING = 0

#define BASIC_METHODS                                                          \
  virtual std::string to_string() const ENDING;                                \
  virtual Time::Eval time() const ENDING;                                      \
  virtual void generate_llvm() const ENDING;                                   \
  virtual void EmitInit(IR::Value id_val) ENDING;                              \
  virtual IR::Value EmitInitialValue() const ENDING;                           \
  virtual void EmitRepr(IR::Value id_val) ENDING;                              \
  virtual llvm::Constant *InitialValue() const ENDING

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

  virtual llvm::Value *allocate() const;

  virtual bool requires_uninit() const { return false; }

  virtual bool is_primitive() const { return false; }
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
    Err, Unknown, Bool, Char, Int, Real, Type, Uint, Void, NullPtr
  };

  Primitive::TypeEnum type_;
  llvm::Function *repr_fn_;

  Primitive(TypeEnum pt);
};

struct Array : public Type {
  TYPE_FNS(Array, array);
  Array(Type *t, size_t l);

  virtual bool requires_uninit() const;

  llvm::Function *initialize();
  llvm::Value *initialize_literal(llvm::Value *alloc, llvm::Value *len);

  virtual llvm::Value *allocate() const;

  IR::Func *init_func, *repr_func;

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

  // TODO requires_uninit()

  virtual llvm::Value *allocate() const;

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

  operator llvm::FunctionType *() const;

  virtual llvm::Value *allocate() const;

  Function(Type *in, Type *out);
  Type *input, *output;
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

  void EmitDefaultAssign(IR::Value to_var, IR::Value from_val);

  virtual bool requires_uninit() const;

  void set_name(const std::string &name);

  // Return the type of a field, or a nullptr if it doesn't exist
  Type *field(const std::string &name) const;

  size_t field_num(const std::string &name) const;

  AST::StructLiteral *ast_expression;
  std::string bound_name;

  void insert_field(const std::string &name, Type *ty,
                    AST::Expression *init_val);

  // Field database info
  std::map<std::string, size_t> field_name_to_num;
  std::vector<std::string> field_num_to_name;
  std::vector<Type *> field_type;
  std::vector<size_t> field_offsets;

  std::vector<AST::Expression *> init_values;
  AST::ParametricStructLiteral *creator;

private:
  IR::Func *init_func, *assign_func;
};

struct ParametricStructure : public Type {
  TYPE_FNS(ParametricStructure, parametric_struct);

  ParametricStructure(const std::string &name, AST::ParametricStructLiteral *expr);

  void set_name(const std::string &name);

  AST::ParametricStructLiteral *ast_expression;
  std::string bound_name;
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

struct RangeType : public Type {
  TYPE_FNS(RangeType, range);

  RangeType(Type *t) : end_type(t) { has_vars = end_type->has_vars; }

  Type *end_type;
};

struct SliceType : public Type {
  TYPE_FNS(SliceType, slice);

  SliceType(Array *a) : array_type(a) { has_vars = array_type->has_vars; }

  Array *array_type;
};
std::ostream &operator<<(std::ostream &os, const Type &t);

#undef TYPE_FNS
#undef BASIC_METHODS
#undef ENDING

#endif // ICARUS_TYPE_H
