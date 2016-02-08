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

extern llvm::DataLayout* data_layout;

namespace AST {
  class Expression;
  class EnumLiteral;
  class Declaration;
  class TypeLiteral;
}  // namespace AST

class Type;
class Primitive;
class Array;
class Tuple;
class Pointer;
class Function;
class Enumeration;
class Structure;

namespace TypeSystem {
  void initialize();
  extern std::map<std::string, Type*> Literals;
  extern Type* get_operator(Language::Operator op, Type* signature);
  extern Type* get(const std::string& name);
}  // namespace TypeSystem

extern Primitive* Error;
extern Primitive* Unknown;
extern Primitive* Bool;
extern Primitive* Char;
extern Primitive* Int;
extern Primitive* Real;
extern Primitive* Type_;
extern Primitive* Uint;
extern Primitive* Void;
extern Pointer*   RawPtr;
extern Structure* String;

extern Pointer* Ptr(Type* t);
extern Array* Arr(Type* t);
extern Tuple* Tup(const std::vector<Type*>& t);
extern Function* Func(Type* in, Type* out);
extern Function* Func(std::vector<Type*> in, Type* out);
extern Function* Func(Type* in, std::vector<Type*> out);
extern Function* Func(std::vector<Type*> in, std::vector<Type*> out);
extern Enumeration* Enum(const std::string& name,
    const AST::EnumLiteral* e = nullptr);
extern Structure* Struct(const std::string& name);

#include "typedefs.h"

#define ENDING = 0 

#define BINARY_OPERATOR_MACRO(op, symbol, prec, assoc) \
  virtual llvm::Value* call_##op (llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) ENDING;

#define LEFT_UNARY_OPERATOR_MACRO(op) \
  virtual llvm::Value* call_##op (llvm::IRBuilder<>& bldr, llvm::Value* operand) ENDING;

#define BASIC_FUNCTIONS                                                   \
  virtual llvm::Function* assign() ENDING;                                \
virtual std::string to_string() const ENDING;                             \
virtual Time::Eval time() const ENDING;                                   \
virtual void set_print(llvm::Function* fn) ENDING;                        \
virtual void set_assign(llvm::Function* fn) ENDING;                       \
virtual bool add_llvm_input(std::vector<llvm::Type*>& llvm_in) ENDING;    \
virtual void call_init(llvm::IRBuilder<>& bldr, llvm::Value* var) ENDING; \
virtual void call_repr(llvm::IRBuilder<>& bldr, llvm::Value* val) ENDING; \
virtual void call_uninit(llvm::IRBuilder<>& bldr, llvm::Value* var) ENDING

class Type {
  public:
    friend class ::Array;

    virtual operator llvm::Type* () const { return llvm_type_; }

    size_t bytes() const;

    // Note: this one is special. It functions identically to the rest, but
    // it's special in that it will return nullptr if you haven't imported the
    // string library. This should never come up, because it's only used to add
    // type to a string literal, and using a string literal should import strings.
    static Type* get_string();

    static std::map<std::string, Type*> literals;

    virtual llvm::Value* allocate(llvm::IRBuilder<>& bldr) const {
      return bldr.CreateAlloca(*this);
    }

    virtual void call_print(llvm::IRBuilder<>& bldr, llvm::Value* val) {
      call_repr(bldr, val);
    }

    virtual llvm::Value* call_cast(llvm::IRBuilder<>& bldr, llvm::Value* val, Type* to_type) { return nullptr; }

    BASIC_FUNCTIONS;
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"

    virtual bool requires_uninit() const { return false; }

    virtual bool is_primitive() const { return false; }
    virtual bool is_array()     const { return false; }
    virtual bool is_tuple()     const { return false; }
    virtual bool is_pointer()   const { return false; }
    virtual bool is_function()  const { return false; }
    virtual bool is_struct()    const { return false; }
    virtual bool is_enum()      const { return false; }

    llvm::Type* llvm() const { return llvm_type_; }

    Type();

    virtual ~Type() {}

  protected:
    llvm::Function* assign_fn_;
    llvm::Type* llvm_type_;
};


#undef ENDING
#define ENDING

class Primitive : public Type {
  public:
    Primitive() = delete;
    virtual ~Primitive() {}
    virtual bool is_primitive() const { return true; }
    friend void TypeSystem::initialize();

    virtual void call_print(llvm::IRBuilder<>& bldr, llvm::Value* val);
    virtual llvm::Value* call_cast(llvm::IRBuilder<>& bldr,
        llvm::Value* val, Type* to_type);

    BASIC_FUNCTIONS;
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"

  private:
    enum class TypeEnum { Error, Unknown, Bool, Char, Int, Real, Type, Uint, Void };
    Primitive(TypeEnum pt);

    Primitive::TypeEnum type_;
    llvm::Function* repr_fn_;
};

class Array : public Type {
  public:
    Array() = delete;
    virtual ~Array() {}
    virtual bool is_array() const { return true; }
    friend Array* Arr(Type*);

    friend class AST::Declaration;
    friend class Type;

      virtual bool requires_uninit() const;
      virtual llvm::Value* call_cast(llvm::IRBuilder<>& bldr, llvm::Value* val, Type* to_type);

      BASIC_FUNCTIONS;
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"

      virtual Type* data_type() const { return type_; }
      virtual size_t dim() const { return dim_; }

      llvm::Function* initialize();
      llvm::Value* initialize_literal(llvm::IRBuilder<>& bldr,
          llvm::Value* runtime_len = nullptr);


    private:
      Array(Type* t);

      // Not the length of the array, but the dimension. That is, it's how many
      // times you can access an element.
      size_t dim_;

      llvm::Function *init_fn_, *uninit_fn_, *repr_fn_;

      Type* type_;
  };

class Tuple : public Type {
  public:
    Tuple() = delete;
    virtual ~Tuple() {}
    virtual bool is_tuple() const { return true; }
    friend Tuple* Tup(const std::vector<Type*>&);

    std::vector<Type*> entry_types() { return entry_types_; }

    // TODO requires_uninit()

    virtual llvm::Value* allocate(llvm::IRBuilder<>& bldr) const;
    virtual llvm::Value* call_cast(llvm::IRBuilder<>& bldr, llvm::Value* val, Type* to_type);

    BASIC_FUNCTIONS;
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"

    size_t size() const { return entry_types_.size(); } 

  private:
    Tuple(const std::vector<Type*>& types);

    std::vector<Type*> entry_types_;
};

class Pointer : public Type {
  public:
    Pointer() = delete;
    virtual ~Pointer() {}
    virtual bool is_pointer() const { return true; }

    friend Pointer* Ptr(Type*);

    Type* pointee_type() const { return pointee_type_; }


    virtual llvm::Value* call_cast(llvm::IRBuilder<>& bldr, llvm::Value* val, Type* to_type);

    BASIC_FUNCTIONS;
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"


  private:
    Pointer(Type* t);
    Type* pointee_type_;
};

class Function : public Type {
  public:
    Function() = delete;
    virtual ~Function() {}
    virtual bool is_function() const { return true; }
    friend Function* Func(Type* in, Type* out);

    operator llvm::FunctionType* () const {
      return static_cast<llvm::FunctionType*>(llvm_type_);
    }

    Type* argument_type() const { return input_type_; }
    Type* return_type()   const { return output_type_; }

    virtual llvm::Value* allocate(llvm::IRBuilder<>& bldr) const;
    virtual llvm::Value* call_cast(llvm::IRBuilder<>& bldr, llvm::Value* val, Type* to_type);

    BASIC_FUNCTIONS;
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"

  private:
    Function(Type* in, Type* out);

    Type* input_type_;
    Type* output_type_;
};

class Enumeration : public Type {
  public:
    Enumeration() = delete;
    virtual ~Enumeration() {}
    virtual bool is_enum() const { return true; }
    friend Enumeration* Enum(const std::string& name, const AST::EnumLiteral* e );

    BASIC_FUNCTIONS;
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"

    llvm::Value* get_value(const std::string& str) const;

    void set_name(const std::string& name) { name_ = name; }

  private:
    Enumeration(const std::string& name, const AST::EnumLiteral* enumlit);

    std::string name_;
    llvm::Function* repr_fn_;
    std::map<std::string, llvm::Value*> intval_;
    llvm::GlobalVariable* str_array_;
};

class Structure : public Type {
  public:
    Structure();
    virtual ~Structure() {}
    virtual bool is_struct() const { return true; }
    friend Structure* Struct(const std::string& name,
        const std::vector<DeclPtr>& decls);

    Structure(const std::string& name);

    friend Type* TypeSystem::get(const std::string& name);
    friend class AST::TypeLiteral;
    virtual bool requires_uninit() const;

    void set_name(const std::string& name) {
      name_ = name;
      if (name == "string") {
        String = this;
      }
    }

    virtual llvm::Value* call_cast(llvm::IRBuilder<>& bldr, llvm::Value* val, Type* t);

    BASIC_FUNCTIONS;
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"
    Type* field(const std::string& name) const;
    llvm::Value* field_num(const std::string& name) const;

    virtual void call_print(llvm::IRBuilder<>& bldr, llvm::Value* val);

  private:
    std::string name_;

    llvm::Function *init_fn_, *uninit_fn_, *print_fn_;

    std::vector<std::pair<std::string, Type*>> fields_;
};

std::ostream& operator<<(std::ostream& os, const Type& t);

#undef BASIC_FUNCTIONS
#undef LEFT_UNARY_OPERATOR_MACRO
#undef BINARY_OPERATOR_MACRO
#undef ENDING

#endif  // ICARUS_TYPE_H
