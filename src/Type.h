#ifndef ICARUS_TYPE_H
#define ICARUS_TYPE_H

#include <vector>
#include <string>
#include <map>

// TODO Figure out what you need from this.
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

// TODO why is replace(_, _) not const?

namespace AST {
  class Expression;
  class Declaration;
}  // namespace AST

class Function;
class Pointer;

#include "typedefs.h"

class Type {
  public:
    friend class Array;
    friend class UserDefined;

    enum time_loc {
      either_time  = 0x0,
      compile_time = 0x1,
      run_time     = 0x2,
      mixed_time   = 0x3
    };

    static Type* get_type_error();
    static Type* get_unknown();
    static Type* get_bool();
    static Type* get_char();
    static Type* get_int();
    static Type* get_real();
    static Type* get_type();
    static Type* get_uint();
    static Type* get_void();

    static Function* get_function(Type* in, Type* out);
    static Type* get_pointer(Type* t);
    static Type* get_tuple(const std::vector<Type*>& types);
    static Type* get_array(Type* t);
    static Type* get_user_defined(const std::string& name);

    static void make_user_defined(
        const std::vector<DeclPtr>& decls, const std::string& name);

    static std::map<std::string, Type*> literals;

    virtual llvm::Value* allocate(llvm::IRBuilder<>& bldr) const = 0;
    virtual size_t bytes() const = 0;

    virtual llvm::Function* assign() = 0;
    virtual llvm::Function* initialize() = 0;
    virtual llvm::Function* print() = 0;
    // virtual llvm::Function* repr() { return nullptr; } // TODO Implement this
    virtual llvm::Function* uninitialize() = 0;

    virtual Type* replace(Type* pattern, Type* replacement) = 0;
    virtual std::string to_string() const = 0;
    virtual time_loc type_time() const = 0;

    virtual bool is_array()        const { return false; }
    virtual bool is_function()     const { return false; }
    virtual bool is_pointer()      const { return false; }
    virtual bool is_primitive()    const { return false; }
    virtual bool is_tuple()        const { return false; }
    virtual bool is_void()         const { return this == Type::get_void(); }
    virtual bool is_user_defined() const { return false; }

    llvm::Type* llvm() const { return llvm_type_; }

    Type() :
      assign_fn_(nullptr),
      init_fn_  (nullptr),
      print_fn_ (nullptr),
      repr_fn_  (nullptr),
      uninit_fn_(nullptr) {}

    virtual ~Type() {}

  protected:
    // NOTE: For the same of simplicity, none of these will be left undefined,
    // even if they do something that should obviously be inlined. We'll trust
    // the inliner to do it's job.
    llvm::Function
      * assign_fn_,
      * init_fn_,
      * print_fn_,
      * repr_fn_,
      * uninit_fn_;

    llvm::Type* llvm_type_;
};

class Primitive : public Type {
  public:
    static constexpr size_t num_primitive_types_ = 9;

    friend class Type;
    virtual ~Primitive() {}

    virtual llvm::Value* allocate(llvm::IRBuilder<>& bldr) const;
    virtual size_t bytes() const;

    virtual llvm::Function* assign();
    virtual llvm::Function* initialize();
    virtual llvm::Function* print();
    // virtual llvm::Function* repr();
    virtual llvm::Function* uninitialize();

    virtual Type* replace(Type* pattern, Type* replacement);
    virtual std::string to_string() const;
    virtual time_loc type_time() const;

    virtual bool is_primitive() const { return true; }

  private:
    // This needs to be an enum becasue we use it to access other arrays and
    // vectors
    enum PrimitiveEnum {
      t_type_error, t_unknown, t_bool, t_char, t_int, t_real, t_type, t_uint, t_void
    };


    PrimitiveEnum prim_type_;

    Primitive(PrimitiveEnum pe);

    static Primitive primitive_types_[ num_primitive_types_ ];
    static llvm::Type* llvm_types_[ num_primitive_types_ ];
};

class Tuple : public Type {
  public:
    friend class Type;
    friend class Function;

    virtual bool is_tuple() const { return true; }

    virtual llvm::Value* allocate(llvm::IRBuilder<>& bldr) const;
    virtual size_t bytes() const;

    virtual llvm::Function* assign();
    virtual llvm::Function* initialize();
    virtual llvm::Function* print();
    // virtual llvm::Function* repr();
    virtual llvm::Function* uninitialize();

    virtual Type* replace(Type* pattern, Type* replacement);
    virtual std::string to_string() const;
    virtual time_loc type_time() const;

    size_t size() const { return entry_types_.size(); } 

    virtual ~Tuple() {}

  private:
    Tuple(const std::vector<Type*>& types) : entry_types_(types) {}

    std::vector<Type*> entry_types_;

    static std::vector<Tuple*> tuple_types_;
};

class Function : public Type {
  public:
    friend class Type;
    virtual bool is_function() const { return true; }
    Type* argument_type() const { return input_type_; }
    Type* return_type() const { return output_type_; }

    llvm::FunctionType* llvm() const {
      return static_cast<llvm::FunctionType*>(llvm_type_);
    }

    virtual llvm::Value* allocate(llvm::IRBuilder<>& bldr) const;
    virtual size_t bytes() const;

    virtual llvm::Function* assign();
    virtual llvm::Function* initialize();
    virtual llvm::Function* print();
    // virtual llvm::Function* repr();
    virtual llvm::Function* uninitialize();

    virtual Type* replace(Type* pattern, Type* replacement);
    virtual std::string to_string() const;
    virtual time_loc type_time() const;

    virtual ~Function() {}

  private:
    Function(Type* in, Type* out) : input_type_(in), output_type_(out) {
      std::vector<llvm::Type*> input_list;
      if (input_type_->is_tuple()) {
        auto input_tuple = static_cast<Tuple*>(input_type_);
        auto len = input_tuple->size();
        input_list.resize(len, nullptr);

        size_t i = 0;
        // TODO robustify this.
        // What if tuple element is a tuple?
        // What if tuple element is a function?
        // ...
        for (const auto& input : input_tuple->entry_types_) {
          input_list[i] = input->llvm();
          ++i;
        }
      } else if (input_type_->is_function()) {
        input_list.push_back(Type::get_pointer(input_type_)->llvm());
      
      } else if (!input_type_->is_void()) {
        input_list.push_back(input_type_->llvm());
      }

      auto llvm_output = output_type_->is_function()
        ? Type::get_pointer(output_type_)->llvm()
        : output_type_->llvm();

      // Boolean parameter 'false' designates that this function is not variadic.
      llvm_type_ = llvm::FunctionType::get(llvm_output, input_list, false);
    }

    Type* input_type_;
    Type* output_type_;

    static std::vector<Function*> fn_types_;
};

class Pointer : public Type {
  public:
    friend class Type;

    virtual bool is_pointer() const { return true; }

    virtual llvm::Value* allocate(llvm::IRBuilder<>& bldr) const;
    virtual size_t bytes() const;

    virtual llvm::Function* assign();
    virtual llvm::Function* initialize();
    virtual llvm::Function* print();
    // virtual llvm::Function* repr();
    virtual llvm::Function* uninitialize();

    virtual Type* replace(Type* pattern, Type* replacement);
    virtual std::string to_string() const;
    virtual time_loc type_time() const;

    virtual ~Pointer() {}

  private:
    Pointer(Type* t) : pointee_type_(t) {
      llvm_type_ = llvm::PointerType::getUnqual(t->llvm());
    }
    Type* pointee_type_;

    static std::vector<Pointer*> pointer_types_;
};

class Array : public Type {
  public:
    friend class AST::Declaration;
    friend class Type;

    virtual bool is_array() const { return true; }

    virtual llvm::Value* allocate(llvm::IRBuilder<>& bldr) const;
    virtual size_t bytes() const;

    virtual llvm::Function* assign();
    virtual llvm::Function* initialize();
    virtual llvm::Function* print();
    // virtual llvm::Function* repr();
    virtual llvm::Function* uninitialize();

    virtual Type* replace(Type* pattern, Type* replacement);
    virtual std::string to_string() const;
    virtual time_loc type_time() const;

    virtual Type* data_type() const { return type_; }

    llvm::Value* initialize_literal(llvm::IRBuilder<>& bldr, llvm::Value* runtime_len = nullptr);

    virtual ~Array() {}

  private:
    // A value of -1 for the length means this is to be dependently typed. All
    // other values are the actual type
    Array(Type* t);

    // Not the length of the array, but the dimension. That is, it's how many
    // times you can access an element.
    size_t dim_;

    Type* type_;

    static std::vector<Array*> array_types_;
};

class UserDefined : public Type {
  public:
    friend class Type;

    virtual llvm::Value* allocate(llvm::IRBuilder<>& bldr) const;
    virtual size_t bytes() const;

    virtual llvm::Function* assign();
    virtual llvm::Function* initialize();
    virtual llvm::Function* print();
    //virtual llvm::Function* repr();
    virtual llvm::Function* uninitialize();

    virtual Type* replace(Type* pattern, Type* replacement);
    virtual std::string to_string() const;
    virtual time_loc type_time() const;

    virtual bool is_user_defined() const { return true; }

    Type* field(const std::string& name) const;
    llvm::Value* field_num(const std::string& name) const;


    virtual ~UserDefined() {}

  private:
    std::vector<std::pair<std::string, Type*>> fields_;

    static std::map<std::string, UserDefined*> lookup_;
};

#endif
