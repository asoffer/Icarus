#ifndef ICARUS_TYPE_H
#define ICARUS_TYPE_H

#include <vector>
#include <string>
#include <map>
#include <sstream>

#include "llvm/IR/IRBuilder.h"

namespace AST {
  class Expression;
}  // namespace AST

class Function;
class Pointer;

class Type {
  public:
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
    static Type* get_tuple(std::vector<Type*> types);
    static Type* get_array(Type* t, size_t len);

    static std::map<std::string, Type*> literals;
    static std::vector<std::string> type_strings;

    virtual std::string to_string() const = 0;

    virtual bool is_function() const { return false; }

    llvm::Type* llvm() const { return llvm_type_; }

    virtual ~Type() {}

  protected:
    llvm::Type* llvm_type_;
};

class Primitive : public Type {
  public:
    friend class Type;
    virtual ~Primitive() {}
    virtual std::string to_string() const { return Type::type_strings[prim_type_]; }

  private:
    static constexpr size_t num_primitive_types_ = 9;

    enum PrimitiveEnum {
      t_type_error, t_unknown, t_bool, t_char, t_int, t_real, t_type, t_uint, t_void
    };

    PrimitiveEnum prim_type_;

    Primitive(PrimitiveEnum pe);

    static Primitive primitive_types_[ num_primitive_types_ ];
    static llvm::Type* llvm_types_[ num_primitive_types_ ];
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

    virtual std::string to_string() const {
      return "("
        + argument_type()->to_string()
        + " -> "
        + return_type()->to_string()
        + ")";
    }

    virtual ~Function() {}

  private:
    Function(Type* in, Type* out) : input_type_(in), output_type_(out) {
      // TODO should I expand an input tuple into a vector?
      std::vector<llvm::Type*> input_list(1, input_type_->llvm());

      if (input_type_ == get_void()) {
        input_list.clear();
      }

      llvm_type_ = llvm::FunctionType::get( output_type_->llvm(),
          input_list, false);
    }

    Type* input_type_;
    Type* output_type_;

    static std::vector<Function*> fn_types_;
};

class Pointer : public Type {
  public:
    friend class Type;
    virtual std::string to_string() const {
      return "&" + pointee_type_->to_string();
    }

    virtual ~Pointer() {}

  private:
    Pointer(Type* t) : pointee_type_(t) {
      llvm_type_ = llvm::PointerType::get(t->llvm(), 0);
    }
    Type* pointee_type_;

    static std::vector<Pointer*> pointer_types_;
};


class Tuple : public Type {
  public:
    friend class Type;

    virtual std::string to_string() const {
      std::stringstream ss;

      auto iter = tuple_types_.begin();
      ss << "(" << (*iter)->to_string();
      while (iter != tuple_types_.end()) {
        ss << ", ";
        ss << (*iter)->to_string();
        ++iter;
      }
      ss << ")";

      return ss.str();
    }

    virtual ~Tuple() {}

  private:
    Tuple(const std::vector<Type*>& types) : entry_types_(types) {}

    std::vector<Type*> entry_types_;

    static std::vector<Tuple*> tuple_types_;
};

class Array : public Type {
  public:
    friend class Type;

    virtual std::string to_string() const {
      return type_->to_string() + "[" + std::to_string(len_) + "]";
    }

    virtual ~Array() {}

  private:
    Array(Type* t, size_t len) : type_(t), len_(len) {}

    Type* type_;
    size_t len_;

    static std::vector<Array*> array_types_;
};

#endif
