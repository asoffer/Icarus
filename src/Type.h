#ifndef ICARUS_TYPE_H
#define ICARUS_TYPE_H

#include <vector>
#include <string>
#include <map>

namespace AST {
  class Expression;
}  // namespace AST

class Type {
  private:
    enum PrimEnum {
      t_type_error, t_unknown, t_bool, t_char, t_int, t_real, t_string, t_type, t_uint, t_void, t_fn
    };

  public:
    Type(PrimEnum p = t_unknown) : rpn_type_(1, p) {}
    static Type build(const AST::Expression* expr_ptr);

    std::string serialize() const {
      std::string output;
      for (const auto& pe : rpn_type_) {
        output += " " + type_strings[pe];
      }
      return output;
    }

    std::string to_string() const {
      return serialize();
    }

    bool is_function() const {
      return rpn_type_.back() == t_fn;
    }

    Type return_type() const;
    Type input_type() const;

    static const Type Unknown;
    static const Type TypeError;
    static const Type Bool;
    static const Type Char;
    static const Type Int;
    static const Type Real;
    static const Type String; //TODO this is not really primitive
    static const Type Type_;
    static const Type UInt;
    static const Type Void;

    static const Type Function(Type in, Type out);

    static const std::map<std::string, Type> Literals;

  private:
    friend struct std::less<Type>;

    static std::vector<std::string> type_strings;

    // Store the type in reverse polish notation, so char -> int gets stored as
    // char int ->
    // { t_char, t_int, t_fn }
    std::vector<PrimEnum> rpn_type_;
};

inline bool operator==(const Type& lhs, const Type& rhs) {
  return lhs.serialize() == rhs.serialize();
}

inline bool operator!=(const Type& lhs, const Type& rhs) {
  return !(lhs == rhs);
}

// For putting in ordered containers
namespace std {
  template<> struct less<Type> {
    bool operator()(const Type& lhs, const Type& rhs) {
      if (lhs.rpn_type_.size() < rhs.rpn_type_.size()) return true;
      if (lhs.rpn_type_.size() < rhs.rpn_type_.size()) return false;

      for (size_t i = 0; i < lhs.rpn_type_.size(); ++i) {
        if (lhs.rpn_type_[i] < rhs.rpn_type_[i]) return true;
        if (lhs.rpn_type_[i] < rhs.rpn_type_[i]) return false;
      }

      return false;
    }
  };
};

#endif
