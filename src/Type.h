#ifndef ICARUS_TYPE_H
#define ICARUS_TYPE_H

#include <vector>
#include <string>
#include <map>

class Type {
  private:
    enum PrimEnum {
      t_type_error, t_unknown, t_bool, t_char, t_int, t_real, t_string, t_type, t_uint, t_void
    };

  public:
    Type(PrimEnum p = t_unknown) : prim_type_(p) {}

    virtual std::string serialize() const {
      return std::to_string(prim_type_);
    }

    virtual std::string to_string() const {
      return type_strings[prim_type_];
    }

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
    static const std::map<std::string, Type> Literals;

  private:
    static std::vector<std::string> type_strings;

    PrimEnum prim_type_;
};

inline bool operator==(const Type& lhs, const Type& rhs) {
  return lhs.serialize() == rhs.serialize();
}

inline bool operator!=(const Type& lhs, const Type& rhs) {
  return !(lhs == rhs);
}

#endif
