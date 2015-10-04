#include "Type.h"
#include "AST/Expression.h"

std::vector<std::string> Type::type_strings = {
  "type_error", "??", "bool", "char", "int", "real", "string", "type", "uint", "void"
};

Type Type::build(const AST::Expression* expr_ptr) {
  return Type::Literals.at(expr_ptr->token());
}

const Type Type::Unknown(t_unknown);
const Type Type::TypeError(t_type_error);
const Type Type::Bool(t_bool);
const Type Type::Char(t_char);
const Type Type::Int(t_int);
const Type Type::Real(t_real);
const Type Type::String(t_string);
const Type Type::Type_(t_type);
const Type Type::UInt(t_uint);
const Type Type::Void(t_void);

const std::map<std::string, Type> Type::Literals = {
  { "bool",   Type::Bool },
  { "char",   Type::Char },
  { "int",    Type::Int },
  { "real",   Type::Real },
  { "string", Type::String },
  { "uint",   Type::UInt },
  { "void",   Type::Void }
};

