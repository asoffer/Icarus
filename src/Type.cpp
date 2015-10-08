#include "Type.h"
#include "AST.h"

std::vector<std::string> Type::type_strings = {
  "type_error", "??", "bool", "char", "int", "real", "string", "type", "uint", "void", "->"
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

const Type Type::Function(Type in, Type out) {
  Type output_type;
  output_type.rpn_type_ = in.rpn_type_;
  output_type.rpn_type_.insert(output_type.rpn_type_.end(),
      out.rpn_type_.begin(), out.rpn_type_.end());
  output_type.rpn_type_.push_back(t_fn);

  return output_type;
}

const std::map<std::string, Type> Type::Literals = {
  { "bool",   Type::Bool },
  { "char",   Type::Char },
  { "int",    Type::Int },
  { "real",   Type::Real },
  { "string", Type::String },
  { "uint",   Type::UInt },
  { "void",   Type::Void }
};

Type Type::return_type() const {
#if DEBUG
  if (!is_function()) throw "This isn't a function!";
#endif

  size_t end_of_first_pos = 0;
  size_t stack_size = 0;
  for (size_t i = 0; i < rpn_type_.size() - 1; ++i) {
    if (rpn_type_[i] == t_fn) {
      --stack_size;
    } else {
      ++stack_size;
    }

    if (stack_size == 1) {
      end_of_first_pos = i;
    }
  }

  Type output;
  auto first = rpn_type_.begin() + static_cast<std::ptrdiff_t>(end_of_first_pos + 1);
  auto last = rpn_type_.end() - 1;
  output.rpn_type_ = std::vector<PrimEnum>(first, last);
  return output;
}

Type Type::input_type() const {
#if DEBUG
  if (!is_function()) throw "This isn't a function!";
#endif

  size_t end_of_first_pos = 0;
  size_t stack_size = 0;
  for (size_t i = 0; i < rpn_type_.size() - 1; ++i) {
    if (rpn_type_[i] == t_fn) {
      --stack_size;
    } else {
      ++stack_size;
    }

    if (stack_size == 1) {
      end_of_first_pos = i;
    }
  }

  Type output;
  auto last = rpn_type_.begin() + static_cast<std::ptrdiff_t>(end_of_first_pos + 1);
  output.rpn_type_ = std::vector<PrimEnum>(rpn_type_.begin(), last);
  return output;
}

