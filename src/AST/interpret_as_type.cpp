#include "AST.h"

namespace AST {
  Type* Unop::interpret_as_type() {
    return Type::get_type_error();
  }

  Type* Binop::interpret_as_type() {
    if (token() == "->") {
      return Type::get_function(
          lhs_->interpret_as_type(),
          rhs_->interpret_as_type());
    }

    // TODO more cases here probably
    return Type::get_type_error();
  }

  Type* ArrayLiteral::interpret_as_type() {
    return nullptr;
  }

  Type* ArrayType::interpret_as_type() {
    return Type::get_array(array_type_->interpret_as_type());
  }

  Type* ChainOp::interpret_as_type() {
    // In order for a ChainOp to even be created, ops_.front() must exist.
    // Because nothing has the same precedence levels as a comma, if the
    // first op is a comma, they all are.
    if (ops_.front()->token() == ",") {
      // Create a vector to hold the types so that we can pass it in to the
      // tuple constructor. We know how big it needs to be, so we make it
      // that big to begin with.
      std::vector<Type*> type_vec(exprs_.size(), nullptr);

      size_t position = 0;
      for (const auto& eptr : exprs_) {
        type_vec[position] = eptr->interpret_as_type();
        ++position;
      }
      return Type::get_tuple(type_vec);
    }

    return Type::get_type_error();
  }

  Type* Identifier::interpret_as_type() {
    if (expr_type_ == Type::get_type()) {
      return Type::get_user_defined(token());
    }

    error_log.log(line_num(), "`" + token() + "` is not at type.");

    return Type::get_type_error();
  }

  Type* Terminal::interpret_as_type() {
    if (expr_type_ == Type::get_type()) {

      if (token() == "bool") return Type::get_bool();
      if (token() == "char") return Type::get_char();
      if (token() == "int") return Type::get_int();
      if (token() == "real") return Type::get_real();
      if (token() == "type") return Type::get_type();
      if (token() == "uint") return Type::get_uint();
      if (token() == "void") return Type::get_void();

      // TODO better error message
      error_log.log(line_num(), "I don't think `" + token() + "` is a type!");

      return Type::get_type_error();
    }

    error_log.log(line_num(), "`" + token() + "` is not at type.");

    return Type::get_type_error();
  }

  Type* FunctionLiteral::interpret_as_type() {
    return Type::get_type_error();
  }

  Type* Case::interpret_as_type() {
    return Type::get_type_error();
  }

  Type* TypeLiteral::interpret_as_type() {
    return type_value_;
  }

  Type* EnumLiteral::interpret_as_type() {
    return type_value_;
  }

}  // namespace AST
