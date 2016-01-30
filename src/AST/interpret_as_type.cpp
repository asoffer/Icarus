#include "AST.h"

namespace AST {
  Type* Binop::interpret_as_type() {
    if (op_ == Language::Operator::Arrow) {
      return Func(lhs_->interpret_as_type(), rhs_->interpret_as_type());
    }

    // TODO more cases here probably
    return Error;
  }

  Type* ArrayType::interpret_as_type() {
    return Arr(array_type_->interpret_as_type());
  }

  Type* ChainOp::interpret_as_type() {
    // In order for a ChainOp to even be created, ops_.front() must exist.
    // Because nothing has the same precedence levels as a comma, if the
    // first op is a comma, they all are.
    if (ops_.front() == Language::Operator::Comma) {
      // Create a vector to hold the types so that we can pass it in to the
      // tuple constructor. We know how big it needs to be, so we make it
      // that big to begin with.
      std::vector<Type*> type_vec(exprs_.size(), nullptr);

      size_t position = 0;
      for (const auto& eptr : exprs_) {
        type_vec[position] = eptr->interpret_as_type();
        ++position;
      }
      return Tup(type_vec);
    }

    return Error;
  }

  Type* Identifier::interpret_as_type() {
    if (type() == Type_) return TypeSystem::get(token());

    error_log.log(line_num(), "`" + token() + "` is not a type.");

    return Error;
  }

  Type* Declaration::interpret_as_type() {
    return decl_type_->interpret_as_type();
  }

  Type* Terminal::interpret_as_type() {
    if (type() == Type_) {

      // TODO Lookup table as part of Type class
      // TODO TOKENREMOVAL
      if (token() == "bool") return Bool;
      if (token() == "char") return Char;
      if (token() == "int")  return Int;
      if (token() == "real") return Real;
      if (token() == "type") return Type_;
      if (token() == "uint") return Uint;
      if (token() == "void") return Void;

      // TODO better error message
      error_log.log(line_num(), "I don't think `" + token() + "` is a type!");

      return Error;
    }

    error_log.log(line_num(), "`" + token() + "` is not at type.");

    return Error;
  }

  Type* FunctionLiteral::interpret_as_type()  { return Error; }
  Type* Case::interpret_as_type()             { return Error; }
  Type* ArrayLiteral::interpret_as_type()     { return Error; }
  Type* Unop::interpret_as_type()             { return Error; }

  Type* TypeLiteral::interpret_as_type() { return type_value_; }
  Type* EnumLiteral::interpret_as_type() { return type_value_; }
}  // namespace AST
