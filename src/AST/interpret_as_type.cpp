#include "AST.h"

namespace AST {
  Type Unop::interpret_as_type() const {
    // TODO Implement this
    return Type::TypeError;
  }

  Type Binop::interpret_as_type() const {
    if (token() == "->") {
      return Type::Function(
          lhs_->interpret_as_type(),
          rhs_->interpret_as_type());
    }

    // TODO more cases here probably
    return Type::TypeError;
  }


  Type ChainOp::interpret_as_type() const {
    return Type::TypeError;
  }


  Type Terminal::interpret_as_type() const {
    if (expr_type_ == Type::Type_) {

      if (token() == "bool") return Type::Bool;
      if (token() == "char") return Type::Char;
      if (token() == "int") return Type::Int;
      if (token() == "real") return Type::Real;
      if (token() == "string") return Type::String;
      if (token() == "type") return Type::Type_;
      if (token() == "uint") return Type::UInt;
      if (token() == "void") return Type::Void;

      std::cerr
        << "I don't think " << token()
        << " is a type!" << std::endl;

      return Type::TypeError;
    }

    std::cerr << token() + " is not a type!" << std::endl;

    return Type::TypeError;
  }

//  Type AnonymousScope::interpret_as_type() const {
//    // throw "Stub, this shouldn't be possible";
//    return Type::TypeError;
//  }

  Type FunctionLiteral::interpret_as_type() const {
    // throw "Stub, this shouldn't be possible";
    return Type::TypeError;
  }

  Type Case::interpret_as_type() const {
    // throw "Stub, this shouldn't be possible";
    return Type::TypeError;
  }

}  // namespace AST
