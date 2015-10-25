#include "AST.h"

namespace AST {
  Type* Unop::interpret_as_type() const {
    // TODO Implement this
    return Type::get_type_error();
  }

  Type* Binop::interpret_as_type() const {
    if (token() == "->") {
      return Type::get_function(
          lhs_->interpret_as_type(),
          rhs_->interpret_as_type());
    }

    // TODO more cases here probably
    return Type::get_type_error();
  }


  Type* ChainOp::interpret_as_type() const {
    return Type::get_type_error();
  }


  Type* Terminal::interpret_as_type() const {
    if (expr_type_ == Type::get_type()) {

      if (token() == "bool") return Type::get_bool();
      if (token() == "char") return Type::get_char();
      if (token() == "int") return Type::get_int();
      if (token() == "real") return Type::get_real();
      if (token() == "type") return Type::get_type();
      if (token() == "uint") return Type::get_uint();
      if (token() == "void") return Type::get_void();

      std::cerr
        << "I don't think " << token()
        << " is a type!" << std::endl;

      return Type::get_type_error();
    }

    std::cerr << token() + " is not a type!" << std::endl;

    return Type::get_type_error();
  }

//  Type AnonymousScope::interpret_as_type() const {
//    // throw "Stub, this shouldn't be possible";
//    return Type::get_type_error();
//  }

  Type* FunctionLiteral::interpret_as_type() const {
    // throw "Stub, this shouldn't be possible";
    return Type::get_type_error();
  }

  Type* Case::interpret_as_type() const {
    // throw "Stub, this shouldn't be possible";
    return Type::get_type_error();
  }

}  // namespace AST
