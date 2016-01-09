#include "AST.h"

namespace data {
  extern llvm::Value* const_bool(bool b);
  extern llvm::Value* const_char(char c);
  extern llvm::Value* const_uint(size_t n);
  extern llvm::Value* const_real(double d);

}  // namespace data

namespace AST {
  llvm::Value* Expression::llvm_value(Context::Value v) {
    if (type() == Type::get_bool()) {
      return data::const_bool(v.as_bool);

    } else if (type() == Type::get_char()) {
      return data::const_char(v.as_char);

    } else if (type() == Type::get_int()) {
      // TODO allow negative constants
      return data::const_uint(static_cast<size_t>(v.as_int));

    } else if (type() == Type::get_real()) {
      return data::const_real(v.as_real);

    } else if (type() == Type::get_uint()) {
      return data::const_uint(v.as_uint);
    }

    return nullptr;
  }

  // TODO
  Context::Value Identifier::evaluate(Context&) {
    return Context::Value();
  }

  Context::Value Unop::evaluate(Context& ctx) {
    if (is_return()) {
      ctx.set_return_value(expr_->evaluate(ctx));
    }

    // TODO
    return Context::Value();
  }

  Context::Value ChainOp::evaluate(Context&){ 
    for (size_t i = 0; i < ops_.size(); ++i) {
      auto& last = exprs_[i];
      auto& next = exprs_[i + 1];

      if (ops_[i]->token() == "==") {
        if (last->interpret_as_type() != next->interpret_as_type()) {
          return false;
        }

      } else if (ops_[i]->token() == "!=") {
        if (last->interpret_as_type() == next->interpret_as_type()) {
          return false;
        }
      }
    }

    return true;
  }

  Context::Value ArrayType::evaluate(Context&)       { return Context::Value(); }
  Context::Value ArrayLiteral::evaluate(Context&)    { return Context::Value(); }

  Context::Value Terminal::evaluate(Context& ctx) {
    if (token() == "true") {
      return Context::Value(true);

    } else if (token() == "false") {
      return Context::Value(false);
    }

    // TOD
    return Context::Value();
  }

  Context::Value FunctionLiteral::evaluate(Context&) { return Context::Value(); }
  Context::Value Case::evaluate(Context&)            { return Context::Value(); }
  Context::Value Assignment::evaluate(Context&)      { return Context::Value(); }
  Context::Value Declaration::evaluate(Context&)     { return Context::Value(); }
  Context::Value TypeLiteral::evaluate(Context&)     { return Context::Value(); }
  Context::Value EnumLiteral::evaluate(Context&)     { return Context::Value(); }

  Context::Value Binop::evaluate(Context&) {
    if (token() == "()") {
      Context ctx;
      return lhs_->evaluate(ctx);
    }

    return Context::Value();
  }

  Context::Value KVPairList::evaluate(Context&)      { return Context::Value(); }
  Context::Value Statements::evaluate(Context& ctx) {
    for (auto& stmt : statements_) {
      stmt->evaluate(ctx);
      if (ctx.has_return()) {
        return ctx.return_value();
      }
    }

    return Context::Value();
  }
  Context::Value Conditional::evaluate(Context&)     { return Context::Value(); }
  Context::Value Break::evaluate(Context&)           { return Context::Value(); }
  Context::Value While::evaluate(Context&)           { return Context::Value(); }
}  // namespace AST
