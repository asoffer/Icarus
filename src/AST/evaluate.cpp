#include "AST.h"

namespace data {
  extern llvm::Value* const_bool(bool b);
  extern llvm::Value* const_char(char c);
  extern llvm::Value* const_int(int n);
  extern llvm::Value* const_real(double d);
  extern llvm::Value* const_uint(size_t n);

}  // namespace data

namespace AST {
  llvm::Value* Expression::llvm_value(Context::Value v) {
    if (type() == Type::get_bool()) {
      return data::const_bool(v.as_bool);

    } else if (type() == Type::get_char()) {
      return data::const_char(v.as_char);

    } else if (type() == Type::get_int()) {
      return data::const_int(v.as_int);

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

    } else if (token() == "-") {
      if (type() == Type::get_int()) {
        return Context::Value(-expr_->evaluate(ctx).as_int);

      } else if (type() == Type::get_real()) {
        return Context::Value(-expr_->evaluate(ctx).as_real);
      }

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
          return Context::Value(false);
        }

      } else if (ops_[i]->token() == "!=") {
        if (last->interpret_as_type() == next->interpret_as_type()) {
          return Context::Value(false);
        }
      }
    }

    return Context::Value(true);
  }

  Context::Value ArrayType::evaluate(Context&)       { return Context::Value(); }
  Context::Value ArrayLiteral::evaluate(Context&)    { return Context::Value(); }

  Context::Value Terminal::evaluate(Context& ctx) {
    if (type() == Type::get_bool()) {
      if (token() == "true") {
        return Context::Value(true);

      } else if (token() == "false") {
        return Context::Value(false);

      } else {
        std::cerr << "FATAL: BOOL LITERAL?" << std::endl;
        return Context::Value();
      }

    } else if (type() == Type::get_char()) {
      return Context::Value(token()[0]);

    } else if (type() == Type::get_int()) {
      return Context::Value(std::stoi(token()));

    } else if (type() == Type::get_real()) {
      return Context::Value(std::stod(token()));

    } else if (type() == Type::get_uint()) {
      return Context::Value(std::stoul(token()));

    } else {
      // TODO
      return Context::Value();
    }
  }

  Context::Value FunctionLiteral::evaluate(Context& ctx) {
    return statements_->evaluate(ctx);
  }

  Context::Value Case::evaluate(Context&)            { return Context::Value(); }
  Context::Value Assignment::evaluate(Context&)      { return Context::Value(); }
  Context::Value Declaration::evaluate(Context&)     { return Context::Value(); }
  Context::Value TypeLiteral::evaluate(Context&)     { return Context::Value(); }
  Context::Value EnumLiteral::evaluate(Context&)     { return Context::Value(); }

  Context::Value Binop::evaluate(Context& ctx) {
    if (token() == "()") {
      if (lhs_->is_identifier()) {
        auto fn = ctx.get(std::static_pointer_cast<Identifier>(lhs_));
        Context fn_ctx = Context::GlobalContext.spawn();

        // TODO populate the function context with arguments

        auto x = fn->evaluate(fn_ctx);
        return x;
      }
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
