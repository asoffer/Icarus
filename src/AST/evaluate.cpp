#include "AST.h"
#include "Language.h"
namespace data {
  extern llvm::Value* const_bool(bool b);
  extern llvm::Value* const_char(char c);
  extern llvm::Value* const_int(int n);
  extern llvm::Value* const_real(double d);
  extern llvm::Value* const_uint(size_t n);

}  // namespace data

using InOutVec = std::vector<std::pair<std::vector<Context::Value>, Context::Value>>;
static std::map<AST::FunctionLiteral*, InOutVec> function_call_cache;

namespace AST {
  llvm::Value* Expression::llvm_value(Context::Value v) {
    assert(type != Type_   && "Type_ conversion to llvm::Value*");
    assert(type != Error   && "Error conversion to llvm::Value*");
    assert(type != Unknown && "Unknown conversion to llvm::Value*");

    if (type == Bool)  return data::const_bool(v.as_bool);
    if (type == Char)  return data::const_char(v.as_char);
    if (type == Int)   return data::const_int(v.as_int);
    if (type == Real)  return data::const_real(v.as_real);
    if (type == Uint)  return data::const_uint(v.as_uint);

    return nullptr;
  }

  Context::Value Identifier::evaluate(Context& ctx) {
    // TODO log the struct name in the context of the scope
    if (type != Type_) {
      return ctx.get(shared_from_this());

    } else if (type->is_struct()) {
      return Context::Value(TypeSystem::get(token()));

    } else {

      auto val = ctx.get(shared_from_this());
      assert(val.as_type && "Unknown value for identifier in this scope");

      return val;
    }
  }

  Context::Value Unop::evaluate(Context& ctx) {
    if (op == Language::Operator::Return) {
      ctx.set_return_value(operand->evaluate(ctx));

      return nullptr;

    } else if (op == Language::Operator::Print) {
      auto val = operand->evaluate(ctx);
      if (operand->type == Bool)        std::cout << (val.as_bool ? "true" : "false");
      else if (operand->type == Char)   std::cout << val.as_char;
      else if (operand->type == Int)    std::cout << val.as_int;
      else if (operand->type == Real)   std::cout << val.as_real;
      else if (operand->type == Type_)  std::cout << val.as_type->to_string();
      else if (operand->type == Uint)   std::cout << val.as_uint;
      else { /* TODO */ }

      std::cout.flush();
      return nullptr;

    } else if (op == Language::Operator::Sub) {
      if (type == Int) {
        return Context::Value(-operand->evaluate(ctx).as_int);

      } else if (type == Real) {
        return Context::Value(-operand->evaluate(ctx).as_real);
      }
    } else if (op == Language::Operator::And) {
      if (operand->type != Type_) {
        // TODO better error message
        error_log.log(line_num, "Taking the address of a " + operand->type->to_string() + " is not allowed at compile-time");
      }

      // TODO FIXME I know this is wrong
      return Context::Value(Ptr(operand->evaluate(ctx).as_type));
    }

    assert(false && "Unop eval: I don't know what to do.");
  }

  Context::Value ChainOp::evaluate(Context& ctx) { 
    if (exprs[0]->type == Bool) {
      switch (ops[0]) {
        case Language::Operator::Xor:
          {
            bool expr_val = false;
            for (auto& expr : exprs) {
              expr_val = (expr_val != expr->evaluate(ctx).as_bool);
            }
            return Context::Value(expr_val);
          }
        case Language::Operator::And:
          for (auto& expr : exprs) {
            if (expr->evaluate(ctx).as_bool) return Context::Value(false);
          }
          return Context::Value(true);
        case Language::Operator::Or:
          for (auto& expr : exprs) {
            if (expr->evaluate(ctx).as_bool) {
              return Context::Value(true);
            }
          }
          return Context::Value(false);
        default:;
      }

      return Context::Value(true);

    } else if (exprs[0]->type == Int) {

      bool total = true;
      auto last = exprs[0]->evaluate(ctx);
      for (size_t i = 0; i < ops.size(); ++i) {
        auto next = exprs[i + 1]->evaluate(ctx);

        using Language::Operator;
        switch (ops[i]) {
          case Operator::LessThan:    total &= (last.as_int < next.as_int);
          case Operator::LessEq:      total &= (last.as_int <= next.as_int);
          case Operator::Equal:       total &= (last.as_int == next.as_int);
          case Operator::NotEqual:    total &= (last.as_int != next.as_int);
          case Operator::GreaterThan: total &= (last.as_int >= next.as_int);
          case Operator::GreaterEq:   total &= (last.as_int > next.as_int);
          default:;
        }

        if (!total) { return Context::Value(false); }

        last = next;
      }

      return Context::Value(true);

    } else if (exprs[0]->type == Type_) {
      auto last = exprs[0]->evaluate(ctx);
      for (size_t i = 0; i < ops.size(); ++i) {
        auto next = exprs[i + 1]->evaluate(ctx);

        if (ops[i] == Language::Operator::Equal) {
          if (last.as_type != next.as_type) {
            return Context::Value(false);
          }

        } else if (ops[i] == Language::Operator::NotEqual) {
          if (last.as_type == next.as_type) {
            return Context::Value(false);
          }
        }

        last = next;
      }

      return Context::Value(true);

    } else if (exprs[0]->type == Uint) {
      bool total = true;
      auto last = exprs[0]->evaluate(ctx);
      for (size_t i = 0; i < ops.size(); ++i) {
        auto next = exprs[i + 1]->evaluate(ctx);

        using Language::Operator;
        switch (ops[i]) {
        case Operator::LessThan:
          total &= (last.as_int < next.as_int);
          break;
        case Operator::LessEq:
          total &= (last.as_int <= next.as_int);
          break;
        case Operator::Equal:
          total &= (last.as_int == next.as_int);
          break;
        case Operator::NotEqual:
          total &= (last.as_int != next.as_int);
          break;
        case Operator::GreaterThan:
          total &= (last.as_int >= next.as_int);
          break;
        case Operator::GreaterEq:
          total &= (last.as_int > next.as_int);
          break;
        default:;
        }

        if (!total) {
          return Context::Value(false);
        }

        last = next;
      }

      return Context::Value(true);

    } else {
      return nullptr;
    }
  }

  Context::Value ArrayType::evaluate(Context& ctx)       {
    // TODO what if this is just a compile time array in shorthand?
    return Context::Value(Arr(data_type->evaluate(ctx).as_type));
  }

  Context::Value ArrayLiteral::evaluate(Context&)    { return nullptr; }

  Context::Value Terminal::evaluate(Context& ctx) {
    if (type == Bool) {
      assert((token() == "true" || token() == "false")
          && "Bool literal other than true or false");
      return Context::Value(token() == "true");
    }
    else if (type == Char)  return Context::Value(token()[0]);
    else if (type == Int)   return Context::Value(std::stoi(token()));
    else if (type == Real)  return Context::Value(std::stod(token()));
    else if (type == Uint)  return Context::Value(std::stoul(token()));
    else if (type == Type_) {
      if (token() == "bool") return Context::Value(Bool);
      if (token() == "char") return Context::Value(Char);
      if (token() == "int")  return Context::Value(Int);
      if (token() == "real") return Context::Value(Real);
      if (token() == "type") return Context::Value(Type_);
      if (token() == "uint") return Context::Value(Uint);
      if (token() == "void") return Context::Value(Void);

      error_log.log(line_num, "I don't think `" + token() + "` is a type!");
      return Context::Value(Error);
    }
    else { /* TODO */ }
    return nullptr;
  }

  Context::Value FunctionLiteral::evaluate(Context& ctx) {
    return statements_->evaluate(ctx);
  }

  Context::Value Case::evaluate(Context& ctx) {
    for (size_t i = 0; i < kv->pairs.size() - 1; ++i) {
      auto pair = kv->pairs[i];

      if (pair.first->evaluate(ctx).as_bool) {
        return pair.second->evaluate(ctx);
      }
    }
    return kv->pairs.back().second->evaluate(ctx);
  }

  Context::Value TypeLiteral::evaluate(Context& ctx) {
    static size_t anon_type_counter = 0;
    // TODO just make the type no matter what?
    bool dep_type_flag = false;
    for (const auto& decl : decls_) {
      if (decl->type->has_variables()) {
        dep_type_flag = true;
        break;
      }
    }

    if (!dep_type_flag) return Context::Value(type_value_);

    std::vector<DeclPtr> decls_in_ctx;
    for (const auto& decl : decls_) {
      auto d = std::make_shared<Declaration>();
      d->is_inferred = false;
      d->identifier = std::make_shared<Identifier>(0, decl->identifier->token());
      d->identifier->decl = d.get();
      decls_in_ctx.push_back(d);

      // TODO finish setting data in d so that we can safely print this
      // out for debugging

      auto dtype = decl->type_expr->evaluate(ctx).as_type;
      d->type = dtype;
      if (dtype == Int) {
        auto intnode = std::make_shared<TokenNode>(0, Language::type_literal, "int");
        d->type_expr = std::static_pointer_cast<Expression>(
            Terminal::build_type_literal({ intnode }));

      } else if (dtype == Char) {
        auto intnode = std::make_shared<TokenNode>(0, Language::type_literal, "char");
        d->type_expr = std::static_pointer_cast<Expression>(
            Terminal::build_type_literal({ intnode }));
      }
    }

    // TODO note that this will be captured if it's the result of a
    // function call by the cache, but otherwise it's currently leaked
    auto type_lit_ptr = new TypeLiteral;
    type_lit_ptr->decls_ = std::move(decls_in_ctx);

    type_lit_ptr->type_value_ = 
      Struct("__anon.param.struct" + std::to_string(anon_type_counter++), type_lit_ptr);
    type_lit_ptr->build_llvm_internals();

    Dependency::mark_as_done(type_lit_ptr);

    return Context::Value(type_lit_ptr->type_value_);
  }

  Context::Value Assignment::evaluate(Context&)  { return nullptr; }

  Context::Value Declaration::evaluate(Context& ctx) {
    if (is_inferred) {
      if (type_expr->type->is_function()) {
        ctx.bind(Context::Value(type_expr.get()), identifier);
      } else {
        auto type_as_ctx_val = type_expr->evaluate(ctx);
        ctx.bind(type_as_ctx_val, identifier);

        if (type_expr->is_type_literal()) {
          assert(type_as_ctx_val.as_type->is_struct());
          static_cast<Structure*>(type_as_ctx_val.as_type)->set_name(identifier->token());

        } else if (type_expr->is_enum_literal()) {
          assert(type_as_ctx_val.as_type->is_enum());
          static_cast<Enumeration*>(type_as_ctx_val.as_type)->set_name(identifier->token());
        } 
      }
    } else {
      if (type_expr->type == Type_) {
        ctx.bind(Context::Value(TypeVar(identifier)), identifier);
      } else if (type_expr->type->is_type_variable()) {
        // TODO Should we just skip this?
      } else { /* There's nothing to do */ }
    }

/*
      // To do nice printing, we want to replace __anon... with a name. For
      // now, we just choose the first name that was bound to it.
      //
      // TODO come up with a better way to
      // 1. figure out what name to print
      // 2. determine if the type chosen hasn't had a name bound to it yet.
      if (type_for_binding->to_string()[0] == '_') {
        if (type_for_binding->is_enum()) {
          static_cast<Enumeration*>(type_for_binding)->set_name(token());

        } else if (type_for_binding->is_struct()) {
          static_cast<Structure*>(type_for_binding)->set_name(token());

        } else {
          assert(false && "non-enum non-struct starting with '_'");
        }
      }
      assert(scope_->context().get(identifier).as_type && "Bound type was a nullptr");
*/
    return nullptr;
  }

  Context::Value EnumLiteral::evaluate(Context&) {
    return Context::Value(type_value_);
  }

  Context::Value Access::evaluate(Context& ctx) {
    assert(false && "not yet implemented");
  }

  Context::Value Binop::evaluate(Context& ctx) {
    using Language::Operator;
    if (op == Operator::Call) {
      assert(lhs->type->is_function());
      auto lhs_val = lhs->evaluate(ctx).as_expr;
      assert(lhs_val);
      auto fn_ptr = static_cast<FunctionLiteral*>(lhs_val);

      std::vector<EPtr> arg_vals;
      if (rhs->is_comma_list()) {
        arg_vals = std::static_pointer_cast<ChainOp>(rhs)->exprs;
      } else {
        arg_vals.push_back(rhs);
      }     

      assert(arg_vals.size() == fn_ptr->inputs_.size());

      std::vector<Context::Value> ctx_vals;

      // Populate the function context with arguments
      for (size_t i = 0; i < arg_vals.size(); ++i) {
        auto rhs_eval = arg_vals[i]->evaluate(ctx);
        ctx_vals.push_back(rhs_eval);
      }

      // For functions that return types, we cache all calls
      // TODO add possibility for #nocache
      bool returns_type =
        (static_cast<Function*>(fn_ptr->type)->return_type() == Type_);

      auto& fn_cache = function_call_cache[fn_ptr];
      if (returns_type) {
        for (const auto& cache_entry : fn_cache) {
          size_t cache_size = cache_entry.first.size();

          assert(ctx_vals.size() == cache_size);
          // Because it's a call to a function with that many arguments!

          bool matches = true;
          for (size_t i = 0; i < cache_size; ++i) {
            if (cache_entry.first[i] != ctx_vals[i]) {
              matches = false;
              break;
            }
          }

          if (matches) return cache_entry.second;
        }
      }


      Context fn_ctx = ctx.spawn();
      for (size_t i = 0; i < arg_vals.size(); ++i) {
        fn_ctx.bind(ctx_vals[i], fn_ptr->inputs_[i]->identifier);
      }

      fn_cache.emplace_back(ctx_vals, Context::Value(static_cast<Type*>(nullptr)));
      auto& cached_val = fn_cache.back();
      auto return_val = fn_ptr->evaluate(fn_ctx);
      if (returns_type) {
        delete cached_val.second.as_type;
        cached_val.second = return_val;
      }
      return return_val;

    } else if (op == Operator::Arrow) {
      auto lhs_type = lhs->evaluate(ctx).as_type;
      auto rhs_type = rhs->evaluate(ctx).as_type;
      return Context::Value(Func(lhs_type, rhs_type));
    }

    return nullptr;
  }

  Context::Value KVPairList::evaluate(Context&)      { return nullptr; }

  Context::Value Statements::evaluate(Context& ctx) {
    for (auto& stmt : statements_) {
      stmt->evaluate(ctx);
      if (ctx.has_return()) {
        return ctx.return_value();
      }
    }

    return nullptr;
  }

  Context::Value Conditional::evaluate(Context& ctx) {
    for (size_t i = 0; i < conds_.size(); ++i) {
      if (conds_[i]->evaluate(ctx).as_bool) {
        Context cond_ctx(&ctx);
        statements_[i]->evaluate(cond_ctx);
        if (cond_ctx.has_return()) {
          return cond_ctx.return_value();
        }
      }
    }

    if (has_else()) {
        Context cond_ctx(&ctx);
        statements_.back()->evaluate(cond_ctx);
        if (cond_ctx.has_return()) {
          return cond_ctx.return_value();
        }
    }

    return nullptr;
  }

  Context::Value Break::evaluate(Context&)           { return nullptr; }
  Context::Value While::evaluate(Context&)           { return nullptr; }
}  // namespace AST
