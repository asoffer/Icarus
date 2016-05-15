#ifndef ICARUS_UNITY
#include "Scope.h"
#include "Type.h"
#endif

namespace TypeSystem {
void initialize();
extern Type *get(const std::string &name);
} // namespace TypeSystem

namespace debug {
extern bool parametric_struct;
} // namespace debug

namespace data {
extern llvm::ConstantInt *const_bool(bool b);
extern llvm::ConstantInt *const_char(char c);
extern llvm::ConstantInt *const_int(long n);
extern llvm::ConstantFP *const_real(double d);
extern llvm::ConstantInt *const_uint(size_t n);
} // namespace data

static void AppendValueToStream(Type *type, Context::Value ctx_val,
                                std::stringstream &ss) {
  if (type == Bool) {
    ss << (ctx_val.as_bool ? "true" : "false");
  } else if (type == Char) {
    ss << ctx_val.as_char;

  } else if (type == Int) {
    ss << ctx_val.as_int;

  } else if (type == Real) {
    ss << ctx_val.as_real;

  } else if (type == Uint) {
    ss << ctx_val.as_uint;

  } else if (type == Type_) {
    ss << ctx_val.as_type->to_string();
  }
}

namespace AST {
llvm::Value *Expression::llvm_value(Context::Value v) {
  assert(type != Type_ && "Type_ conversion to llvm::Value*");
  assert(type != Error && "Error conversion to llvm::Value*");
  assert(type != Unknown && "Unknown conversion to llvm::Value*");

  if (type == Bool) return data::const_bool(v.as_bool);
  if (type == Char) return data::const_char(v.as_char);
  if (type == Int) return data::const_int(v.as_int);
  if (type == Real) return data::const_real(v.as_real);
  if (type == Uint) return data::const_uint(v.as_uint);

  // TODO
  return nullptr;
}

Context::Value Identifier::evaluate(Context &ctx) {
  // TODO log the struct name in the context of the scope
  if (type != Type_) {
    return ctx.get(this);

  } else if (type->is_struct()) {
    return Context::Value(TypeSystem::get(token()));

  } else {
    auto val = ctx.get(this);

    assert(val.as_type && "Unknown value for identifier in this scope");

    return val;
  }
}

Context::Value DummyTypeExpr::evaluate(Context &) { return value; }

Context::Value Unop::evaluate(Context &ctx) {
  if (op == Language::Operator::Return) {
    ctx.set_return_value(operand->evaluate(ctx));

    return nullptr;

  } else if (op == Language::Operator::Print) {
    auto val = operand->evaluate(ctx);
    if (operand->type == Bool)
      std::cout << (val.as_bool ? "true" : "false");
    else if (operand->type == Char)
      std::cout << val.as_char;
    else if (operand->type == Int)
      std::cout << val.as_int;
    else if (operand->type == Real)
      std::cout << val.as_real;
    else if (operand->type == Type_)
      std::cout << val.as_type->to_string();
    else if (operand->type == Uint)
      std::cout << val.as_uint;
    else { /* TODO */
    }

    std::cout.flush();
    return nullptr;

  } else if (op == Language::Operator::Sub) {
    if (type == Int) {
      return Context::Value(-operand->evaluate(ctx).as_int);

    } else if (type == Real) {
      return Context::Value(-operand->evaluate(ctx).as_real);
    }
  } else if (op == Language::Operator::And) {
    if (operand->type == Unknown) {
      // Create a cached value for the operand
      assert(false);

    } else if (operand->type != Type_) {
      std::cout << *this << operand->type << std::endl;
      // TODO better error message
      error_log.log(loc, "Taking the address of a " +
                             operand->type->to_string() +
                             " is not allowed at compile-time");
    }

    return Context::Value(Ptr(operand->evaluate(ctx).as_type));
  }

  assert(false && "Unop eval: I don't know what to do.");
}

Context::Value ChainOp::evaluate(Context &ctx) {
  using Language::Operator;
  auto expr_type = exprs[0]->type;
  if (expr_type == Bool) {
    switch (ops[0]) {
    case Operator::Xor: {
      bool expr_val = false;
      for (auto &expr : exprs) {
        expr_val = (expr_val != expr->evaluate(ctx).as_bool);
      }
      return Context::Value(expr_val);
    }
    case Operator::And:
      for (auto &expr : exprs) {
        if (expr->evaluate(ctx).as_bool) return Context::Value(false);
      }
      return Context::Value(true);
    case Operator::Or:
      for (auto &expr : exprs) {
        if (expr->evaluate(ctx).as_bool) { return Context::Value(true); }
      }
      return Context::Value(false);
    default: assert(false && "Invalid chainop for bool in evaluate()");
    }

    return Context::Value(true);

  } else if (expr_type == Int) {
    bool total = true;
    auto last = exprs[0]->evaluate(ctx);
    for (size_t i = 0; i < ops.size(); ++i) {
      auto next = exprs[i + 1]->evaluate(ctx);

      switch (ops[i]) {
      case Operator::LT: total &= (last.as_int < next.as_int); break;
      case Operator::LE: total &= (last.as_int <= next.as_int); break;
      case Operator::EQ: total &= (last.as_int == next.as_int); break;
      case Operator::NE: total &= (last.as_int != next.as_int); break;
      case Operator::GE: total &= (last.as_int >= next.as_int); break;
      case Operator::GT: total &= (last.as_int > next.as_int); break;
      default: assert(false && "Invalid chainop for int in evaluate()");
      }

      if (!total) { return Context::Value(false); }

      last = next;
    }

    return Context::Value(true);

  } else if (expr_type == Type_) {
    bool total = true;
    auto last = exprs[0]->evaluate(ctx);
    for (size_t i = 0; i < ops.size(); ++i) {
      auto next = exprs[i + 1]->evaluate(ctx);

      switch (ops[i]) {
      case Operator::EQ: total &= (last.as_type == next.as_type); break;
      case Operator::NE: total &= (last.as_type != next.as_type); break;
      default: assert(false && "Invalid chainop for type in evaluate()");
      }

      if (!total) { return Context::Value(false); }

      last = next;
    }

    return Context::Value(true);

  } else if (expr_type == Uint) {
    bool total = true;
    auto last = exprs[0]->evaluate(ctx);
    for (size_t i = 0; i < ops.size(); ++i) {
      auto next = exprs[i + 1]->evaluate(ctx);

      switch (ops[i]) {
      case Operator::LT: total &= (last.as_int < next.as_int); break;
      case Operator::LE: total &= (last.as_int <= next.as_int); break;
      case Operator::EQ: total &= (last.as_int == next.as_int); break;
      case Operator::NE: total &= (last.as_int != next.as_int); break;
      case Operator::GE: total &= (last.as_int >= next.as_int); break;
      case Operator::GT: total &= (last.as_int > next.as_int); break;
      default: assert(false && "Invalid chainop for uint in evaluate()");
      }

      if (!total) { return Context::Value(false); }

      last = next;
    }

    return Context::Value(true);

  } else if (expr_type == Real) {
    bool total = true;
    auto last = exprs[0]->evaluate(ctx);
    for (size_t i = 0; i < ops.size(); ++i) {
      auto next = exprs[i + 1]->evaluate(ctx);

      switch (ops[i]) {
      case Operator::LT: total &= (last.as_real < next.as_real); break;
      case Operator::LE: total &= (last.as_real <= next.as_real); break;
      case Operator::EQ: total &= (last.as_real == next.as_real); break;
      case Operator::NE: total &= (last.as_real != next.as_real); break;
      case Operator::GE: total &= (last.as_real >= next.as_real); break;
      case Operator::GT: total &= (last.as_real > next.as_real); break;
      default: assert(false && "Invalid chainop for uint in evaluate()");
      }

      if (!total) { return Context::Value(false); }

      last = next;
    }

    return Context::Value(true);

  } else if (expr_type->is_enum()) {
    bool total = true;
    auto last = exprs[0]->evaluate(ctx);
    for (size_t i = 0; i < ops.size(); ++i) {
      auto next = exprs[i + 1]->evaluate(ctx);

      switch (ops[i]) {
      case Operator::EQ: total &= (last.as_uint == next.as_uint); break;
      case Operator::NE: total &= (last.as_uint != next.as_uint); break;
      default: assert(false && "Invalid chainop for enum in evaluate()");
      }

      if (!total) { return Context::Value(false); }

      last = next;
    }

    return Context::Value(true);
  } else {
    assert(false && "ChainOp::evaluate case unhandled");
  }
}

Context::Value ArrayType::evaluate(Context &ctx) {
  assert(length);
  determine_time();
  if ((length->time() == Time::either || length->time() == Time::compile) &&
      !length->is_hole()) {
    auto data_type_eval = data_type->evaluate(ctx).as_type;
    auto length_eval    = length->evaluate(ctx).as_uint;

    return Context::Value(Arr(data_type_eval, length_eval));
  }

  return Context::Value(Arr(data_type->evaluate(ctx).as_type));
}

Context::Value ArrayLiteral::evaluate(Context &) { return nullptr; }

Context::Value Terminal::evaluate(Context &ctx) {
  if (type == Bool) {
    assert((token() == "true" || token() == "false") &&
           "Bool literal other than true or false");
    return Context::Value(terminal_type == Language::Terminal::True);
  } else if (type == Char) {
    return Context::Value(token()[0]);
  } else if (type == Int) {
    return Context::Value(std::stol(token()));
  } else if (type == Real) {
    return Context::Value(std::stod(token()));
  } else if (type == Uint) {
    return Context::Value(std::stoul(token()));
  } else if (type == Type_) {
    if (token() == "bool") return Context::Value(Bool);
    if (token() == "char") return Context::Value(Char);
    if (token() == "int") return Context::Value(Int);
    if (token() == "real") return Context::Value(Real);
    if (token() == "type") return Context::Value(Type_);
    if (token() == "uint") return Context::Value(Uint);
    if (token() == "void") return Context::Value(Void);

    error_log.log(loc, "I don't think `" + token() + "` is a type!");
    return Context::Value(Error);
  } else { /* TODO */
  }
  return nullptr;
}

Context::Value FunctionLiteral::evaluate(Context &ctx) {
  return statements->evaluate(ctx);
}

Context::Value Case::evaluate(Context &ctx) {
  for (auto kv : key_vals) {
    if (kv.first->evaluate(ctx).as_bool) { return kv.second->evaluate(ctx); }
  }
  // Must have an else-clause, so this is unreachable.
  assert(false);
}

Context::Value StructLiteral::evaluate(Context &ctx) { return value; }

Context::Value InDecl::evaluate(Context &ctx) { return nullptr; }

Context::Value Declaration::evaluate(Context &ctx) {
  switch (decl_type) {
  case DeclType::Infer: {
    if (expr->type->is_function()) {
      ctx.bind(Context::Value(expr), identifier);
    } else {
      auto type_as_ctx_val = expr->evaluate(ctx);
      ctx.bind(type_as_ctx_val, identifier);

      if (expr->is_struct_literal()) {
        if (type_as_ctx_val.as_type->is_struct()) {
          static_cast<Structure *>(type_as_ctx_val.as_type)
              ->set_name(identifier->token());
        } else if (type_as_ctx_val.as_type->is_parametric_struct()) {
          static_cast<ParametricStructure *>(type_as_ctx_val.as_type)
              ->set_name(identifier->token());
        } else {
          assert(false);
        }

      } else if (expr->is_enum_literal()) {
        assert(type_as_ctx_val.as_type->is_enum());
        static_cast<Enumeration *>(type_as_ctx_val.as_type)->bound_name =
            identifier->token();
      }
    }
  } break;
  case DeclType::Std: {
    if (expr->type == Type_) {
      ctx.bind(Context::Value(TypeVar(identifier)), identifier);
    } else if (expr->type->is_type_variable()) {
      // TODO Should we just skip this?
    } else { /* There's nothing to do */
    }
  } break;
  case DeclType::Tick: {
    // Being asked to evaluate a tick, is just being asked to figure out what
    // type it must represent from the available information. There is very
    // little information here, since it's a generic function, so we simply bind
    // a type variable and return it.
    ctx.bind(Context::Value(TypeVar(identifier, expr)), identifier);
    return ctx.get(identifier);
  }
  }

  return nullptr;
}

Context::Value EnumLiteral::evaluate(Context &) { return value; }

Context::Value Access::evaluate(Context &ctx) {
  if (type->is_enum()) {
    auto enum_type = (Enumeration *)type;
    return Context::Value(enum_type->get_index(member_name));
  }
  assert(false && "not yet implemented");
}

Context::Value Binop::evaluate(Context &ctx) {
  using Language::Operator;
  if (op == Operator::Call) {
    if (lhs->type->is_function()) {
      auto lhs_val = lhs->evaluate(ctx).as_expr;
      assert(lhs_val);
      auto fn_ptr = static_cast<FunctionLiteral *>(lhs_val);

      std::vector<Expression *> arg_vals;
      if (rhs->is_comma_list()) {
        arg_vals = static_cast<ChainOp *>(rhs)->exprs;
      } else {
        arg_vals.push_back(rhs);
      }

      assert(arg_vals.size() == fn_ptr->inputs.size());

      std::vector<Context::Value> ctx_vals;

      // Populate the function context with arguments
      for (size_t i = 0; i < arg_vals.size(); ++i) {
        auto rhs_eval = arg_vals[i]->evaluate(ctx);
        ctx_vals.push_back(rhs_eval);
      }

      Context fn_ctx = ctx.spawn();
      for (size_t i = 0; i < arg_vals.size(); ++i) {
        fn_ctx.bind(ctx_vals[i], fn_ptr->inputs[i]->identifier);
      }

      return fn_ptr->evaluate(fn_ctx);

    } else if (lhs->type == Type_) {
      auto lhs_evaled = lhs->evaluate(ctx).as_type;
      assert(lhs_evaled->is_parametric_struct());

      auto param_struct = static_cast<ParametricStructure *>(lhs_evaled);
      auto struct_lit   = param_struct->ast_expression;

      if (debug::parametric_struct) {
        std::cout << "\n== Evaluating a parametric struct call ==\n"
                  << *struct_lit << std::endl;
      }
      std::vector<Expression *> arg_vals;
      if (rhs->is_comma_list()) {
        arg_vals = static_cast<ChainOp *>(rhs)->exprs;
      } else {
        arg_vals.push_back(rhs);
      }

      auto num_args = arg_vals.size();

      if (debug::parametric_struct) {
        std::cout << " * Argument values:" << std::endl;
      }
      std::vector<Context::Value> ctx_vals;
      // Populate the function context with arguments
      for (size_t i = 0; i < num_args; ++i) {
        auto rhs_eval = arg_vals[i]->evaluate(ctx);
        ctx_vals.push_back(rhs_eval);

        if (debug::parametric_struct) {
          std::cout << "   " << i << ". " << *ctx_vals.back().as_type
                    << std::endl;
        }
      }

      if (debug::parametric_struct) { std::cout << std::endl; }

      // look through the cache
      // TODO there's definitely a better way to do this.

      size_t cache_num = 0;
      for (const auto &cached_val : struct_lit->cache) {
        if (debug::parametric_struct) {
          std::cout << " * Checking match against cache position " << cache_num
                    << std::endl;
        }

        if (cached_val.first.size() != num_args) {
          if (debug::parametric_struct) {
            std::cout << "   - Parameter number mismatch (" << num_args
                      << " vs " << cached_val.first.size() << ")"
                      << std::endl;
          }
          continue;
        }

        for (size_t i = 0; i < num_args; ++i) {
          // TODO not always a type
          if (ctx_vals[i].as_type != cached_val.first[i]) {

            if (debug::parametric_struct) {
              std::cout << "   - Failed matching argument " << i << std::endl;
            }

            goto outer_continue;
          }
        }

        if (debug::parametric_struct) {
          std::cout << "   - Found a match." << std::endl;
        }
        // If you get down here, you have found the right thing.
        return cached_val.second->value;

      outer_continue:;
      }

      // TODO there's definitely a way to consolidate/speed up this stuff.
      // Create the key earlier and do a binary rather than linear search
      // through the cache using this key.
      std::vector<Type *> vec_key;
      for (size_t i = 0; i < num_args; ++i) {
        vec_key.push_back(ctx_vals[i].as_type);
      }
        
      auto &cache_loc = (struct_lit->cache[vec_key] = new StructLiteral);

      // TODO move the functionality of verify_types out into another function
      // and have this call that function and verify_types call that as well.
      // The naming is wacky. The call here is just to use the value assignment
      // functionality.
      cache_loc->verify_types();

      if (debug::parametric_struct) {
        std::cout << " * No match found.\n"
                     " * Creating new cached value.\n"
                     " * Cache size is now "
                  << struct_lit->cache.size() << " for " << struct_lit << "."
                  << std::endl;
        assert(struct_lit->cache.size() < 5);
      }

      Context struct_ctx = ctx.spawn();
      for (size_t i = 0; i < arg_vals.size(); ++i) {
        struct_ctx.bind(ctx_vals[i], struct_lit->params[i]->identifier);
      }

      auto cloned_struct = param_struct->ast_expression->CloneStructLiteral(
          cache_loc, struct_ctx);

      std::stringstream ss;
      // TODO if the parameter is not a type?
      ss << param_struct->bound_name << "(";

      auto param_type = struct_lit->params[0]->identifier->type;
      AppendValueToStream(param_type, ctx_vals[0], ss);

      for (size_t i = 1; i < arg_vals.size(); ++i) {
        auto parameter_type = struct_lit->params[0]->identifier->type;
        ss << ", ";
        AppendValueToStream(parameter_type, ctx_vals[i], ss);
      }
      ss << ")";

      // TODO move the functionality of verify_types out into another function
      // and have this call that function and verify_types call that as well.
      // The naming is wacky. The call here is just to use the value assignment
      // functionality.
      cloned_struct->verify_types();
      static_cast<Structure *>(cloned_struct->value.as_type)->set_name(ss.str());

      auto struct_type =
          static_cast<Structure *>(cloned_struct->value.as_type);
      if (struct_type->field_type.size() == 0) {
        for (auto decl : cloned_struct->declarations) {
          bool is_inferred = (decl->decl_type == DeclType::Infer);

          Type *field =
              is_inferred ? decl->expr->type
                          : decl->expr->evaluate(scope_->context).as_type;
          assert(field && "field is nullptr");
          struct_type->insert_field(decl->identifier->token(), field,
                                    is_inferred ? decl->expr : nullptr);
        }
      }

      return cloned_struct->value;
    } else {
      assert(false);
    }

  } else if (op == Operator::Arrow) {
    auto lhs_type = lhs->evaluate(ctx).as_type;
    auto rhs_type = rhs->evaluate(ctx).as_type;
    return Context::Value(Func(lhs_type, rhs_type));
  }

  return nullptr;
}

Context::Value Statements::evaluate(Context &ctx) {
  for (auto &stmt : statements) {
    stmt->evaluate(ctx);
    if (ctx.has_return()) { return ctx.return_value(); }
  }

  return nullptr;
}

Context::Value Conditional::evaluate(Context &ctx) {
  for (size_t i = 0; i < conditions.size(); ++i) {
    if (conditions[i]->evaluate(ctx).as_bool) {
      Context cond_ctx(&ctx);
      statements[i]->evaluate(cond_ctx);
      if (cond_ctx.has_return()) { return cond_ctx.return_value(); }
    }
  }

  if (has_else()) {
    Context cond_ctx(&ctx);
    statements.back()->evaluate(cond_ctx);
    if (cond_ctx.has_return()) { return cond_ctx.return_value(); }
  }

  return nullptr;
}

Context::Value Jump::evaluate(Context &) {
  assert(false && "Not yet implemented");
}

Context::Value While::evaluate(Context &) {
  assert(false && "Not yet implemented");
}

Context::Value For::evaluate(Context &) {
  assert(false && "Not yet implemented");
}
} // namespace AST
