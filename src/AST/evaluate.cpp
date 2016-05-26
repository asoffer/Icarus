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

static void AppendValueToStream(Type *type, const Context::Value val,
                                std::ostream &os) {
  if (type == Bool) {
    os << (val.as_bool ? "true" : "false");
  } else if (type == Char) {
    os << val.as_char;

  } else if (type == Int) {
    os << val.as_int;

  } else if (type == Real) {
    os << val.as_real;

  } else if (type == Uint) {
    os << val.as_uint;

  } else if (type == Type_) {
    os << val.as_type->to_string();
  }
}

namespace AST {
// TODO there's definitely a better way to do this.
Context::Value ParametricStructLiteral::CreateOrGetCached(const Ctx &arg_vals) {
  size_t cache_num = 0;
  auto num_args = arg_vals.size();
  assert(value.as_type);
  assert(value.as_type->is_parametric_struct());
  auto param_struct = (ParametricStructure *)value.as_type;

  for (const auto &cached_val : cache) {
    if (debug::parametric_struct) {
      std::cerr << " * Checking match against cache position " << cache_num++
                << std::endl;
    }

    if (cached_val.first.size() != num_args) {
      if (debug::parametric_struct) {
        std::cerr << "   - Parameter number mismatch (" << num_args << " vs "
                  << cached_val.first.size() << ")" << std::endl;
      }
      continue;
    }

    for (auto kv : arg_vals) {
      if (cached_val.first.at(kv.first) != kv.second) {
        if (debug::parametric_struct) {
          std::cerr << "   - Failed matching argument " << kv.first
                    << std::endl;
        }
        goto outer_continue;
      }
    }

    // If you get here, you found a match
    if (debug::parametric_struct) {
      std::cerr << "   - Found a match." << std::endl;
      // TODO which match?
    }
    // If you get down here, you have found the right thing.
    return cached_val.second->value;

  outer_continue:;
  }

  auto &cache_loc = (cache[arg_vals] = new StructLiteral);
  reverse_cache[cache_loc] = arg_vals;

  std::stringstream ss;
  ss << param_struct->bound_name << "(";

  Type *parameter_type;
  if (params.type_exprs[0]) {
    Ctx ctx;
    parameter_type = params.type_exprs[0]->evaluate(ctx).as_type;
  } else {
    assert(params.init_vals[0]);
    params.init_vals[0]->verify_types();
    parameter_type = params.init_vals[0]->type;
  }

  AppendValueToStream(parameter_type, arg_vals.at(params.ids[0]), ss);

  for (size_t i = 1; i < num_args; ++i) {
    if (params.type_exprs[i]) {
      Ctx ctx;
      parameter_type = params.type_exprs[i]->evaluate(ctx).as_type;
    } else {
      assert(params.init_vals[i]);
      params.init_vals[i]->verify_types();
      parameter_type = params.init_vals[i]->type;
    }

    ss << ", ";
    AppendValueToStream(parameter_type, arg_vals.at(params.ids[0]), ss);
  }
  ss << ")";

  cache_loc->value = Context::Value(Struct(ss.str(), cache_loc));

  if (debug::parametric_struct) {
    std::cerr << " * No match found.\n"
                 " * Creating new cached value.\n"
                 " * Cache size is now "
              << cache.size() << " for " << *this << "." << std::endl;
    // For debugging so we don't get too far generating these things.
    assert(cache.size() < 5);
  }


  auto cloned_struct =
      param_struct->ast_expression->CloneStructLiteral(cache_loc);

  cloned_struct->verify_types();
  static_cast<Structure *>(cloned_struct->value.as_type)->set_name(ss.str());

  return cloned_struct->value;
}

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

Context::Value Identifier::evaluate(Ctx &ctx) {
  // What about when it's, e.g., an int with the value 0?
  auto iter = ctx.find(token);
  if (iter != ctx.end()) { return iter->second; }

  if (type == Type_ && !value.as_type) {
    assert(decls.size() == 1);
    decls[0]->evaluate(ctx);
  }

  return value;
}

Context::Value DummyTypeExpr::evaluate(Ctx &) { return value; }

Context::Value Unop::evaluate(Ctx &ctx) {
  operand->verify_types();

  if (op == Language::Operator::Return) {
    scope_->SetCTRV(operand->evaluate(ctx));

    return value = nullptr;

  } else if (op == Language::Operator::Print) {
    // TODO Don't print. Raise an error.
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
    return value = nullptr;

  } else if (op == Language::Operator::Sub) {
    if (type == Int) {
      return Context::Value(-operand->evaluate(ctx).as_int);

    } else if (type == Real) {
      return Context::Value(-operand->evaluate(ctx).as_real);
    }
  } else if (op == Language::Operator::And) {
    if (operand->type != Type_) {
      // TODO better error message
      error_log.log(loc, "Taking the address of a " +
                             operand->type->to_string() +
                             " is not allowed at compile-time");
    }

    return value = Context::Value(Ptr(operand->evaluate(ctx).as_type));
  }

  assert(false && "Unop eval: I don't know what to do.");
}

Context::Value ChainOp::evaluate(Ctx &ctx) {
  using Language::Operator;
  auto expr_type = exprs[0]->type;
  if (expr_type == Bool) {
    switch (ops[0]) {
    case Operator::Xor: {
      bool expr_val = false;
      for (auto &expr : exprs) {
        expr_val = (expr_val != expr->evaluate(ctx).as_bool);
      }
      return value = Context::Value(expr_val);
    }
    case Operator::And:
      for (auto &expr : exprs) {
        if (expr->evaluate(ctx).as_bool) return value = Context::Value(false);
      }
      return value = Context::Value(true);
    case Operator::Or:
      for (auto &expr : exprs) {
        if (expr->evaluate(ctx).as_bool) { return value = Context::Value(true); }
      }
      return value = Context::Value(false);
    default: assert(false && "Invalid chainop for bool in evaluate()");
    }

    return value = Context::Value(true);

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

      if (!total) { return value = Context::Value(false); }

      last = next;
    }

    return value = Context::Value(true);

  } else if (expr_type == Type_) {
    // TODO remove assumption that if you're using a comma, it's all types
    if (ops[0] == Operator::Comma) {
      std::vector<Type *> types;
      for (size_t i = 0; i < exprs.size(); ++i) {
        types.push_back(exprs[i]->evaluate(ctx).as_type);
      }
      return Context::Value(Tup(types));
    }

    bool total = true;
    auto last = exprs[0]->evaluate(ctx);
    for (size_t i = 0; i < ops.size(); ++i) {
      auto next = exprs[i + 1]->evaluate(ctx);

      switch (ops[i]) {
      case Operator::EQ: total &= (last.as_type == next.as_type); break;
      case Operator::NE: total &= (last.as_type != next.as_type); break;
      default: assert(false && "Invalid chainop for type in evaluate()");
      }

      if (!total) { return value = Context::Value(false); }

      last = next;
    }

    return value = Context::Value(true);

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

      if (!total) { return value = Context::Value(false); }

      last = next;
    }

    return value = Context::Value(true);

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

      if (!total) { return value = Context::Value(false); }

      last = next;
    }

    return value = Context::Value(true);

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

      if (!total) { return value = Context::Value(false); }

      last = next;
    }

    return value = Context::Value(true);
  } else {
    std::cerr << *this << std::endl;
    assert(false && "ChainOp::evaluate case unhandled");
  }
}

Context::Value ArrayType::evaluate(Ctx &ctx) {
  assert(length);
  determine_time();
  if ((length->time() == Time::either || length->time() == Time::compile) &&
      !length->is_hole()) {
    auto data_type_eval = data_type->evaluate(ctx).as_type;
    auto length_eval    = length->evaluate(ctx).as_uint;

    return value = Context::Value(Arr(data_type_eval, length_eval));
  }

  return value = Context::Value(Arr(data_type->evaluate(ctx).as_type));
}

Context::Value ArrayLiteral::evaluate(Ctx &ctx) { assert(false); }

// TODO ord, ascii
Context::Value Terminal::evaluate(Ctx &ctx) { return value; }

Context::Value FunctionLiteral::evaluate(Ctx &ctx) {
  return statements->evaluate(ctx);
}

Context::Value Case::evaluate(Ctx& ctx) {
  for (auto kv : key_vals) {
    if (kv.first->evaluate(ctx).as_bool) { return kv.second->evaluate(ctx); }
  }
  // Must have an else-clause, so this is unreachable.
  assert(false);
}

Context::Value ParametricStructLiteral::evaluate(Ctx& ctx) { return value; }
Context::Value StructLiteral::evaluate(Ctx& ctx) { return value; }
Context::Value InDecl::evaluate(Ctx& ctx) { return nullptr; }

Context::Value Declaration::evaluate(Ctx& ctx) {
  switch (decl_type) {
  case DeclType::Infer: {
    if (type_expr->type->is_function()) {
      identifier->value = Context::Value(type_expr);
    } else {
      identifier->value = type_expr->evaluate(ctx);

      if (type_expr->is_struct_literal()) {
        if (identifier->value.as_type->is_struct()) {
          static_cast<Structure *>(identifier->value.as_type)
              ->set_name(identifier->token);
        } else if (identifier->value.as_type->is_parametric_struct()) {
          static_cast<ParametricStructure *>(identifier->value.as_type)
              ->set_name(identifier->token);
        } else {
          assert(false);
        }

      } else if (type_expr->is_enum_literal()) {
        assert(identifier->value.as_type->is_enum());
        static_cast<Enumeration *>(identifier->value.as_type)->bound_name =
            identifier->token;
      }
    }
  } break;
  case DeclType::Std: {
    if (type_expr->type == Type_) {
      identifier->value = Context::Value(TypeVar(identifier));
    } else if (type_expr->type->is_type_variable()) {
      // TODO Should we just skip this?
    } else { /* There's nothing to do */
    }
  } break;
  case DeclType::Tick: {
    // Being asked to evaluate a tick, is just being asked to figure out what
    // type it must represent from the available information. There is very
    // little information here, since it's a generic function, so we simply bind
    // a type variable and return it.
    identifier->value = Context::Value(TypeVar(identifier, type_expr));
    return identifier->value;
  }
  }

  return nullptr;
}

Context::Value EnumLiteral::evaluate(Ctx& ctx) { return value; }

Context::Value Access::evaluate(Ctx& ctx) {
  if (type->is_enum()) {
    auto enum_type = (Enumeration *)type;
    return Context::Value(enum_type->get_index(member_name));
  }

  if (member_name == "bytes") {
    return Context::Value(operand->evaluate(ctx).as_type->bytes());

  } else if (member_name == "alignment") {
    return Context::Value(operand->evaluate(ctx).as_type->alignment());
  }

  assert(false && "not yet implemented");
}

Context::Value Binop::evaluate(Ctx& ctx) {
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

      std::vector<Context::Value> vals;

      // Populate the function context with arguments
      for (size_t i = 0; i < arg_vals.size(); ++i) {
        auto rhs_eval = arg_vals[i]->evaluate(ctx);
        vals.push_back(rhs_eval);
      }

      for (size_t i = 0; i < arg_vals.size(); ++i) {
        // TODO do we need to clean this up? I think not. It should just be
        // overwritten next time the function is called, right?
        fn_ptr->inputs[i]->identifier->value = vals[i];
      }

      fn_ptr->fn_scope->ClearCTRV();
      return fn_ptr->evaluate(ctx);

    } else if (lhs->type == Type_) {
      auto lhs_evaled = lhs->evaluate(ctx).as_type;

      assert(lhs_evaled->is_parametric_struct());

      auto struct_lit =
          static_cast<ParametricStructure *>(lhs_evaled)->ast_expression;

      assert(struct_lit->value.as_type);
      assert(struct_lit->value.as_type->is_parametric_struct());

      if (debug::parametric_struct) {
        assert(struct_lit->value.as_type);
        assert(struct_lit->value.as_type->is_parametric_struct());
        std::cerr << "\n== Evaluating a parametric struct call ==\n"
                  << static_cast<ParametricStructure *>(
                         struct_lit->value.as_type)
                         ->bound_name
                  << std::endl;
      }

      Ctx param_struct_args;

      std::vector<Context::Value> arg_vals;
      int arg_val_counter = 0;

      if (debug::parametric_struct) {
        std::cerr << " * Argument values:" << std::endl;
      }

      if (rhs->is_comma_list()) {
        // Note: The right number of elements are here because we've already
        // verified this.
        const auto &elems = static_cast<ChainOp*>(rhs)->exprs;
        for (size_t i = 0; i < elems.size(); ++i) {
          auto evaled_elem                             = elems[i]->evaluate(ctx);
          param_struct_args[struct_lit->params.ids[i]] = evaled_elem;
          if (debug::parametric_struct) {
            std::cerr << "   " << arg_val_counter++ << ". "
                      << *evaled_elem.as_type << std::endl;
          }
        }
      } else {
        auto evaled_rhs = rhs->evaluate(ctx);
        param_struct_args[struct_lit->params.ids[0]] = rhs->evaluate(ctx);
        arg_vals.push_back(evaled_rhs);
        if (debug::parametric_struct) {
          std::cerr << "   " << arg_val_counter++ << ". " << *evaled_rhs.as_type
                    << std::endl;
        }
      }

      if (debug::parametric_struct) { std::cerr << std::endl; }

      return value = struct_lit->CreateOrGetCached(param_struct_args);

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

Context::Value Statements::evaluate(Ctx& ctx) {
  for (auto &stmt : statements) {
    stmt->evaluate(ctx);
    if (scope_->HasCTRV()) {
      return scope_->GetCTRV();
    }
  }

  return nullptr;
}

Context::Value Conditional::evaluate(Ctx& ctx) {
  for (size_t i = 0; i < conditions.size(); ++i) {
    if (conditions[i]->evaluate(ctx).as_bool) {
      statements[i]->evaluate(ctx);
      if (scope_->HasCTRV()) { return scope_->GetCTRV(); }
    }
  }

  if (has_else()) {
    statements.back()->evaluate(ctx);
    if (scope_->HasCTRV()) { return scope_->GetCTRV(); }
  }

  return nullptr;
}

Context::Value Jump::evaluate(Ctx& ctx) { assert(false && "Not yet implemented"); }
Context::Value While::evaluate(Ctx& ctx) { assert(false && "Not yet implemented"); }
Context::Value For::evaluate(Ctx& ctx) { assert(false && "Not yet implemented"); }
} // namespace AST
