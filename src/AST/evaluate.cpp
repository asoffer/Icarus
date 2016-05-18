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

static void AppendValueToStream(Type *type, Context::Value val,
                                std::stringstream &ss) {
  if (type == Bool) {
    ss << (val.as_bool ? "true" : "false");
  } else if (type == Char) {
    ss << val.as_char;

  } else if (type == Int) {
    ss << val.as_int;

  } else if (type == Real) {
    ss << val.as_real;

  } else if (type == Uint) {
    ss << val.as_uint;

  } else if (type == Type_) {
    ss << val.as_type->to_string();
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

Context::Value Identifier::evaluate() {
  // What about when it's, e.g., an int with the value 0?
  if (type == Type_ && !value.as_type) {
    assert(decls.size() == 1);
    decls[0]->evaluate();
  }

  return value;
}

Context::Value DummyTypeExpr::evaluate() { return value; }

Context::Value Unop::evaluate() {
  operand->verify_types();

  if (op == Language::Operator::Return) {
    scope_->SetCTRV(operand->evaluate());

    return value = nullptr;

  } else if (op == Language::Operator::Print) {
    // TODO Don't print. Raise an error.
    auto val = operand->evaluate();
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
      return Context::Value(-operand->evaluate().as_int);

    } else if (type == Real) {
      return Context::Value(-operand->evaluate().as_real);
    }
  } else if (op == Language::Operator::And) {
    if (operand->type != Type_) {
      // TODO better error message
      error_log.log(loc, "Taking the address of a " +
                             operand->type->to_string() +
                             " is not allowed at compile-time");
    }

    return value = Context::Value(Ptr(operand->evaluate().as_type));
  }

  assert(false && "Unop eval: I don't know what to do.");
}

Context::Value ChainOp::evaluate() {
  using Language::Operator;
  auto expr_type = exprs[0]->type;
  if (expr_type == Bool) {
    switch (ops[0]) {
    case Operator::Xor: {
      bool expr_val = false;
      for (auto &expr : exprs) {
        expr_val = (expr_val != expr->evaluate().as_bool);
      }
      return value = Context::Value(expr_val);
    }
    case Operator::And:
      for (auto &expr : exprs) {
        if (expr->evaluate().as_bool) return value = Context::Value(false);
      }
      return value = Context::Value(true);
    case Operator::Or:
      for (auto &expr : exprs) {
        if (expr->evaluate().as_bool) { return value = Context::Value(true); }
      }
      return value = Context::Value(false);
    default: assert(false && "Invalid chainop for bool in evaluate()");
    }

    return value = Context::Value(true);

  } else if (expr_type == Int) {
    bool total = true;
    auto last = exprs[0]->evaluate();
    for (size_t i = 0; i < ops.size(); ++i) {
      auto next = exprs[i + 1]->evaluate();

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
        types.push_back(exprs[i]->evaluate().as_type);
      }
      return Context::Value(Tup(types));
    }

    bool total = true;
    auto last = exprs[0]->evaluate();
    for (size_t i = 0; i < ops.size(); ++i) {
      auto next = exprs[i + 1]->evaluate();

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
    auto last = exprs[0]->evaluate();
    for (size_t i = 0; i < ops.size(); ++i) {
      auto next = exprs[i + 1]->evaluate();

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
    auto last = exprs[0]->evaluate();
    for (size_t i = 0; i < ops.size(); ++i) {
      auto next = exprs[i + 1]->evaluate();

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
    auto last = exprs[0]->evaluate();
    for (size_t i = 0; i < ops.size(); ++i) {
      auto next = exprs[i + 1]->evaluate();

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

Context::Value ArrayType::evaluate() {
  assert(length);
  determine_time();
  if ((length->time() == Time::either || length->time() == Time::compile) &&
      !length->is_hole()) {
    auto data_type_eval = data_type->evaluate().as_type;
    auto length_eval    = length->evaluate().as_uint;

    return value = Context::Value(Arr(data_type_eval, length_eval));
  }

  return value = Context::Value(Arr(data_type->evaluate().as_type));
}

Context::Value ArrayLiteral::evaluate() { assert(false); }

Context::Value Terminal::evaluate() {
  if (type == Bool) {
    assert((token == "true" || token == "false") &&
           "Bool literal other than true or false");
    return Context::Value(terminal_type == Language::Terminal::True);
  } else if (type == Char) {
    return Context::Value(token[0]);
  } else if (type == Int) {
    return Context::Value(std::stol(token));
  } else if (type == Real) {
    return Context::Value(std::stod(token));
  } else if (type == Uint) {
    return Context::Value(std::stoul(token));
  } else if (type == Type_) {
    if (token == "bool") return Context::Value(Bool);
    if (token == "char") return Context::Value(Char);
    if (token == "int") return Context::Value(Int);
    if (token == "real") return Context::Value(Real);
    if (token == "type") return Context::Value(Type_);
    if (token == "uint") return Context::Value(Uint);
    if (token == "void") return Context::Value(Void);

    error_log.log(loc, "I don't think `" + token + "` is a type!");
    return Context::Value(Error);
  } else { /* TODO */
  }
  return nullptr;
}

Context::Value FunctionLiteral::evaluate() { return statements->evaluate(); }

Context::Value Case::evaluate() {
  for (auto kv : key_vals) {
    if (kv.first->evaluate().as_bool) { return kv.second->evaluate(); }
  }
  // Must have an else-clause, so this is unreachable.
  assert(false);
}

Context::Value ParametricStructLiteral::evaluate() { return value; }
Context::Value StructLiteral::evaluate() { return value; }
Context::Value InDecl::evaluate() { return nullptr; }

Context::Value Declaration::evaluate() {
  switch (decl_type) {
  case DeclType::Infer: {
    if (expr->type->is_function()) {
      identifier->value = Context::Value(expr);
    } else {
      identifier->value = expr->evaluate();

      if (expr->is_struct_literal()) {
        if (identifier->value.as_type->is_struct()) {
          static_cast<Structure *>(identifier->value.as_type)
              ->set_name(identifier->token);
        } else if (identifier->value.as_type->is_parametric_struct()) {
          static_cast<ParametricStructure *>(identifier->value.as_type)
              ->set_name(identifier->token);
        } else {
          assert(false);
        }

      } else if (expr->is_enum_literal()) {
        assert(identifier->value.as_type->is_enum());
        static_cast<Enumeration *>(identifier->value.as_type)->bound_name =
            identifier->token;
      }
    }
  } break;
  case DeclType::Std: {
    if (expr->type == Type_) {
      identifier->value = Context::Value(TypeVar(identifier));
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
    identifier->value = Context::Value(TypeVar(identifier, expr));
    return identifier->value;
  }
  }

  return nullptr;
}

Context::Value EnumLiteral::evaluate() { return value; }

Context::Value Access::evaluate() {
  if (type->is_enum()) {
    auto enum_type = (Enumeration *)type;
    return Context::Value(enum_type->get_index(member_name));
  }

  if (member_name == "bytes") {
    return Context::Value(operand->evaluate().as_type->bytes());

  } else if (member_name == "alignment") {
    return Context::Value(operand->evaluate().as_type->alignment());
  }

  std::cout << *this << std::endl;
  assert(false && "not yet implemented");
}

Context::Value Binop::evaluate() {
  using Language::Operator;
  if (op == Operator::Call) {
    if (lhs->type->is_function()) {
      auto lhs_val = lhs->evaluate().as_expr;
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
        auto rhs_eval = arg_vals[i]->evaluate();
        vals.push_back(rhs_eval);
      }

      for (size_t i = 0; i < arg_vals.size(); ++i) {
        // TODO do we need to clean this up? I think not. It should just be
        // overwritten next time the function is called, right?
        fn_ptr->inputs[i]->identifier->value = vals[i];
      }

      fn_ptr->fn_scope->ClearCTRV();
      return fn_ptr->evaluate();

    } else if (lhs->type == Type_) {
      auto lhs_evaled = lhs->evaluate().as_type;
      assert(lhs_evaled->is_parametric_struct());

      auto param_struct = static_cast<ParametricStructure *>(lhs_evaled);
      auto struct_lit   = param_struct->ast_expression;

      if (debug::parametric_struct) {
        assert(struct_lit->value.as_type);
        assert(struct_lit->value.as_type->is_parametric_struct());
        std::cout << "\n== Evaluating a parametric struct call ==\n"
                  << static_cast<ParametricStructure *>(struct_lit->value.as_type)
                         ->bound_name
                  << std::endl;
      }
      std::vector<Context::Value> arg_vals;
      int arg_val_counter = 0;

      if (debug::parametric_struct) {
        std::cout << " * Argument values:" << std::endl;
      }

      if (rhs->is_comma_list()) {
        for (auto elem : static_cast<ChainOp *>(rhs)->exprs) {
          auto evaled_elem = elem->evaluate();
          assert(!evaled_elem.as_type->has_vars);
          arg_vals.push_back(evaled_elem);
          if (debug::parametric_struct) {
            std::cout << "   " << arg_val_counter++ << ". "
                      << *evaled_elem.as_type << std::endl;
          }
        }
      } else {
        auto evaled_rhs = rhs->evaluate();
        assert(!evaled_rhs.as_type->has_vars);

        arg_vals.push_back(evaled_rhs);
        if (debug::parametric_struct) {
          std::cout << "   " << arg_val_counter++ << ". " << *evaled_rhs.as_type
                    << std::endl;
        }
      }

      auto num_args = arg_vals.size();

      if (debug::parametric_struct) { std::cout << std::endl; }

      // look through the cache
      // TODO there's definitely a better way to do this.

      size_t cache_num = 0;
      for (const auto &cached_val : struct_lit->cache) {
        if (debug::parametric_struct) {
          std::cout << " * Checking match against cache position " << cache_num++
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
          if (arg_vals[i].as_type != cached_val.first[i]) {

            if (debug::parametric_struct) {
              std::cout << "   - Failed matching argument " << i << std::endl;
            }

            goto outer_continue;
          }
        }

        if (debug::parametric_struct) {
          std::cout << "   - Found a match." << std::endl;
          std::cout << *(cached_val.first[0]) << std::endl;
        }
        // If you get down here, you have found the right thing.
        return value = cached_val.second->value;

      outer_continue:;
      }

      // TODO there's definitely a way to consolidate/speed up this stuff.
      // Create the key earlier and do a binary rather than linear search
      // through the cache using this key.
      std::vector<Type *> vec_key;
      for (size_t i = 0; i < num_args; ++i) {
        vec_key.push_back(arg_vals[i].as_type);
      }
        
      auto &cache_loc = (struct_lit->cache[vec_key] = new StructLiteral);

      // TODO do we need to clean this up? I think not. It should just be
      // overwritten next time the generic struct is called, right?
      for (size_t i = 0; i < num_args; ++i) {
        struct_lit->params[i]->identifier->value = arg_vals[i];
      }

      std::stringstream ss;
      // TODO if the parameter is not a type?
      ss << param_struct->bound_name << "(";

      // auto param_type = struct_lit->params[0]->identifier->type;

      // TODO you haven't done type verification of the fields yet, so using
      // param_type above will likely return 0x0.
      AppendValueToStream(Type_, arg_vals[0], ss);

      for (size_t i = 1; i < num_args; ++i) {
        auto parameter_type = struct_lit->params[i]->expr->value.as_type;
        ss << ", ";
        AppendValueToStream(parameter_type, arg_vals[i], ss);
      }
      ss << ")";

      cache_loc->value = Context::Value(Struct(ss.str(), cache_loc));

      if (debug::parametric_struct) {
        std::cout << " * No match found.\n"
                     " * Creating new cached value.\n"
                     " * Cache size is now "
                  << struct_lit->cache.size() << " for " << struct_lit << "."
                  << std::endl;
        // For debugging so we don't get too far generating these things.
        assert(struct_lit->cache.size() < 5);
      }

      auto cloned_struct =
          param_struct->ast_expression->CloneStructLiteral(cache_loc);

      cloned_struct->verify_types();
      static_cast<Structure *>(cloned_struct->value.as_type)->set_name(ss.str());

      return value = cloned_struct->value;
    } else {
      assert(false);
    }

  } else if (op == Operator::Arrow) {
    auto lhs_type = lhs->evaluate().as_type;
    auto rhs_type = rhs->evaluate().as_type;
    return Context::Value(Func(lhs_type, rhs_type));
  }

  return nullptr;
}

Context::Value Statements::evaluate() {
  for (auto &stmt : statements) {
    stmt->evaluate();
    if (scope_->HasCTRV()) {
      return scope_->GetCTRV();
    }
  }

  return nullptr;
}

Context::Value Conditional::evaluate() {
  for (size_t i = 0; i < conditions.size(); ++i) {
    if (conditions[i]->evaluate().as_bool) {
      statements[i]->evaluate();
      if (scope_->HasCTRV()) { return scope_->GetCTRV(); }
    }
  }

  if (has_else()) {
    statements.back()->evaluate();
    if (scope_->HasCTRV()) { return scope_->GetCTRV(); }
  }

  return nullptr;
}

Context::Value Jump::evaluate() { assert(false && "Not yet implemented"); }
Context::Value While::evaluate() { assert(false && "Not yet implemented"); }
Context::Value For::evaluate() { assert(false && "Not yet implemented"); }
} // namespace AST
