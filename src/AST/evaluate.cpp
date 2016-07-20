#ifndef ICARUS_UNITY
#include "Scope.h"
#include "Type/Type.h"
#endif

#include "IR/IR.h"
#include "IR/Stack.h"

extern std::stack<Scope *> ScopeStack;

namespace TypeSystem {
void initialize();
extern Type *get(const std::string &name);
} // namespace TypeSystem

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

  bool has_vars = false;
  for (const auto &arg : arg_vals) {
    // TODO What if the argument isn't a type
    has_vars |= arg.second.as_type->has_vars;
  }

  std::stringstream ss;
  ss << param_struct->bound_name << "(";

  Type *parameter_type;
  if (params[0]->type_expr) {
    parameter_type = params[0]->type_expr->evaluate().as_type;
  } else {
    assert(params[0]->init_val);
    params[0]->init_val->verify_types();
    parameter_type = params[0]->init_val->type;
  }

  AppendValueToStream(parameter_type, arg_vals.at(params[0]->identifier->token),
                      ss);

  for (size_t i = 1; i < num_args; ++i) {
    if (params[i]->type_expr) {
      parameter_type = params[i]->type_expr->evaluate().as_type;
    } else {
      assert(params[i]->init_val);
      params[i]->init_val->verify_types();
      parameter_type = params[i]->init_val->type;
    }

    ss << ", ";
    AppendValueToStream(parameter_type,
                        arg_vals.at(params[i]->identifier->token), ss);
  }
  ss << ")";

  if (debug::parametric_struct) {
    fprintf(stderr, " * No match found.\n"
                    " * Creating new cached value.\n"
                    " * Cache size is now %lu for %s.\n",
            cache.size(), to_string(0).c_str());
    // For debugging so we don't get too far generating these things.
    assert(cache.size() < 5);
  }

  std::vector<IR::Value> args;
  for (size_t i = 0; i < num_args; ++i) {
    // TODO what other things can be plugged in?
    auto iter = arg_vals.find(params[i]->identifier->token);
    assert(iter != arg_vals.end());
    args.push_back(IR::Value(iter->second.as_type));
  }
  auto local_stack = new IR::LocalStack;
  auto f = EmitIR();
  assert(f.flag == IR::ValType::F);
  auto result = f.as_func->Call(local_stack, args);
  assert(result.flag == IR::ValType::T);

  assert(result.as_type->is_struct());

  delete local_stack;

  auto struct_result = ((Structure *)result.as_type);
  struct_result->set_name(ss.str());

  auto &cache_loc = (cache[arg_vals] = struct_result->ast_expression);
  reverse_cache[cache_loc] = arg_vals;
  cache_loc->value         = Context::Value(struct_result);

  ScopeStack.push(scope_);
  cache_loc->assign_scope();
  ScopeStack.pop();

  cache_loc->verify_types();

  return cache_loc->value;
}

llvm::Value *Expression::llvm_value(Context::Value v) {
  assert(type != Type_ && "Type_ conversion to llvm::Value*");
  assert(type != Err && "Error conversion to llvm::Value*");
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
  verify_types();
  value_flag = ValueFlag::In;

  if (type == Type_ && !value.as_type) { decl->evaluate(); }

  decl->evaluate();
  value      = decl->value;
  value_flag = ValueFlag::Done;
  return value;
}

Context::Value DummyTypeExpr::evaluate() { return value; }

Context::Value Unop::evaluate() {
  switch (op) {
  case Language::Operator::Sub:
    if (type == Int) {
      value_flag = ValueFlag::Done;
      return Context::Value(-operand->evaluate().as_int);

    } else if (type == Real) {
      value_flag = ValueFlag::Done;
      return Context::Value(-operand->evaluate().as_real);
    }
  case Language::Operator::And:
    if (op == Language::Operator::And) {
      operand->verify_types();
      if (operand->type != Type_) {
        // TODO better error message
        Error::Log::Log(loc, "Taking the address of a " +
                                 operand->type->to_string() +
                                 " is not allowed at compile-time");
      }

      value_flag   = ValueFlag::Done;
      return value = Context::Value(Ptr(operand->evaluate().as_type));
    }
  case Language::Operator::Eval: NOT_YET;

  /* These calls are illegal. */
  case Language::Operator::Return: UNREACHABLE;
  case Language::Operator::Print: UNREACHABLE;
  default: NOT_YET;
  }
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
  // TODO what if the length is given but isn't a compile-time value? e.g.,
  //
  //   [input(int); char] // a char-array of length given by user input
  auto data_type_eval = data_type->evaluate().as_type;
  if (length->is_hole()) {
    value = Context::Value(Arr(data_type_eval));
  } else {
    auto length_eval = length->evaluate().as_uint;
    value            = Context::Value(Arr(data_type_eval, length_eval));
  }

  value_flag = ValueFlag::Done;
  return value;
}

Context::Value ArrayLiteral::evaluate() { NOT_YET; }
Context::Value Terminal::evaluate() { return value; }

// Values determined when they are built.
Context::Value FunctionLiteral::evaluate() { return value; }
Context::Value ParametricStructLiteral::evaluate() { return value; }
Context::Value StructLiteral::evaluate() { return value; }
Context::Value EnumLiteral::evaluate() { return value; }

Context::Value Case::evaluate() {
  for (auto kv : key_vals) {
    if (kv.first->evaluate().as_bool) { return kv.second->evaluate(); }
  }
  // Must have an else-clause, so this is unreachable.
  assert(false);
}

Context::Value InDecl::evaluate() { return nullptr; }

Context::Value Generic::evaluate() {
  // Being asked to evaluate a tick, is just being asked to figure out what type
  // it must represent from the available information. There is very little
  // information here, since it's a generic function, so we simply bind a type
  // variable and return it.
  value = identifier->value = Context::Value(TypeVar(identifier, test_fn));
  return value;
}

Context::Value Declaration::evaluate() {
  if (IsInferred() || IsCustomInitialized()) {
    if (init_val->type->is_function()) {
      value = Context::Value(init_val);

    } else {
      value = init_val->evaluate();

      if (init_val->is_struct_literal()) {
        assert(identifier->value.as_type->is_struct());
        ((Structure *)(identifier->value.as_type))->set_name(identifier->token);

      } else if (init_val->is_parametric_struct_literal()) {
        assert(identifier->value.as_type->is_parametric_struct());
        ((ParametricStructure *)(identifier->value.as_type))
            ->set_name(identifier->token);

      } else if (init_val->is_enum_literal()) {
        assert(identifier->value.as_type->is_enum());
        ((Enumeration *)(identifier->value.as_type))->bound_name =
            identifier->token;
      }
    }

  } else if (IsUninitialized()) {
    NOT_YET;
  }

  value_flag = ValueFlag::Done;
  return nullptr;
}

Context::Value Access::evaluate() {
  if (type->is_enum()) {
    auto enum_type = (Enumeration *)type;
    value_flag = ValueFlag::Done;
    return Context::Value(enum_type->get_index(member_name));
  }

  if (member_name == "bytes") {
    value_flag = ValueFlag::Done;
    return Context::Value(operand->evaluate().as_type->bytes());

  } else if (member_name == "alignment") {
    value_flag = ValueFlag::Done;
    return Context::Value(operand->evaluate().as_type->alignment());
  }

  assert(false && "not yet implemented");
}

Context::Value Binop::evaluate() {
  using Language::Operator;
  if (op == Operator::Call) {
    if (lhs->type->is_function()) {
      auto lhs_val = lhs->evaluate().as_expr;
      assert(lhs_val && lhs_val->is_function_literal());
      auto fn_ptr = (FunctionLiteral *)lhs_val;


      std::vector<Expression *> arg_vals;
      if (rhs->is_comma_list()) {
        arg_vals = ((ChainOp *)rhs)->exprs;
      } else {
        arg_vals.push_back(rhs);
      }

      assert(arg_vals.size() == fn_ptr->inputs.size());

      std::vector<IR::Value> args;
      for (auto a : arg_vals) {
        // Doing value conversion
        if (a->type == Bool) {
          args.emplace_back(a->evaluate().as_bool);
        } else if (a->type == Char) {
          args.emplace_back(a->evaluate().as_char);
        } else if (a->type == Int) {
          args.emplace_back((long)a->evaluate().as_int);
        } else if (a->type == Real) {
          args.emplace_back(a->evaluate().as_real);
        } else if (a->type == Uint) {
          args.emplace_back(a->evaluate().as_uint);
        } else if (a->type == Type_) {
          args.emplace_back(a->evaluate().as_type);
        } else {
          NOT_YET;
        }
      }

      auto local_stack = new IR::LocalStack;
      IR::Func *func   = fn_ptr->EmitIR().as_func;
      auto result      = func->Call(local_stack, args);
      delete local_stack;

      // Doing value conversion
      if (result.flag == IR::ValType::B) {
        value_flag = ValueFlag::Done;
        return value = Context::Value(result.as_bool);

      } else if (result.flag == IR::ValType::C) {
        value_flag = ValueFlag::Done;
        return value = Context::Value(result.as_char);

      } else if (result.flag == IR::ValType::I) {
        value_flag = ValueFlag::Done;
        return value = Context::Value((long)result.as_int);

      } else if (result.flag == IR::ValType::R) {
        value_flag = ValueFlag::Done;
        return value = Context::Value(result.as_real);

      } else if (result.flag == IR::ValType::U) {
        value_flag = ValueFlag::Done;
        return value = Context::Value(result.as_uint);

      } else if (result.flag == IR::ValType::T) {
        value_flag = ValueFlag::Done;
        return value = Context::Value(result.as_type);
      }
      NOT_YET;

    } else if (lhs->type == Type_) {
      auto lhs_evaled = lhs->evaluate().as_type;

      assert(lhs_evaled->is_parametric_struct());

      auto struct_lit = ((ParametricStructure *)lhs_evaled)->ast_expression;

      assert(struct_lit->value.as_type);
      assert(struct_lit->value.as_type->is_parametric_struct());

      if (debug::parametric_struct) {
        assert(struct_lit->value.as_type);
        assert(struct_lit->value.as_type->is_parametric_struct());
        std::cerr << "\n== Evaluating a parametric struct call ==\n"
                  << ((ParametricStructure *)struct_lit->value.as_type)
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
        const auto &elems = ((ChainOp *)rhs)->exprs;
        for (size_t i = 0; i < elems.size(); ++i) {
          auto evaled_elem = elems[i]->evaluate();
          param_struct_args[struct_lit->params[i]->identifier->token] = evaled_elem;
          if (debug::parametric_struct) {
            std::cerr << "   " << arg_val_counter++ << ". "
                      << *evaled_elem.as_type << std::endl;
          }
        }
      } else {
        auto evaled_rhs = rhs->evaluate();
        param_struct_args[struct_lit->params[0]->identifier->token] =
            rhs->evaluate();
        arg_vals.push_back(evaled_rhs);
        if (debug::parametric_struct) {
          std::cerr << "   " << arg_val_counter++ << ". " << *evaled_rhs.as_type
                    << std::endl;
        }
      }

      if (debug::parametric_struct) { std::cerr << std::endl; }

      value_flag   = ValueFlag::Done;

      // std::cerr << *struct_lit << std::endl;
      return value = struct_lit->CreateOrGetCached(param_struct_args);

    } else {
      assert(false);
    }

  } else if (op == Operator::Arrow) {
    auto lhs_type = lhs->evaluate().as_type;
    auto rhs_type = rhs->evaluate().as_type;
    value_flag    = ValueFlag::Done;
    return value  = Context::Value(Func(lhs_type, rhs_type));
  }

  return nullptr;
}

Context::Value Statements::evaluate() {
  for (auto &stmt : statements) { stmt->evaluate(); }
  return nullptr;
}

Context::Value Conditional::evaluate() {
  for (size_t i = 0; i < conditions.size(); ++i) {
    if (conditions[i]->evaluate().as_bool) { statements[i]->evaluate(); }
  }

  if (has_else()) { statements.back()->evaluate(); }

  return nullptr;
}

Context::Value Jump::evaluate() { NOT_YET; }
Context::Value While::evaluate() { NOT_YET; }
Context::Value For::evaluate() { NOT_YET; }
} // namespace AST
