#ifndef ICARUS_UNITY
#include "Scope.h"
#include "Type/Type.h"
#endif

#include "IR/IR.h"
#include "IR/Stack.h"

extern AST::FunctionLiteral *WrapExprIntoFunction(AST::Expression *expr);
extern std::stack<Scope *> ScopeStack;

namespace TypeSystem {
void initialize();
extern Type *get(const std::string &name);
} // namespace TypeSystem

IR::Value Evaluate(AST::Expression *expr) {
  auto old_func  = IR::Func::Current;
  auto old_block = IR::Block::Current;

  auto fn_ptr      = WrapExprIntoFunction(expr);
  auto local_stack = new IR::LocalStack;
  IR::Func *func   = fn_ptr->EmitAnonymousIR().as_func;

  func->SetName("anonymous-func");

  expr->verify_types();
  IR::Func::Current  = old_func;
  IR::Block::Current = old_block;

  auto result = func->Call(local_stack, {});
  delete local_stack;

  if (fn_ptr->type == Func(Void, Void)) {
    fn_ptr->statements->statements[0] = nullptr;
  } else {
    auto ret = fn_ptr->statements->statements.front();
    assert(ret->is_unop() && ((AST::Unop *)ret)->op == Language::Operator::Return);
    ((AST::Unop *)ret)->operand = nullptr;
  }
  delete fn_ptr;

  return result;
}

namespace AST {
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

Context::Value Terminal::evaluate() { return value; }

#define NO_LONGER_NEEDED(node)                                                 \
  Context::Value node::evaluate() { UNREACHABLE; }
NO_LONGER_NEEDED(ArrayLiteral)
NO_LONGER_NEEDED(FunctionLiteral)
#undef NO_LONGER_NEEDED

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
  return value;
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

  NOT_YET;
}

Context::Value Binop::evaluate() {
  using Language::Operator;
  if (op == Operator::Call) {
    if (lhs->type->is_function()) {
      auto result = Evaluate(this);

      if (result.flag == IR::ValType::B) {
        value_flag   = ValueFlag::Done;
        return value = Context::Value(result.as_bool);

      } else if (result.flag == IR::ValType::C) {
        value_flag   = ValueFlag::Done;
        return value = Context::Value(result.as_char);

      } else if (result.flag == IR::ValType::I) {
        value_flag   = ValueFlag::Done;
        return value = Context::Value((long)result.as_int);

      } else if (result.flag == IR::ValType::R) {
        value_flag   = ValueFlag::Done;
        return value = Context::Value(result.as_real);

      } else if (result.flag == IR::ValType::U) {
        value_flag   = ValueFlag::Done;
        return value = Context::Value(result.as_uint);

      } else if (result.flag == IR::ValType::T) {
        value_flag   = ValueFlag::Done;
        return value = Context::Value(result.as_type);
      }
      NOT_YET;

    } else if (lhs->type == Type_) {
      auto lhs_evaled = lhs->evaluate().as_type;
      assert(lhs_evaled->is_parametric_struct());
      value      = Context::Value(Evaluate(this).as_type);
      value_flag = ValueFlag::Done;
      return value;
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
} // namespace AST
