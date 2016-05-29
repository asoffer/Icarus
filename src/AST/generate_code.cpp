#ifndef ICARUS_UNITY
#include "Scope.h"
#include "Type.h"
#include <cstring>
#endif

extern llvm::BasicBlock *make_block(const std::string &name,
                                    llvm::Function *fn);

extern Type *GetFunctionTypeReferencedIn(Scope *scope,
                                         const std::string &fn_name,
                                         Type *input_type);

extern llvm::Value *PtrCallFix(Type *t, llvm::Value *ptr);

extern AST::FunctionLiteral *GetFunctionLiteral(AST::Expression *expr);

extern llvm::Module *global_module;
extern llvm::IRBuilder<> builder;

namespace cstdlib {
extern llvm::Constant *free();
extern llvm::Constant *memcpy();
extern llvm::Constant *malloc();
extern llvm::Constant *printf();
extern llvm::Constant *putchar();
} // namespace cstdlib

namespace builtin {
extern llvm::Function *ascii();
extern llvm::Function *ord();
} // namespace builtin

namespace data {
extern llvm::Constant *null_pointer(Type *t);
extern llvm::Constant *null(const Type *t);
extern llvm::ConstantInt *const_true();
extern llvm::ConstantInt *const_false();
extern llvm::ConstantInt *const_uint(size_t n);
extern llvm::ConstantInt *const_int(long n);
extern llvm::ConstantInt *const_char(char c);
extern llvm::ConstantFP *const_real(double d);
extern llvm::Value *global_string(const std::string &s);
} // namespace data

#define CONTINUE_FLAG data::const_char('\00')
#define RESTART_FLAG data::const_char('\01')
#define REPEAT_FLAG data::const_char('\02')
#define BREAK_FLAG data::const_char('\03')
#define RETURN_FLAG data::const_char('\04')

extern llvm::Value *GetFunctionReferencedIn(Scope *scope,
                                            const std::string &fn_name,
                                            Type *input_type);

static llvm::Value *FunctionComposition(const std::string &name,
                                        llvm::Value *lhs, llvm::Value *rhs,
                                        Function *fn_type) {
  auto old_block = builder.GetInsertBlock();

  llvm::FunctionType *llvm_fn_type = *fn_type;
  auto llvm_fn = static_cast<llvm::Function *>(
      global_module->getOrInsertFunction(name, llvm_fn_type));

  auto entry = make_block("entry", llvm_fn);
  builder.SetInsertPoint(entry);

  // TODO multiple args, multiple return values, non-primitives, void return
  auto arg = llvm_fn->args().begin();
  builder.CreateRet(builder.CreateCall(lhs, {builder.CreateCall(rhs, {arg})}));

  builder.SetInsertPoint(old_block);
  return llvm_fn;
}

namespace AST {
llvm::Value *Identifier::generate_code() {
  if (type == Type_) {
    return nullptr;

  } else if (type->is_function()) {
    // TODO better way to get functions based on their name
    auto fn_type      = (Function *)type;
    auto llvm_fn_type = (llvm::FunctionType *)fn_type->llvm_type;
    auto mangled_name = Mangle(fn_type, this);

    return global_module->getOrInsertFunction(mangled_name, llvm_fn_type);

  } else if (type->is_big()) {
    return alloc;

  } else {
    return builder.CreateLoad(alloc, token);
  }
}

// Invariant:
// Only returns nullptr if the expression type is void or a type
llvm::Value *Terminal::generate_code() {
  // Else is a terminal only in case statements. In this situation, it's
  // corresponding resulting value is always to be the one chosen, so we should
  // have 'else' represent the value true.
  if (terminal_type == Language::Terminal::Else) { return data::const_true(); }

  if (terminal_type == Language::Terminal::StringLiteral) {
    auto str = data::global_string(value.as_str);
    auto len = data::const_uint(std::strlen(value.as_str));

    auto str_alloc = builder.CreateAlloca(*type);

    auto len_ptr = builder.CreateGEP(
        str_alloc, {data::const_uint(0),
                    static_cast<Structure *>(String)->field_num("_length")},
        "len_ptr");
    builder.CreateStore(len, len_ptr);

    auto char_array_ptr = builder.CreateGEP(
        str_alloc, {data::const_uint(0),
                    static_cast<Structure *>(String)->field_num("_chars")},
        "char_array_ptr");

    // NOTE: no need to uninitialize because we never initialized it.
    Arr(Char)->initialize_literal(char_array_ptr, len);

    auto char_data_ptr = builder.CreateLoad(
        builder.CreateGEP(char_array_ptr,
                          {data::const_uint(0), data::const_uint(1)}),
        "char_data_ptr");

    builder.CreateCall(cstdlib::memcpy(), {char_data_ptr, str, len});

    return str_alloc;
  }

  return GetGlobal();
}

static void CallPrint(Expression *expr) {
  if (expr->type == Type_) {
    Ctx ctx;
    builder.CreateCall(
        cstdlib::printf(),
        {data::global_string("%s"),
         data::global_string(expr->evaluate(ctx).as_type->to_string())});
    return;
  }

  llvm::Value *val = expr->generate_code();

  // NOTE: this is complicated because if the function is quantum, we cannot
  // just generate the code from Identifier::generate_code. Knowing which
  // quanta to pick requires contextual information.
  //
  // TODO We should log that information so we don't repeat this process.
  if (expr->type->is_struct()) {
    // TODO ensure that it is generated
    auto print_fn = GetFunctionReferencedIn(expr->scope_, "__print__", expr->type);
    assert(print_fn && "No print function available");
    builder.CreateCall(print_fn, val);

  } else if (expr->type == Char) {
    builder.CreateCall(cstdlib::putchar(), {val});

  } else if (expr->type == Uint) {
    builder.CreateCall(cstdlib::printf(), {data::global_string("%u"), val});

  } else {
    expr->type->call_repr(val);
  }
}

// Invariant:
// Only returns nullptr if the expression type is void or a type
llvm::Value *Unop::generate_code() {
  // We first go through all the possible operators where we don't necessarily
  // need to generate code for the operand.
  switch (op) {
  case Language::Operator::Import: return nullptr;
  case Language::Operator::And: {
    return operand->generate_lvalue();
  }
  case Language::Operator::Print: {
    if (operand->is_comma_list()) {
      for (auto expr: static_cast<ChainOp *>(operand)->exprs) {
        CallPrint(expr);
      }
    } else {
      CallPrint(operand);
    }
    return nullptr;
  }
  default:;
  }

  llvm::Value *val = operand->generate_code();
  switch (op) {
  case Language::Operator::Not: {
    if (operand->type == Bool) {
      return builder.CreateNot(val);
    } else {
      auto not_fn = GetFunctionReferencedIn(scope_, "__not__", operand->type);
      assert(not_fn && "No 'not' function available");
      builder.CreateCall(not_fn, val);
    }
  }
  case Language::Operator::Sub: {
    if (operand->type == Int) {
      return builder.CreateNeg(val, "neg");

    } else if (operand->type == Real) {
      return builder.CreateFNeg(val, "fneg");

    } else if (operand->type->is_struct()) {
      auto neg_fn = GetFunctionReferencedIn(scope_, "__neg__", operand->type);
      assert(neg_fn && "No negation function available");
      // TODO put this alloc at the beginning!
      // TODO what if this returns a primitive, actually return it. otherwise,
      // use a return parameter like you're doing already
      assert(scope_->is_block_scope());
      auto local_ret = ((BlockScope *)scope_)->CreateLocalReturn(type);
      builder.CreateCall(neg_fn, {val, local_ret});
      return local_ret;

    } else {
      assert(false);
    }
  }
  case Language::Operator::Free: {
    builder.CreateCall(cstdlib::free(), {builder.CreateBitCast(val, *RawPtr)});
    if (operand->lvalue) {
      builder.CreateStore(data::null(operand->type),
                          operand->generate_lvalue());
    }

    return nullptr;
  }
  case Language::Operator::Return: {
    assert(scope_->is_block_scope());
    static_cast<BlockScope *>(scope_)->make_return(val);
    return nullptr;
  }
  case Language::Operator::At: {
    return type->is_big() ? val : builder.CreateLoad(val);
  }
  default: assert(false && "Unimplemented unary operator codegen");
  }
}

llvm::Value *Access::generate_code() {
  if (operand->type == Type_) {
    // As of 4/18/16, the only ways an operand can be a type are:
    // 1. TYPE.bytes (returning the number of bytes taken up by the type)
    // 2. TYPE.alignment (returning the alignment of the type)
    // 3. An enum, as in `Color.Red`. Here Color is an enum with the access
    //    parameter Red.
    //
    // NOTE: this will likely change if we implement UFCS. In that case, this
    // whole method will probably be out of date.

    Ctx ctx;
    auto expr_as_type = operand->evaluate(ctx).as_type;

    // TODO in the process of moving this into evaluate (because it's
    // compile-time) Remove it from here.
    if (member_name == "bytes") {
      return data::const_uint(expr_as_type->bytes());

    } else if (member_name == "alignment") {
      return data::const_uint(expr_as_type->alignment());
    }

    assert(expr_as_type->is_enum() && "Expression should be an enum");
    return static_cast<Enumeration *>(expr_as_type)->get_value(member_name);

  } else if (operand->type->is_array() && member_name == "size" &&
             static_cast<Array *>(operand->type)->fixed_length) {
    // Fixed length arrays shouldn't bother to do any real code-gen
    return data::const_uint(static_cast<Array *>(operand->type)->len);
  }

  // Generate the code for the operand
  auto eval = operand->generate_code();

  // To make access pass through all layers of pointers, we loop through
  // loading values while we're looking at pointers.
  auto base_type = operand->type;
  while (base_type->is_pointer()) {
    base_type = static_cast<Pointer *>(base_type)->pointee;
    if (!base_type->is_big()) eval = builder.CreateLoad(eval);
  }

  if (base_type->is_array() && member_name == "size") {
    return builder.CreateLoad(
        builder.CreateGEP(eval, {data::const_uint(0), data::const_uint(0)}));
  }

  if (base_type->is_struct()) {
    auto struct_type = static_cast<Structure *>(base_type);

    if (!type->stores_data()) { assert(false && "Not yet implemented"); }

    auto elem_ptr = builder.CreateGEP(
        eval, {data::const_uint(0), struct_type->field_num(member_name)});
    return type->is_big() ? elem_ptr : builder.CreateLoad(elem_ptr);

  } else {
    assert(false && "Not yet implemented");
  }
}

// This function exists because both '=' and ':=' need to call some version of
// the same code. it's been factored out here.
static llvm::Value *generate_assignment_code(Expression *lhs, Expression *rhs) {
  llvm::Value *var = nullptr;
  llvm::Value *val = nullptr;

  // Treat functions special
  if (lhs->is_identifier() && rhs->type->is_function()) {
    auto id           = static_cast<Identifier *>(lhs);
    auto fn_type      = (Function *)rhs->type;
    auto llvm_fn_type = (llvm::FunctionType *)fn_type->llvm_type;
    auto mangled_name = Mangle(fn_type, lhs);

    // Then If it is a function literal, notify the function literal of the code
    // name/scope, etc.
    if (rhs->is_function_literal()) {
      auto fn            = (FunctionLiteral *)rhs;
      fn->fn_scope->name = id->token;

      fn->llvm_fn = static_cast<llvm::Function *>(
          global_module->getOrInsertFunction(mangled_name, llvm_fn_type));

    } else if (rhs->is_binop() &&
               static_cast<Binop *>(rhs)->op == Language::Operator::Mul) {
      auto binop   = (Binop *)rhs;
      auto lhs_val = binop->lhs->generate_code();
      auto rhs_val = binop->rhs->generate_code();

      return FunctionComposition(mangled_name, lhs_val, rhs_val, fn_type);
    }

    val = rhs->generate_code();

    // Null value can be returned here, if for instance, the rhs is a function
    // on types.
    if (val) { val->setName(mangled_name); }

  } else {
    var = lhs->generate_lvalue();
    assert(var && "LHS of assignment generated null code");

    if (rhs->is_terminal() &&
        static_cast<Terminal *>(rhs)->terminal_type ==
            Language::Terminal::Hole) {
      return nullptr;
    }
    val = rhs->generate_code();
    assert(val && "RHS of assignment generated null code");

    Type::CallAssignment(lhs->scope_, lhs->type, rhs->type, var, val);
  }

  return nullptr;
}

static std::vector<llvm::Value *> CollateArgsForFunctionCall(Expression *arg) {
  if (arg->is_comma_list()) {
    auto &arg_exprs = static_cast<ChainOp *>(arg)->exprs;
    auto num_args   = arg_exprs.size();

    std::vector<llvm::Value *> arg_vals(num_args, nullptr);

    for (size_t i = 0; i < num_args; ++i) {
      arg_vals[i] = arg_exprs[i]->generate_code();
      assert(arg_vals[i] && "Argument value was null");
    }
    return arg_vals;
  }

  return {arg->generate_code()};
}

llvm::Value *Binop::generate_code() {
  if (time() == Time::compile) {
    Ctx ctx;
    return llvm_value(evaluate(ctx));
  }

  using Language::Operator;

  // The left-hand side may be a declaration
  if (op == Operator::Assign) {
    return generate_assignment_code(
        (lhs->is_declaration() ? static_cast<Declaration *>(lhs)->identifier
                               : lhs),
        rhs);
  }

  llvm::Value *lhs_val = nullptr;
  if (op == Operator::Call) {
    if (!rhs) {
      assert(lhs->type->is_function() && "Operand should be a function.");
      auto out_type = static_cast<Function *>(lhs->type)->output;
      lhs_val = lhs->generate_code();
      if (out_type->is_struct()) {
        // TODO move this outside of any potential loops
        assert(scope_->is_block_scope());
        auto local_ret =
            static_cast<BlockScope *>(scope_)->CreateLocalReturn(out_type);

        builder.CreateCall((llvm::Function *)lhs_val, local_ret);
        return local_ret;

      } else {
        return builder.CreateCall((llvm::Function *)lhs_val);
      }
    }

    if (lhs->type->has_vars) {
      auto fn_lit = GetFunctionLiteral(lhs);
      assert(!fn_lit->cache.empty());

      for (auto kv : fn_lit->cache) {
        if (rhs->type == kv.first) {
          lhs_val = kv.second->generate_code();
          break;
        }
      }

    } else if (lhs->is_identifier()) {
      lhs_val = GetFunctionReferencedIn(
          scope_, static_cast<Identifier *>(lhs)->token, rhs->type);

    } else if (lhs->is_function_literal()) {
      lhs_val = lhs->generate_code();

    } else {
      lhs_val = lhs->generate_code();
    }
  } else {
    lhs_val = lhs->generate_code();
  }

  assert(lhs_val);

// This code block tells us what to do of &= and |= operators so that they
// short-circuit.
#define SHORT_CIRCUITING_OPERATOR(goto_on_true, goto_on_false)                 \
  auto lval = lhs->generate_lvalue();                                          \
  assert(lval && "LHS lval of assignment generated null code");                \
                                                                               \
  auto parent_fn   = builder.GetInsertBlock()->getParent();                    \
  auto more_block  = make_block("more", parent_fn);                            \
  auto merge_block = make_block("merge", parent_fn);                           \
                                                                               \
  builder.CreateCondBr(lhs_val, goto_on_true, goto_on_false);                  \
  builder.SetInsertPoint(more_block);                                          \
  auto rhs_val = rhs->generate_code();                                         \
  assert(rhs_val && "RHS of assignment generated null code");                  \
                                                                               \
  builder.CreateStore(rhs_val, lval);                                          \
  builder.CreateBr(merge_block);                                               \
  builder.SetInsertPoint(merge_block);                                         \
  return nullptr;

  switch (op) {
  case Operator::AndEq: {
    SHORT_CIRCUITING_OPERATOR(more_block, merge_block);
  } break;
  case Operator::OrEq: {
    SHORT_CIRCUITING_OPERATOR(merge_block, more_block);
  } break;
#undef SHORT_CIRCUITING_OPERATOR
  case Operator::Cast: {
    Ctx ctx;
    Type *to_type = rhs->evaluate(ctx).as_type;
    if (lhs->type == Bool) {
      if (to_type == Int || to_type == Uint) {
        return builder.CreateZExt(lhs_val, *to_type, "ext.val");
      } else if (to_type == Real) {
        return builder.CreateUIToFP(lhs_val, *to_type, "ext.val");
      }
    } else if (lhs->type == Int) {
      if (to_type == Real) {
        return builder.CreateSIToFP(lhs_val, *to_type, "fp.val");
      } else if (to_type == Uint) {
        return lhs_val;
      }
    } else if (lhs->type == Uint) {
      if (to_type == Real) {
        return builder.CreateUIToFP(lhs_val, *to_type, "fp.val");
      } else if (to_type == Int) {
        return lhs_val;
      }
    } else if (lhs->type->is_pointer() && to_type->is_pointer()) {
      return builder.CreateBitCast(lhs_val, *to_type);
    }
    assert(false);
  } break;
  case Operator::Index: {
    assert(lhs->type->is_array() && "Type is not an array");
    auto array_type = (Array *)lhs->type;
    if (array_type->fixed_length) {
      return PtrCallFix(type, builder.CreateGEP(lhs_val, {data::const_uint(0),
                                                          rhs->generate_code()},
                                                "array_val"));
    } else {
      auto data_ptr = builder.CreateLoad(builder.CreateGEP(
          lhs_val, {data::const_uint(0), data::const_uint(1)}));
      return PtrCallFix(
          type, builder.CreateGEP(data_ptr, rhs->generate_code(), "array_val"));
    }
  } break;
  case Operator::Call: {
    if (lhs->type->is_function() || lhs->type->is_quantum()) {
      std::vector<llvm::Value *> arg_vals = CollateArgsForFunctionCall(rhs);

      if (type == Void) {
        builder.CreateCall(lhs_val, arg_vals);
        return nullptr;

      } else if (type->is_big() && !type->is_function()) {
        assert(scope_->is_block_scope());

        auto local_ret =
            static_cast<BlockScope *>(scope_)->CreateLocalReturn(type);
        arg_vals.push_back(local_ret);

        builder.CreateCall(lhs_val, arg_vals);
        return arg_vals.back();

      } else {
        return builder.CreateCall(lhs_val, arg_vals, "calltmp");
      }
    }
  }
  default:;
  }

  auto rhs_val = rhs->generate_code();
  assert(rhs_val && "RHS of assignment generated null code");

#define CREATE_CALL(fn_name)                                                   \
  {                                                                            \
    auto input_type = Tup({lhs->type, rhs->type});                             \
    auto fn_type =                                                             \
        GetFunctionTypeReferencedIn(scope_, "__" fn_name "__", input_type);    \
    auto fn = GetFunctionReferencedIn(scope_, "__" fn_name "__", input_type);  \
    assert(fn);                                                                \
    auto output_type = static_cast<Function *>(fn_type)->output;               \
                                                                               \
    if (output_type == Void) {                                                 \
      builder.CreateCall(fn, {lhs_val, rhs_val});                              \
      return nullptr;                                                          \
    } else if (output_type->is_big()) {                                        \
      assert(scope_->is_block_scope());                                        \
      auto local_ret =                                                         \
          static_cast<BlockScope *>(scope_)->CreateLocalReturn(output_type);   \
      builder.CreateCall(fn, {lhs_val, rhs_val, local_ret});                   \
      return local_ret;                                                        \
    } else {                                                                   \
      return builder.CreateCall(fn, {lhs_val, rhs_val});                       \
    }                                                                          \
  }

#define PRIMITIVE_CALL(prim_type, call_op, name)                               \
  if (lhs->type == prim_type && rhs->type == prim_type) {                      \
    return builder.Create##call_op(lhs_val, rhs_val, name);                    \
  }

#define PRIMITIVE_EQ_CALL(prim_type, call_op, name)                            \
  if (lhs->type == prim_type && rhs->type == prim_type) {                      \
    auto tmp = builder.Create##call_op(lhs_val, rhs_val, name ".tmp");         \
    builder.CreateStore(tmp, lval);                                            \
    return nullptr;                                                            \
  }

  switch (op) {
  case Operator::Add: {
    PRIMITIVE_CALL(Int, Add, "add")
    PRIMITIVE_CALL(Uint, Add, "add")
    PRIMITIVE_CALL(Real, FAdd, "fadd")
    CREATE_CALL("add")
  } break;
  case Operator::Sub: {
    PRIMITIVE_CALL(Int, Sub, "sub")
    PRIMITIVE_CALL(Uint, Sub, "sub")
    PRIMITIVE_CALL(Real, FSub, "fsub")
    CREATE_CALL("sub")
  } break;
  case Operator::Mul: {
    PRIMITIVE_CALL(Int, Mul, "mul")
    PRIMITIVE_CALL(Uint, Mul, "mul")
    PRIMITIVE_CALL(Real, FMul, "fmul")
    if (lhs->type->is_function() && rhs->type->is_function()) {
      auto output_type = Func(static_cast<Function *>(rhs->type)->input,
                              static_cast<Function *>(lhs->type)->output);
      return FunctionComposition("__anon_fn", lhs_val, rhs_val, output_type);
    }
    CREATE_CALL("mul")
  } break;
  case Operator::Div: {
    PRIMITIVE_CALL(Int, SDiv, "sdiv")
    PRIMITIVE_CALL(Uint, UDiv, "udiv")
    PRIMITIVE_CALL(Real, FDiv, "fdiv")
    CREATE_CALL("div")
  } break;
  case Operator::Mod: {
    PRIMITIVE_CALL(Int, SRem, "smod")
    PRIMITIVE_CALL(Uint, URem, "umod")
    PRIMITIVE_CALL(Real, FRem, "fmod")
    CREATE_CALL("mod")
  } break;
  case Operator::AddEq: {
    auto lval = lhs->generate_lvalue();
    assert(lval && "LHS lval of assignment generated null code");
    PRIMITIVE_EQ_CALL(Int, Add, "add")
    PRIMITIVE_EQ_CALL(Uint, Add, "add")
    PRIMITIVE_EQ_CALL(Real, FAdd, "fadd")
    CREATE_CALL("add_eq")
  } break;
  case Operator::SubEq: {
    auto lval = lhs->generate_lvalue();
    assert(lval && "LHS lval of assignment generated null code");
    PRIMITIVE_EQ_CALL(Int, Sub, "sub")
    PRIMITIVE_EQ_CALL(Uint, Sub, "sub")
    PRIMITIVE_EQ_CALL(Real, FSub, "fsub")
    CREATE_CALL("sub_eq")
  } break;
  case Operator::MulEq: {
    auto lval = lhs->generate_lvalue();
    assert(lval && "LHS lval of assignment generated null code");
    PRIMITIVE_EQ_CALL(Int, Mul, "mul")
    PRIMITIVE_EQ_CALL(Uint, Mul, "mul")
    PRIMITIVE_EQ_CALL(Real, FMul, "fmul")
    CREATE_CALL("mul_eq")
  } break;
  case Operator::DivEq: {
    auto lval = lhs->generate_lvalue();
    assert(lval && "LHS lval of assignment generated null code");
    PRIMITIVE_EQ_CALL(Int, SDiv, "sdiv")
    PRIMITIVE_EQ_CALL(Uint, UDiv, "udiv")
    PRIMITIVE_EQ_CALL(Real, FDiv, "fdiv")
    CREATE_CALL("div_eq")
  } break;
  case Operator::ModEq: {
    auto lval = lhs->generate_lvalue();
    assert(lval && "LHS lval of assignment generated null code");
    PRIMITIVE_EQ_CALL(Int, SRem, "smod")
    PRIMITIVE_EQ_CALL(Uint, URem, "umod")
    PRIMITIVE_EQ_CALL(Real, FRem, "fmod")
    CREATE_CALL("mod_eq")
  } break;
  case Operator::XorEq: {
    auto lval = lhs->generate_lvalue();
    assert(lval && "LHS lval of assignment generated null code");
    PRIMITIVE_EQ_CALL(Bool, Xor, "xor")
  } break;

  default:;
  }

  std::cerr << *this << std::endl;
  assert(false && "Reached end of Binop::generate_code");
}

llvm::Value *ArrayType::generate_code() {
  assert(false && "Not valid for code-gen");
}

llvm::Value *Statements::generate_code() {
  if (statements.empty()) { return nullptr; }

  auto it = statements.begin();
  (*it)->generate_code();
  ++it;
  while (it != statements.end()) {
    assert(builder.GetInsertBlock());
    auto term_inst = builder.GetInsertBlock()->getTerminator();
    if (term_inst) {
      // TODO Log a warning about unreachable code.
      break;
    }

    (*it)->generate_code();
    ++it;
  }

  return nullptr;
}

#define BEGIN_SHORT_CIRCUIT                                                    \
  builder.SetInsertPoint(curr_block);                                          \
  auto lhs_val = exprs[0]->generate_code();                                    \
  for (size_t i = 1; i < exprs.size(); ++i) {                                  \
    builder.SetInsertPoint(curr_block);                                        \
    auto rhs_val = exprs[i]->generate_code();                                  \
                                                                               \
    llvm::Value *cmp_val = nullptr;                                            \
    switch (ops[i - 1]) {

#define CASE(llvm_call, op_name)                                               \
  case Language::Operator::op_name: {                                          \
    cmp_val =                                                                  \
        builder.Create##llvm_call##op_name(lhs_val, rhs_val, #op_name "tmp");  \
  } break

#define END_SHORT_CIRCUIT                                                      \
  default: assert(false && "Invalid operator");                                \
    }                                                                          \
    assert(cmp_val && "cmp_val is nullptr");                                   \
                                                                               \
    auto next_block = make_block("next", parent_fn);                           \
    builder.CreateCondBr(cmp_val, next_block, landing);                        \
    phi->addIncoming(data::const_false(), curr_block);                         \
    curr_block = next_block;                                                   \
    lhs_val    = rhs_val;                                                      \
    }                                                                          \
                                                                               \
    builder.SetInsertPoint(curr_block);                                        \
    phi->addIncoming(data::const_true(), curr_block);                          \
    builder.CreateBr(landing);                                                 \
    builder.SetInsertPoint(landing);                                           \
    return phi;

llvm::Value *ChainOp::generate_code() {
  // TODO Should have already been done
  determine_time();

  // TODO eval of enums at compile-time is wrong. This could be
  // 1. That the eval function is wrong, or
  // 2. That they shouldn't be determined at compile-time
  if (time() == Time::compile) {
    Ctx ctx;
    return llvm_value(evaluate(ctx));
  }

  auto expr_type = exprs[0]->type;

  // Boolean values that cannot be short-circuited.
  if (expr_type == Bool) {
    auto num_exprs = exprs.size();
    if (ops.front() == Language::Operator::Xor) {
      llvm::Value *cmp_val = exprs[0]->generate_code();
      for (size_t i = 1; i < num_exprs; ++i) {
        cmp_val = builder.CreateXor(cmp_val, exprs[i]->generate_code());
      }
      return cmp_val;
    }
    if (Language::precedence(ops.front()) ==
        Language::precedence(Language::Operator::EQ)) {

      llvm::Value *cmp_val = exprs[0]->generate_code();
      for (size_t i = 0; i < num_exprs; ++i) {
        switch (ops[i]) {
        case Language::Operator::EQ: {
          cmp_val = builder.CreateAnd(cmp_val, exprs[i]->generate_code());
        } break;
        case Language::Operator::NE: {
          cmp_val = builder.CreateAnd(cmp_val, exprs[i]->generate_code());
        } break;
        default: assert(false && "Invalid chain-operation on bools");
        }
      }
    }
  }

  auto parent_fn  = builder.GetInsertBlock()->getParent();
  auto landing    = make_block("land", parent_fn);
  auto curr_block = builder.GetInsertBlock();

  // Count the number of incoming branches into the phi node. This is equal to
  // the number of exprs, unless it's & or |. In those instances, it is one more
  // than the number of expressions.
  auto num_incoming = static_cast<unsigned int>(exprs.size());
  if (expr_type == Bool) ++num_incoming;

  // Create the phi node
  builder.SetInsertPoint(landing);
  llvm::PHINode *phi = builder.CreatePHI(*Bool, num_incoming, "phi");
  if (expr_type == Int) {
    BEGIN_SHORT_CIRCUIT
    CASE(ICmpS, LT);
    CASE(ICmpS, LE);
    CASE(ICmp, EQ);
    CASE(ICmp, NE);
    CASE(ICmpS, GE);
    CASE(ICmpS, GT);
    END_SHORT_CIRCUIT

  } else if (expr_type == Uint) {
    BEGIN_SHORT_CIRCUIT
    CASE(ICmpU, LT);
    CASE(ICmpU, LE);
    CASE(ICmp, EQ);
    CASE(ICmp, NE);
    CASE(ICmpU, GE);
    CASE(ICmpU, GT);
    END_SHORT_CIRCUIT

  } else if (expr_type == Char) {
    BEGIN_SHORT_CIRCUIT
    CASE(ICmp, EQ);
    CASE(ICmp, NE);
    END_SHORT_CIRCUIT

  } else if (expr_type == Real) {
    BEGIN_SHORT_CIRCUIT
    CASE(FCmpO, LT);
    CASE(FCmpO, LE);
    CASE(FCmpO, EQ); // TODO should we really allow this?
    CASE(FCmpO, NE); // TODO should we really allow this?
    CASE(FCmpO, GE);
    CASE(FCmpO, GT);
    END_SHORT_CIRCUIT

  } else if (expr_type->is_enum() || expr_type->is_pointer()) {
    BEGIN_SHORT_CIRCUIT
    CASE(ICmp, EQ);
    CASE(ICmp, NE);
    END_SHORT_CIRCUIT

  } else if (expr_type->is_struct()) {
#undef CASE

#define CASE(op_name, OP_NAME)                                                 \
  case Language::Operator::OP_NAME: {                                          \
    auto call_fn =                                                             \
        GetFunctionReferencedIn(scope_, "__" #op_name "__",                    \
                                Tup({exprs[i - 1]->type, exprs[i]->type}));    \
    assert(call_fn);                                                           \
    cmp_val =                                                                  \
        builder.CreateCall(call_fn, {lhs_val, rhs_val}, #op_name ".tmp");      \
  } break

    BEGIN_SHORT_CIRCUIT
    CASE(lt, LT);
    CASE(le, LE);
    CASE(eq, EQ);
    CASE(ne, NE);
    CASE(ge, GE);
    CASE(gt, GT);
    END_SHORT_CIRCUIT

    // TODO function, array, etc
  } else if (expr_type == Bool) {
    // TODO in the last case, can't you just come from the last branch taking
    // the yet unknown value rather than doing another branch? Answer: Yes. Do
    // it.
    if (ops.front() == Language::Operator::And) {
      for (const auto &ex : exprs) {
        builder.SetInsertPoint(curr_block);
        auto next_block = make_block("next", parent_fn);
        builder.CreateCondBr(ex->generate_code(), next_block, landing);
        phi->addIncoming(data::const_false(), builder.GetInsertBlock());
        curr_block = next_block;
      }

      builder.SetInsertPoint(curr_block);
      phi->addIncoming(data::const_true(), curr_block);

    } else if (ops.front() == Language::Operator::Or) {
      for (const auto &ex : exprs) {
        builder.SetInsertPoint(curr_block);
        auto next_block = make_block("next", parent_fn);
        builder.CreateCondBr(ex->generate_code(), landing, next_block);
        phi->addIncoming(data::const_true(), builder.GetInsertBlock());
        curr_block = next_block;
      }

      builder.SetInsertPoint(curr_block);
      phi->addIncoming(data::const_false(), curr_block);

    } else {
      assert(false && "invalid operand in short-circuiting");
    }

    builder.CreateBr(landing);
    builder.SetInsertPoint(landing);
    return phi;

  } else {
    assert(false && "Invalid type in ChainOp::generate_code");
  }
}

#undef BEGIN_SHORT_CIRCUIT
#undef CASE
#undef END_SHORT_CIRCUIT

llvm::Value *FunctionLiteral::generate_code() {
  if (code_gened) { return llvm_fn; }

  if (llvm_fn == nullptr) {
    assert(type->is_function() && "How is the type not a function?");
    auto fn_type = (Function *)type;

    if (fn_type->time() == Time::compile) return nullptr;

    // NOTE: This means a function is not assigned, but has been declared.
    llvm_fn = llvm::Function::Create(
        static_cast<llvm::FunctionType *>(type->llvm_type),
        llvm::Function::ExternalLinkage, "__anon_fn", global_module);
  }

  // Name the inputs
  auto arg_iter = llvm_fn->args().begin();
  for (const auto &input_iter : inputs) {
    arg_iter->setName(input_iter->identifier->token);
    // Set alloc
    auto decl_id   = input_iter->identifier;
    auto decl_type = decl_id->type;
    if (decl_type->is_big()) { decl_id->alloc = arg_iter; }

    ++arg_iter;
  }

  Ctx ctx;
  auto ret_type = return_type_expr->evaluate(ctx).as_type;
  if (ret_type->is_struct()) { arg_iter->setName("retval"); }

  fn_scope->set_parent_function(llvm_fn);
  fn_scope->fn_type = (Function *)type;

  auto old_block = builder.GetInsertBlock();
  builder.SetInsertPoint(fn_scope->entry);

  Scope::Stack.push(fn_scope);
  fn_scope->initialize();

  auto arg = llvm_fn->args().begin();
  for (auto &input_iter : inputs) {
    auto decl_id = input_iter->identifier;

    if (!decl_id->type->is_big()) {
      Type::CallAssignment(scope_, decl_id->type, decl_id->type,
                           input_iter->identifier->alloc, arg);
    }

    ++arg;
  }

  statements->generate_code();
  auto block = builder.GetInsertBlock();
  if (block->empty() || !llvm::isa<llvm::BranchInst>(&block->back())) {
    builder.CreateBr(fn_scope->exit);
  }

  fn_scope->leave();

  Scope::Stack.pop();

  if (old_block) { builder.SetInsertPoint(old_block); }

  assert(type->is_function());

  llvm_fn->setName(Mangle((Function *)type, this));

  code_gened = true;
  return llvm_fn;
}

llvm::Value *Generic::generate_code() { assert(false); }
llvm::Value *InDecl::generate_code() { assert(false); }

llvm::Value *Declaration::generate_code() {
  if (time() == Time::compile) { return nullptr; }

  if (IsCustomInitialized() && type->is_function()) {
    auto fn_type      = (Function *)type;
    auto llvm_fn_type = (llvm::FunctionType *)fn_type->llvm_type;
    auto mangled_name = Mangle(fn_type, identifier);

    if (init_val->is_function_literal()) {
      auto func            = (FunctionLiteral *)init_val;
      func->fn_scope->name = identifier->token;

      func->llvm_fn = (llvm::Function *)global_module->getOrInsertFunction(
          mangled_name, llvm_fn_type);

    } else if (init_val->is_binop()) {
      assert(((Binop *)init_val)->op == Language::Operator::Mul);
      auto binop   = (Binop *)init_val;
      auto lhs_val = binop->lhs->generate_code();
      auto rhs_val = binop->rhs->generate_code();

      return FunctionComposition(mangled_name, lhs_val, rhs_val, fn_type);
    }

    llvm::Value *val = init_val->generate_code();
    assert(val);
    val->setName(mangled_name);

  } else if (!IsDefaultInitialized() && !IsUninitialized()) {
    llvm::Value *var = identifier->generate_lvalue();
    llvm::Value *val = init_val->generate_code();

    assert(var && val);

    Type::CallAssignment(scope_, identifier->type, init_val->type, var, val);
  }
  return nullptr;
}

// TODO cleanup. Nothing incorrect here that I know of, just can be simplified
llvm::Value *Case::generate_code() {
  size_t num_key_vals = key_vals.size();
  auto parent_fn      = builder.GetInsertBlock()->getParent();
  // Condition blocks - The ith block is what you reach when you've
  // failed the ith condition, where conditions are labelled starting at zero.
  std::vector<llvm::BasicBlock *> case_blocks(num_key_vals - 1);

  for (auto &block : case_blocks) {
    block = make_block("case.block", parent_fn);
  }

  // Landing blocks
  auto current_block = builder.GetInsertBlock();
  auto case_landing = make_block("case.landing", parent_fn);
  builder.SetInsertPoint(case_landing);

  llvm::PHINode *phi_node =
      builder.CreatePHI(*(type->is_big() ? Ptr(type) : type),
                        static_cast<unsigned int>(num_key_vals), "phi");
  builder.SetInsertPoint(current_block);

  for (size_t i = 0; i < case_blocks.size(); ++i) {
    auto cmp_val    = key_vals[i].first->generate_code();
    auto true_block = make_block("land_true", parent_fn);

    // If it's false, move on to the next block
    builder.CreateCondBr(cmp_val, true_block, case_blocks[i]);
    builder.SetInsertPoint(true_block);
    auto output_val = key_vals[i].second->generate_code();

    // NOTE: You may be tempted to state that you are coming from the
    // block 'true_block'. However, if the code generated for the right-hand
    // side of the '=>' node is not just a single basic block, this will not
    // be the case.
    phi_node->addIncoming(output_val, builder.GetInsertBlock());
    builder.CreateBr(case_landing);

    builder.SetInsertPoint(case_blocks[i]);
  }

  auto output_val = key_vals.back().second->generate_code();

  phi_node->addIncoming(output_val, builder.GetInsertBlock());
  builder.CreateBr(case_landing);
  builder.SetInsertPoint(case_landing);

  return phi_node;
}

llvm::Value *ArrayLiteral::generate_code() {
  // TODO if this is never assigned to anything, it will be leaked. This should
  // be verified.

  auto type_as_array = (Array *)type;
  auto element_type  = type_as_array->data_type;
  size_t num_elems   = elems.size();

  auto current_block = builder.GetInsertBlock();
  assert(CurrentScope()->is_block_scope());
  auto curr_scope = static_cast<BlockScope *>(CurrentScope());
  auto entry_block = curr_scope->is_function_scope()
                         ? curr_scope->entry
                         : curr_scope->containing_function_->entry;

  builder.SetInsertPoint(entry_block->begin());
  // It's possible we never use this, in which case we must ensure that our
  // free-ing it is safe.

  auto array_data = type->allocate();
  type->call_init(array_data);

  builder.SetInsertPoint(current_block);

  assert(type_as_array->fixed_length);

  // Initialize the literal
  for (size_t i = 0; i < num_elems; ++i) {
    auto data_ptr =
        builder.CreateGEP(array_data, {data::const_uint(0), data::const_uint(i)});

    // TODO determine when initialization is necessary. Certainly it's not for
    // primitive types
    if (!element_type->is_primitive()) { element_type->call_init(data_ptr); }

    Type::CallAssignment(scope_, element_type, element_type, data_ptr,
                         elems[i]->generate_code());
  }

  assert(scope_->is_block_scope());
  static_cast<BlockScope *>(scope_)->defer_uninit(type, array_data);

  return array_data;
}

llvm::Value *Conditional::generate_code() {
  // TODO if you forget this, it causes bad bugs. Make it impossible to forget
  // this!!!
  auto parent_fn = builder.GetInsertBlock()->getParent();
  for (auto s : body_scopes) { s->set_parent_function(parent_fn); }

  std::vector<llvm::BasicBlock *> cond_block(conditions.size(), nullptr);
  std::vector<llvm::BasicBlock *> body_block(body_scopes.size(), nullptr);

  for (size_t i = 0; i < cond_block.size(); ++i) {
    cond_block[i] = make_block("cond.block", parent_fn);
  }

  for (size_t i = 0; i < body_block.size(); ++i) {
    body_block[i] = make_block("body.block", parent_fn);
  }

  auto *land_block = make_block("land", parent_fn);

  builder.CreateBr(cond_block[0]);

  for (size_t i = 0; i < conditions.size() - 1; ++i) {
    builder.SetInsertPoint(cond_block[i]);
    auto condition = conditions[i]->generate_code();
    builder.CreateCondBr(condition, body_scopes[i]->entry, cond_block[i + 1]);
  }

  // Last step
  builder.SetInsertPoint(cond_block.back());
  auto condition = conditions.back()->generate_code();
  builder.CreateCondBr(condition, body_scopes[conditions.size() - 1]->entry,
                       has_else() ? body_scopes.back()->entry : land_block);

  // This loop covers the case of else
  for (size_t i = 0; i < body_scopes.size(); ++i) {
    assert(!body_scopes[i]->land);
    body_scopes[i]->land = land_block;

    Scope::Stack.push(body_scopes[i]);
    body_scopes[i]->initialize();
    builder.CreateBr(body_block[i]);

    builder.SetInsertPoint(body_block[i]);
    statements[i]->generate_code();

    auto term_inst = builder.GetInsertBlock()->getTerminator();
    if (!term_inst) { builder.CreateBr(body_scopes[i]->exit); }

    builder.SetInsertPoint(body_scopes[i]->exit);
    body_scopes[i]->uninitialize();

    auto exit_flag =
        builder.CreateLoad(body_scopes[i]->containing_function_->ExitFlag());

    assert(body_scopes[i]->parent->is_block_scope());
    auto parent_block_scope = static_cast<BlockScope *>(body_scopes[i]->parent);
    auto is_zero = builder.CreateICmpEQ(exit_flag, data::const_char('\00'));
    builder.CreateCondBr(is_zero, land_block, parent_block_scope->exit);

    Scope::Stack.pop();
  }

  builder.SetInsertPoint(land_block);
  return nullptr;
}

llvm::Value *EnumLiteral::generate_code() { assert(false); }
llvm::Value *StructLiteral::generate_code() { assert(false); }
llvm::Value *ParametricStructLiteral::generate_code() { assert(false); }

llvm::Value *Jump::generate_code() {
  llvm::Value *exit_flag_alloc = nullptr;
  if (CurrentScope()->is_function_scope()) {
    if (jump_type == JumpType::Return) {
      // Don't generate the exit flag if we're returning from the base level.
      builder.CreateBr(static_cast<BlockScope *>(CurrentScope())->exit);
      return nullptr;
    } else {
      // But do generate it if we're not at the base level.
      exit_flag_alloc = static_cast<FnScope *>(CurrentScope())->ExitFlag();
    }
  } else {
    // Otherwise generate the exit flag for the containing scope
    exit_flag_alloc = CurrentScope()->containing_function_->ExitFlag();
  }

  switch (jump_type) {
  case JumpType::Restart: {
    builder.CreateStore(RESTART_FLAG, exit_flag_alloc);
  } break;
  case JumpType::Continue: {
    builder.CreateStore(CONTINUE_FLAG, exit_flag_alloc);
  } break;
  case JumpType::Repeat: {
    builder.CreateStore(REPEAT_FLAG, exit_flag_alloc);
  } break;
  case JumpType::Break: {
    builder.CreateStore(BREAK_FLAG, exit_flag_alloc);
  } break;
  case JumpType::Return: {
    builder.CreateStore(RETURN_FLAG, exit_flag_alloc);
  } break;
  }

  builder.CreateBr(static_cast<BlockScope *>(CurrentScope())->exit);

  return nullptr;
}

llvm::Value *While::generate_code() {
  auto parent_fn = builder.GetInsertBlock()->getParent();
  while_scope->set_parent_function(parent_fn);

  assert(!while_scope->land);
  auto cond_block = make_block("while.cond", parent_fn);
  auto body_block = make_block("while.body", parent_fn);
  while_scope->land = make_block("while.land", parent_fn);

  // Loop condition
  builder.CreateBr(cond_block);

  builder.SetInsertPoint(cond_block);
  auto cond = condition->generate_code();

  builder.CreateCondBr(cond, while_scope->entry, while_scope->land);
  // Loop body
  Scope::Stack.push(while_scope);
  while_scope->initialize();
  builder.CreateBr(body_block);

  builder.SetInsertPoint(body_block);
  statements->generate_code();
  builder.CreateBr(while_scope->exit);

  builder.SetInsertPoint(while_scope->exit);
  while_scope->uninitialize();

  auto exit_flag =
      builder.CreateLoad(while_scope->containing_function_->ExitFlag());

  // Switch. By default go back to the start of the loop.
  auto switch_stmt = builder.CreateSwitch(exit_flag, cond_block);
  switch_stmt->addCase(BREAK_FLAG, while_scope->land);

  assert(while_scope->parent->is_block_scope());
  auto parent_block_scope = static_cast<BlockScope *>(while_scope);
  switch_stmt->addCase(RETURN_FLAG, parent_block_scope->exit);

  // Landing
  Scope::Stack.pop();
  builder.SetInsertPoint(while_scope->land);

  return nullptr;
}

llvm::Value *For::generate_code() {
  // The block you were at just before getting to the loop
  auto start_block = builder.GetInsertBlock();

  auto parent_fn = start_block->getParent();
  for_scope->set_parent_function(parent_fn);

  auto init_iters = make_block("init.iters", parent_fn);
  auto incr_iters = make_block("incr.iters", parent_fn);
  auto phi_block  = make_block("phis", parent_fn);
  auto cond_block = make_block("cond.block", parent_fn);

  // Make a landing block so you know where to go at the end of the loop.
  assert(!for_scope->land);
  for_scope->land = make_block("loop.land", parent_fn);


  builder.SetInsertPoint(cond_block);
  llvm::Value *done_cmp = data::const_false();

  builder.SetInsertPoint(start_block);
  builder.CreateBr(init_iters);

  auto num_iters = iterators.size();
  for (size_t i = 0; i < num_iters; ++i) {
    auto iter      = iterators[i];
    if (!iter->identifier->alloc) {
      assert(iter->scope_->is_block_scope());
      auto block_scope = (BlockScope *)(iter->scope_);
      iter->identifier->alloc = block_scope->AllocateLocally(
          iter->identifier->type, iter->identifier->token);
    }
    auto container = iter->container;
    llvm::PHINode *phi = nullptr;

    if (container->type->is_slice()) {
      /* Work on init block */
      builder.SetInsertPoint(init_iters);

      // TODO assume slices aren't first-class types and only defined literally
      // here.
      assert(container->is_binop());

      auto container_as_binop = static_cast<Binop *>(container);
      assert(container_as_binop->op == Language::Operator::Index &&
             container_as_binop->lhs->type->is_array());

      assert(container_as_binop->rhs->type->is_range());

      auto array      = container_as_binop->lhs;
      auto array_val  = array->generate_code();
      auto array_type = static_cast<Array *>(array->type);

      auto range      = container_as_binop->rhs;
      llvm::Value *start_num, *end_num;
      if (container_as_binop->rhs->is_binop()) {
        start_num = static_cast<Binop *>(range)->lhs->generate_code();
        end_num   = static_cast<Binop *>(range)->rhs->generate_code();
      } else {
        assert(container_as_binop->rhs->is_unop());
        auto range_as_unop = static_cast<Unop *>(range);
        assert(range_as_unop->op == Language::Operator::Dots);
        start_num = range_as_unop->operand->generate_code();

        if (array_type->fixed_length) {
          end_num = data::const_uint(array_type->len);

        } else {
          // TODO you're subtracting one so that adding one below is safe.
          // Probably will be optimized away, but you should fix it by hand
          // anyways.
          end_num = builder.CreateSub(
              builder.CreateLoad(builder.CreateGEP(
                  array_val, {data::const_uint(0), data::const_uint(0)})),
              data::const_uint(1));
        }
      }

      auto head_ptr =
          (array_type->fixed_length)
              ? builder.CreateGEP(array_val,
                                  {data::const_uint(0), data::const_uint(0)})
              : builder.CreateLoad(builder.CreateGEP(
                    array_val, {data::const_uint(0), data::const_uint(1)}));

      auto start_ptr = builder.CreateGEP(head_ptr, start_num, "start_ptr");
      auto end_ptr = builder.CreateGEP(
          head_ptr, builder.CreateAdd(end_num, data::const_uint(1)), "end_ptr");

      /* Work on phi block */
      builder.SetInsertPoint(phi_block);
      phi = builder.CreatePHI(*Ptr(iter->type), 2, "phi");
      phi->addIncoming(start_ptr, init_iters);

      /* Work on cond block */
      builder.SetInsertPoint(cond_block);
      iter->identifier->alloc = phi;
      done_cmp = builder.CreateOr(done_cmp, builder.CreateICmpEQ(phi, end_ptr));

      /* Work on incr block */
      builder.SetInsertPoint(incr_iters);
      auto next = builder.CreateGEP(phi, data::const_uint(1));
      phi->addIncoming(next, incr_iters);

    } else if (container->type->is_array()) {
      /* Work on init block */
      builder.SetInsertPoint(init_iters);
      auto container_val = container->generate_code();
      assert(container_val && "container_val is nullptr");

      llvm::Value *start_ptr, *end_ptr;

      auto array_type = (Array *)container->type;
      if (array_type->fixed_length) {
        start_ptr = builder.CreateGEP(
            container_val, {data::const_uint(0), data::const_uint(0)});

        end_ptr = builder.CreateGEP(
            start_ptr, data::const_uint(array_type->len), "end_ptr");
      } else {
        start_ptr = builder.CreateLoad(
            builder.CreateGEP(container_val,
                              {data::const_uint(0), data::const_uint(1)}),
            "start_ptr");

        auto len_ptr = builder.CreateGEP(
            container_val, {data::const_uint(0), data::const_uint(0)},
            "len_ptr");
        auto len_val = builder.CreateLoad(len_ptr, "len");
        end_ptr = builder.CreateGEP(start_ptr, len_val, "end_ptr");
      }

      /* Work on phi block */
      builder.SetInsertPoint(phi_block);
      phi = builder.CreatePHI(*Ptr(iter->type), 2, "phi");
      phi->addIncoming(start_ptr, init_iters);

      /* Work on cond block */
      builder.SetInsertPoint(cond_block);
      iter->identifier->alloc = phi;
      done_cmp = builder.CreateOr(done_cmp, builder.CreateICmpEQ(phi, end_ptr));

      /* Work on incr block */
      builder.SetInsertPoint(incr_iters);
      auto next = builder.CreateGEP(phi, data::const_uint(1));
      phi->addIncoming(next, incr_iters);

    } else if (container->type->is_range()) {
      if (container->is_binop()) {
        assert(static_cast<Binop *>(container)->op == Language::Operator::Dots);

        /* Work on init block */
        builder.SetInsertPoint(init_iters);
        llvm::Value *start_val =
            static_cast<Binop *>(container)->lhs->generate_code();
        llvm::Value *end_val =
            static_cast<Binop *>(container)->rhs->generate_code();

        /* Work on phi block */
        builder.SetInsertPoint(phi_block);
        phi = builder.CreatePHI(*iter->type, 2, "phi");
        phi->addIncoming(start_val, init_iters);

        /* Work on cond block */
        builder.SetInsertPoint(cond_block);
        builder.CreateStore(phi, iter->identifier->alloc);
        done_cmp = builder.CreateOr(done_cmp,
                                    (iter->type == Int)
                                        ? builder.CreateICmpSGT(phi, end_val)
                                        : builder.CreateICmpUGT(phi, end_val));
        // ^ TODO should be testing for LE because of edge case 

      } else if (container->is_unop()) {
        assert(static_cast<Unop *>(container)->op == Language::Operator::Dots);

        /* Work on init block */
        builder.SetInsertPoint(init_iters);
        llvm::Value *start_val =
            static_cast<Unop *>(container)->operand->generate_code();

        /* Work on phi block */
        builder.SetInsertPoint(phi_block);
        phi = builder.CreatePHI(*iter->type, 2, "phi");
        phi->addIncoming(start_val, init_iters);

        /* Work on cond block */
        builder.SetInsertPoint(cond_block);
        builder.CreateStore(phi, iter->identifier->alloc);

      } else {
        assert(false);
      }

      /* Work on incr block */
      builder.SetInsertPoint(incr_iters);
      llvm::Value *next = builder.CreateAdd(
          builder.CreateLoad(iter->identifier->alloc),
          iter->type == Char ? data::const_char(1) : data::const_uint(1));
      phi->addIncoming(next, incr_iters);
    } else {
      Ctx ctx;
      auto ty = container->evaluate(ctx).as_type;
      assert(ty->is_enum());
      auto enum_type = static_cast<Enumeration *>(ty);

      /* Work on init block */
      // Note: Nothing to do

      /* Work on phi block */
      builder.SetInsertPoint(phi_block);
      phi = builder.CreatePHI(*Uint, 2, "phi");
      phi->addIncoming(data::const_uint(0), init_iters);

      /* Work on cond block */
      builder.SetInsertPoint(cond_block);
      done_cmp = builder.CreateOr(
          done_cmp, builder.CreateICmpEQ(
                        phi, data::const_uint(enum_type->int_values.size())));
      builder.CreateStore(phi, iter->identifier->alloc);

      /* Work on incr block */
      builder.SetInsertPoint(incr_iters);
      auto next = builder.CreateAdd(builder.CreateLoad(iter->identifier->alloc),
                                    data::const_uint(1));
      phi->addIncoming(next, incr_iters);
    }
  }

  // Link Blocks
  builder.SetInsertPoint(init_iters);
  builder.CreateBr(phi_block);

  builder.SetInsertPoint(incr_iters);
  builder.CreateBr(phi_block);

  builder.SetInsertPoint(phi_block);
  builder.CreateBr(cond_block);

  builder.SetInsertPoint(cond_block);
  builder.CreateCondBr(done_cmp, for_scope->land, for_scope->entry);

  // Ready for loop body
  auto loop_block = make_block("loop.body", parent_fn);

  Scope::Stack.push(for_scope);
  for_scope->initialize();
  builder.CreateBr(loop_block);

  builder.SetInsertPoint(loop_block);
  statements->generate_code();
  builder.CreateBr(for_scope->exit);

  for_scope->uninitialize();

  auto exit_flag =
      builder.CreateLoad(for_scope->containing_function_->ExitFlag());

  // Switch. By default go back to the start of the loop.
  auto switch_stmt = builder.CreateSwitch(exit_flag, incr_iters);
  // CONTINUE_FLAG is the default
  switch_stmt->addCase(RESTART_FLAG, init_iters);
  switch_stmt->addCase(REPEAT_FLAG, for_scope->entry);
  switch_stmt->addCase(BREAK_FLAG, for_scope->land);
  assert(for_scope->parent->is_block_scope());
  auto parent_block_scope = static_cast<BlockScope *>(for_scope->parent);
  switch_stmt->addCase(RETURN_FLAG, parent_block_scope->exit);

  Scope::Stack.pop();

  builder.SetInsertPoint(for_scope->land);

  return nullptr;
}

llvm::Value *DummyTypeExpr::generate_code() {
  assert(false);
  return nullptr;
}
} // namespace AST


#undef CREATE_CALL
#undef PRIMITIVE_CALL

#undef BREAK_FLAG
#undef CONTINUE_FLAG
#undef RETURN_FLAG
#undef REPEAT_FLAG
#undef RESTART_FLAG
