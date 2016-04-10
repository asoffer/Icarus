#include "AST.h"
#include "Context.h"

// This is the only place that needs to know about operators, so rather than
// keep them in the header everywhere, we just put the necessary templates in
// this one header.
#include "Type/ops.h"

extern llvm::BasicBlock *make_block(const std::string &name,
                                    llvm::Function *fn);

extern ErrorLog error_log;

extern llvm::Module *global_module;
extern llvm::IRBuilder<> builder;

namespace cstdlib {
extern llvm::Constant *free();
extern llvm::Constant *memcpy();
extern llvm::Constant *malloc();
} // namespace cstdlib

namespace builtin {
extern llvm::Function *ascii();
} // namespace builtin

namespace data {
extern llvm::Value *null_pointer(Type *t);
extern llvm::Value *null(TypePtr t);
extern llvm::ConstantInt *const_true();
extern llvm::ConstantInt *const_false();
extern llvm::ConstantInt *const_uint(size_t n);
extern llvm::ConstantInt *const_int(int n);
extern llvm::ConstantInt *const_char(char c);
extern llvm::ConstantFP *const_real(double d);
extern llvm::Value *global_string(const std::string &s);
} // namespace data

llvm::Value *struct_memcpy(TypePtr type, llvm::Value *val) {
  auto arg_ptr  = builder.CreateAlloca(type, nullptr, "struct.tmp");
  auto tmp_raw  = builder.CreateBitCast(arg_ptr, RawPtr);
  auto val_raw  = builder.CreateBitCast(val, RawPtr);
  auto mem_copy = builder.CreateCall(
      cstdlib::memcpy(),
      {tmp_raw, val_raw, data::const_uint(type.get->bytes())});

  return builder.CreateBitCast(mem_copy, *Ptr(type));
}

#define BREAK_FLAG data::const_char('\01')
#define CONTINUE_FLAG data::const_char('\02')
#define RETURN_FLAG data::const_char('\04')

namespace AST {
llvm::Value *Identifier::generate_code() {
  if (type == Type_) {
    return nullptr;

  } else if (type.is_function()) {
    // TODO better way to get functions based on their name
    auto fn_type                     = static_cast<Function *>(type.get);
    llvm::FunctionType *llvm_fn_type = *fn_type;
    auto mangled_name                = Mangle(fn_type, token());

    return global_module->getOrInsertFunction(mangled_name, llvm_fn_type);

  } else if (type.is_big()) {
    return alloc;

  } else {
    return builder.CreateLoad(alloc, token());
  }
}

// Invariant:
// Only returns nullptr if the expression type is void or a type
llvm::Value *Terminal::generate_code() {
  // TODO remove dependence on token() altogether
  switch (terminal_type) {
  case Language::Terminal::Type:
  case Language::Terminal::Return: {

    llvm::Value *exit_flag_alloc =
        CurrentScope()->is_function_scope()
            ? static_cast<FnScope *>(CurrentScope())->exit_flag
            : CurrentScope()->containing_function_->exit_flag;

    builder.CreateStore(RETURN_FLAG, exit_flag_alloc);
    builder.CreateBr(static_cast<BlockScope *>(CurrentScope())->exit);

    // TODO branch to end of current function scope, and delete everything on
    // the way out?
    return nullptr;
  }
  case Language::Terminal::Null: {
    assert(type.is_pointer() && "Null pointer of non-pointer type ");
    return data::null(type);
  }
  case Language::Terminal::ASCII: {
    return builtin::ascii();
  }
  case Language::Terminal::True: {
    return data::const_true();
  }
  case Language::Terminal::False: {
    return data::const_false();
  }
  case Language::Terminal::Else:
    return data::const_true();
  // Else is a terminal only in case statements. In this situation, it's
  // corresponding resulting value is always to be the one chosen, so we
  // should have 'else' represent the value true.
  case Language::Terminal::Char: {
    return data::const_char(token()[0]);
  }
  case Language::Terminal::Int: {
    return data::const_int(std::stoi(token()));
  }
  case Language::Terminal::Real: {
    return data::const_real(std::stod(token()));
  }
  case Language::Terminal::UInt: {
    return data::const_uint(std::stoul(token()));
  }
  case Language::Terminal::Alloc: {
    return cstdlib::malloc();
  }
  case Language::Terminal::StringLiteral: {
    auto str = data::global_string(token());
    auto len = data::const_uint(token().size());

    auto str_alloc = builder.CreateAlloca(type);

    auto len_ptr = builder.CreateGEP(
        str_alloc, {data::const_uint(0),
                    static_cast<Structure *>(String.get)->field_num("length")},
        "len_ptr");
    builder.CreateStore(len, len_ptr);

    auto char_array_ptr = builder.CreateGEP(
        str_alloc, {data::const_uint(0),
                    static_cast<Structure *>(String.get)->field_num("chars")},
        "char_array_ptr");

    // NOTE: no need to uninitialize because we never initialized it.
    Arr(Char)->initialize_literal(char_array_ptr, token().size());

    auto char_data_ptr = builder.CreateLoad(
        builder.CreateGEP(char_array_ptr,
                          {data::const_uint(0), data::const_uint(1)}),
        "char_data_ptr");

    builder.CreateCall(cstdlib::memcpy(), {char_data_ptr, str, len});

    return str_alloc;
  }
  }
}

// Invariant:
// Only returns nullptr if the expression type is void or a type
llvm::Value *Unop::generate_code() {
  // We first go through all the possible operators where we don't necessarily
  // need to generate code for the operand.
  switch (op) {
  case Language::Operator::And: {
    // TODO ensure that this has an l-value
    return operand->generate_lvalue();
  }
  case Language::Operator::Print: {
    // NOTE: Type punning Type* -> llvm::Value*
    llvm::Value *val = (operand->type == Type_)
                           ? reinterpret_cast<llvm::Value *>(
                                 operand->evaluate(CurrentContext()).as_type)
                           : operand->generate_code();

    if (operand->type.is_struct()) {
      // TODO maybe callees should be responsible for the struct memcpy?
      val = struct_memcpy(operand->type, val);
    }

    operand->type.get->call_print(val);

    return nullptr;
  }
  default:;
  }

  llvm::Value *val = operand->generate_code();
  switch (op) {
  case Language::Operator::Sub: {
    return operand->type.get->call_neg(val);
  }
  case Language::Operator::Not: {
    return operand->type.get->call_not(val);
  }
  case Language::Operator::Free: {
    builder.CreateCall(cstdlib::free(), {builder.CreateBitCast(val, RawPtr)});
    // TODO only if it has an l-value
    builder.CreateStore(data::null(operand->type), operand->generate_lvalue());
    return nullptr;
  }
  case Language::Operator::Return: {
    assert(scope_->is_block_scope());
    static_cast<BlockScope *>(scope_)->make_return(val);
    return nullptr;
  }
  case Language::Operator::At: {
    return type.is_big() ? val : builder.CreateLoad(val);
  }
  case Language::Operator::Call: {
    assert(operand->type.is_function() && "Operand should be a function.");
    auto out_type = static_cast<Function *>(operand->type.get)->output;
    // TODO this whole section needs an overhaul when we totally settle on how
    // to pass large things.
    if (out_type.is_struct()) {
      // TODO move this outside of any potential loops
      auto local_ret = builder.CreateAlloca(out_type);

      builder.CreateCall(static_cast<llvm::Function *>(val), local_ret);
      return local_ret;

    } else {
      return builder.CreateCall(static_cast<llvm::Function *>(val));
    }
  }
  default: assert(false && "Unimplemented unary operator codegen");
  }
}

llvm::Value *Access::generate_code() {
  if (operand->type == Type_) {
    // As of 2/26/16, the only way an operand could be a type is if it's an
    // enum, as in `Color.Red`. Here Color is an enum with the access
    // parameter Red.
    //
    // NOTE: this will likely change if we implement UFCS. In that case, this
    // whole method will probably be out of date.
    auto expr_as_type = operand->evaluate(CurrentContext()).as_type;
    assert(expr_as_type->is_enum() && "Expression should be an enum");
    return static_cast<Enumeration *>(expr_as_type)->get_value(member_name);
  }

  // Generate the code for the operand
  auto eval = operand->generate_code();

  // To make access pass through all layers of pointers, we loop through
  // loading values while we're looking at pointers.
  auto base_type = operand->type;
  while (base_type.is_pointer()) {
    base_type = static_cast<Pointer *>(base_type.get)->pointee;
    if (!base_type.is_big()) eval = builder.CreateLoad(eval);
  }

  if (base_type.is_array() &&member_name == "size") {
    return builder.CreateLoad(
        builder.CreateGEP(eval, {data::const_uint(0), data::const_uint(0)}));
  }

  if (base_type.is_struct()) {
    auto struct_type = static_cast<Structure *>(base_type.get);

    if (!type.stores_data()) { assert(false && "Not yet implemented"); }

    auto elem_ptr = builder.CreateGEP(
        eval, {data::const_uint(0), struct_type->field_num(member_name)});
    return type.is_big() ? elem_ptr : builder.CreateLoad(elem_ptr);

  } else {
    assert(false && "Not yet implemented");
  }
}

llvm::Value *Binop::generate_code() {
  if (time() == Time::compile) {
    return llvm_value(evaluate(CurrentContext()));
  }

  llvm::Value *lhs_val = nullptr;

  if (lhs->type.is_quantum()) {
    Function *fn_type = nullptr;
    for (auto opt : static_cast<QuantumType *>(lhs->type.get)->options) {
      fn_type = static_cast<Function *>(opt.get);
      if (fn_type->input == rhs->type) break;
    }

    // Because of type verification we know that if we get here, there is a
    // valid choice for the quantum type. The loop above cannot end normally. It
    // must end at a break statement.
    assert(fn_type && fn_type->input == rhs->type);

    llvm::FunctionType *llvm_fn_type = *fn_type;

    auto mangled_name = Mangle(static_cast<Function *>(fn_type), lhs->token());
    lhs_val           = global_module->getOrInsertFunction(mangled_name, llvm_fn_type);

  } else {
    lhs_val = lhs->generate_code();
  }
  assert(lhs_val);

  switch (op) {
  case Language::Operator::Index: {
    if (lhs->type.is_array()) {
      auto data_ptr = builder.CreateLoad(builder.CreateGEP(
          lhs_val, {data::const_uint(0), data::const_uint(1)}));
      if (type.is_big()) {
        return builder.CreateGEP(data_ptr, {rhs->generate_code()}, "array_val");
      } else {
        return builder.CreateLoad(
            builder.CreateGEP(data_ptr, {rhs->generate_code()}, "array_val"));
      }
    }
    assert(false && "Not yet implemented");
  }
  case Language::Operator::Cast: {
    return lhs->type.get->call_cast(lhs_val,
                                rhs->evaluate(CurrentContext()).as_type);
  }
  case Language::Operator::Call: {
    if (lhs->type.is_function() || lhs->type.is_quantum()) {
      std::vector<llvm::Value *> arg_vals;
      // This whole section should be pulled out into a function called
      // "collate_for_function_call" or something like that.
      if (rhs->is_comma_list()) {
        auto &arg_exprs = static_cast<ChainOp *>(rhs)->exprs;
        arg_vals.resize(arg_exprs.size(), nullptr);
        for (size_t i = 0; i < arg_exprs.size(); ++i) {
          arg_vals[i] = arg_exprs[i]->generate_code();
          assert(arg_vals[i] && "Argument value was null");

          if (arg_exprs[i]->type.is_struct()) {
            // TODO be sure to allocate this ahead of all loops and reuse it
            // when possible. Also, do this for arrays?
            arg_vals[i] =
                struct_memcpy(arg_exprs[i]->type, arg_vals[i]);
          }
        }
      } else {
        auto rhs_val = rhs->generate_code();
        assert(rhs_val && "Argument value was null");

        if (rhs->type.is_struct()) {
          // TODO be sure to allocate this ahead of all loops and reuse it
          // when possible. Also, do this for arrays?
          rhs_val = struct_memcpy(rhs->type, rhs_val);
        }

        arg_vals = {rhs_val};
      }

      if (type == Void) {
        builder.CreateCall(static_cast<llvm::Function *>(lhs_val), arg_vals);
        return nullptr;

      } else {
        return builder.CreateCall(static_cast<llvm::Function *>(lhs_val),
                                  arg_vals, "calltmp");
      }

    } else if (lhs->type.is_dependent_type()) {
      // TODO make this generic. right now dependent_type implies alloc(...)
      auto t = rhs->evaluate(CurrentContext()).as_type;
      auto alloc_ptr =
          builder.CreateCall(lhs_val, {data::const_uint(t->bytes())});
      return builder.CreateBitCast(alloc_ptr, type);
    }
  }
  default:;
  }

  auto rhs_val = rhs->generate_code();
  switch (op) {
  case Language::Operator::Add: return type.get->call_add(lhs_val, rhs_val);
  case Language::Operator::Sub: return type.get->call_sub(lhs_val, rhs_val);
  case Language::Operator::Mul: return type.get->call_mul(lhs_val, rhs_val);
  case Language::Operator::Div: return type.get->call_div(lhs_val, rhs_val);
  case Language::Operator::Mod: return type.get->call_mod(lhs_val, rhs_val);
  default:;
  }

  assert(false && "Reached end of Binop::generate_code");
}

llvm::Value *ArrayType::generate_code() {
  assert(false && "Not valid for code-gen");
}

llvm::Value *Statements::generate_code() {
  assert(!statements.empty());

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

#define CASE(cmp, llvm_call, op_name)                                          \
  case Operator::op_name: {                                                    \
    cmp =                                                                      \
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
  // TODO eval of enums at compile-time is wrong. This could be
  // 1. That the eval function is wrong, or
  // 2. That they shouldn't be determined at compile-time
  if (time() == Time::compile) {
    return llvm_value(evaluate(CurrentContext()));
  }

  using Language::Operator;

  auto expr_type = exprs[0]->type;

  // Boolean xor is separate because it can't be short-circuited
  if (expr_type == Bool && ops.front() == Operator::Xor) {
    llvm::Value *cmp_val = exprs[0]->generate_code();
    for (size_t i = 1; i < exprs.size(); ++i) {
      cmp_val = builder.CreateXor(cmp_val, exprs[i]->generate_code());
    }
    return cmp_val;
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
  llvm::PHINode *phi = builder.CreatePHI(Bool, num_incoming, "phi");
  if (expr_type == Int) {
    BEGIN_SHORT_CIRCUIT
    CASE(cmp_val, ICmpS, LT);
    CASE(cmp_val, ICmpS, LE);
    CASE(cmp_val, ICmp, EQ);
    CASE(cmp_val, ICmp, NE);
    CASE(cmp_val, ICmpS, GE);
    CASE(cmp_val, ICmpS, GT);
    END_SHORT_CIRCUIT

  } else if (expr_type == Uint) {
    BEGIN_SHORT_CIRCUIT
    CASE(cmp_val, ICmpU, LT);
    CASE(cmp_val, ICmpU, LE);
    CASE(cmp_val, ICmp, EQ);
    CASE(cmp_val, ICmp, NE);
    CASE(cmp_val, ICmpU, GE);
    CASE(cmp_val, ICmpU, GT);
    END_SHORT_CIRCUIT

  } else if (expr_type == Char) {
    BEGIN_SHORT_CIRCUIT
    CASE(cmp_val, ICmp, EQ);
    CASE(cmp_val, ICmp, NE);
    END_SHORT_CIRCUIT

  } else if (expr_type == Real) {
    BEGIN_SHORT_CIRCUIT
    CASE(cmp_val, FCmpO, LT);
    CASE(cmp_val, FCmpO, LE);
    CASE(cmp_val, FCmpO, EQ); // TODO should we really allow this?
    CASE(cmp_val, FCmpO, NE); // TODO should we really allow this?
    CASE(cmp_val, FCmpO, GE);
    CASE(cmp_val, FCmpO, GT);
    END_SHORT_CIRCUIT

  } else if (expr_type.is_enum()) {
    BEGIN_SHORT_CIRCUIT
    CASE(cmp_val, ICmp, EQ);
    CASE(cmp_val, ICmp, NE);
    END_SHORT_CIRCUIT

    // TODO struct, function, array, etc
  } else if (expr_type == Bool) {
    // TODO in the last case, can't you just come from the last branch taking
    // the yet unknown value rather than doing another branch? Answer: Yes. Do
    // it.
    if (ops.front() == Operator::And) {
      for (const auto &ex : exprs) {
        builder.SetInsertPoint(curr_block);
        auto next_block = make_block("next", parent_fn);
        builder.CreateCondBr(ex->generate_code(), next_block, landing);
        phi->addIncoming(data::const_false(), builder.GetInsertBlock());
        curr_block = next_block;
      }

      builder.SetInsertPoint(curr_block);
      phi->addIncoming(data::const_true(), curr_block);

    } else if (ops.front() == Operator::Or) {
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
  if (llvm_fn == nullptr) {
    assert(type.is_function() && "How is the type not a function?");
    auto fn_type = static_cast<Function *>(type.get);

    if (fn_type->time() == Time::compile) return nullptr;

    // NOTE: This means a function is not assigned, but has been declared.
    llvm_fn = llvm::Function::Create(
        static_cast<llvm::FunctionType *>(type.get->llvm_type),
        llvm::Function::ExternalLinkage, "__anon_fn", global_module);
  }

  // Name the inputs
  auto arg_iter = llvm_fn->args().begin();
  for (const auto &input_iter : inputs) {
    arg_iter->setName(input_iter->identifier->token());
    // Set alloc
    auto decl_id   = input_iter->identifier;
    auto decl_type = decl_id->type;
    if (decl_type.is_big()) { decl_id->alloc = arg_iter; }

    ++arg_iter;
  }

  auto ret_type = return_type_expr->evaluate(CurrentContext()).as_type;
  if (ret_type->is_struct()) { arg_iter->setName("retval"); }

  fn_scope->set_parent_function(llvm_fn);
  fn_scope->fn_type = static_cast<Function *>(type.get);

  auto old_block = builder.GetInsertBlock();
  builder.SetInsertPoint(fn_scope->entry);

  Scope::Stack.push(fn_scope);
  fn_scope->initialize();

  auto arg = llvm_fn->args().begin();
  for (auto &input_iter : inputs) {
    auto decl_id = input_iter->identifier;

    if (!decl_id->type.is_big()) {
      builder.CreateCall(decl_id->type.get->assign(),
                         {arg, input_iter->identifier->alloc});
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

  return llvm_fn;
}

// This function exists because both '=' and ':=' need to call some version of
// the same code. it's been factored out here.
llvm::Value *generate_assignment_code(Expression *lhs, Expression *rhs) {
  llvm::Value *var = nullptr;
  llvm::Value *val = nullptr;

  // Treat functions special
  if (lhs->is_identifier() && rhs->type.is_function()) {
    if (lhs->token() == "__print__" || lhs->token() == "__assign__") {
      val = rhs->generate_code();
      assert(val && "RHS of assignment generated null code");

      // TODO verify that you are defining print for this struct type.
      auto rhs_as_func = static_cast<Function *>(rhs->type.get);
      auto arg_type    = static_cast<Structure *>(rhs_as_func->input.get);

      arg_type->set_print(static_cast<llvm::Function *>(val));

    } else {
      auto fn_type                     = static_cast<Function *>(rhs->type.get);
      llvm::FunctionType *llvm_fn_type = *fn_type;
      auto mangled_name                = Mangle(fn_type, lhs->token());

      if (rhs->is_function_literal()) {
        auto fn = static_cast<FunctionLiteral *>(rhs);

        fn->llvm_fn = static_cast<llvm::Function *>(
            global_module->getOrInsertFunction(mangled_name, llvm_fn_type));
      }

      val = rhs->generate_code();

      // Null value can be returned here, if for instance, the rhs is a function
      // on types.
      if (val) { val->setName(mangled_name); }
    }
  } else {
    var = lhs->generate_lvalue();
    assert(var && "LHS of assignment generated null code");

    val = rhs->generate_code();
    assert(val && "RHS of assignment generated null code");

    builder.CreateCall(lhs->type.get->assign(), {val, var});
  }

  return nullptr;
}

#define CASE(op, llvm_op, symbol)                                              \
  case Operator::op: {                                                         \
    val = builder.Create##llvm_op(lhs_val, rhs_val, symbol);                   \
  } break

llvm::Value *Assignment::generate_code() {
  // The left-hand side may be a declaration
  if (lhs->is_declaration()) {
    // TODO maybe the declarations generate_code ought to return an l-value for
    // the thing it declares?
    return generate_assignment_code(static_cast<Declaration *>(lhs)->identifier,
                                    rhs);
  }

  using Language::Operator;
  if (op == Operator::OrEq || op == Operator::XorEq || op == Operator::AndEq ||
      op == Operator::AddEq || op == Operator::SubEq || op == Operator::MulEq ||
      op == Operator::DivEq || op == Operator::ModEq) {

    auto lhs_val = lhs->generate_code();
    assert(lhs_val && "LHS of assignment generated null code");

    auto lval = lhs->generate_lvalue();
    assert(lval && "LHS lval of assignment generated null code");

    if (lhs->type == Bool) {
      switch (op) {
      case Operator::XorEq: {
        auto rhs_val = rhs->generate_code();
        assert(rhs_val && "RHS of assignment generated null code");

        builder.CreateStore(builder.CreateXor(lhs_val, rhs_val, "xortmp"),
                            lval);
      } break;
      case Operator::AndEq: {
        auto parent_fn   = builder.GetInsertBlock()->getParent();
        auto more_block  = make_block("more", parent_fn);
        auto merge_block = make_block("merge", parent_fn);
        builder.CreateCondBr(lhs_val, more_block, merge_block);

        builder.SetInsertPoint(more_block);
        auto rhs_val = rhs->generate_code();
        assert(rhs_val && "RHS of assignment generated null code");

        builder.CreateStore(rhs_val, lval);
        builder.CreateBr(merge_block);
        builder.SetInsertPoint(merge_block);
      } break;
      case Operator::OrEq: {
        auto parent_fn   = builder.GetInsertBlock()->getParent();
        auto more_block  = make_block("more", parent_fn);
        auto merge_block = make_block("merge", parent_fn);
        builder.CreateCondBr(lhs_val, merge_block, more_block);

        builder.SetInsertPoint(more_block);
        auto rhs_val = rhs->generate_code();
        assert(rhs_val && "RHS of assignment generated null code");

        builder.CreateStore(rhs_val, lval);
        builder.CreateBr(merge_block);
        builder.SetInsertPoint(merge_block);
      } break;
      default: assert(false && "Invalid assignment operator for boolean type");
      }
      return nullptr;
    }

    auto rhs_val = rhs->generate_code();
    assert(rhs_val && "RHS of assignment generated null code");

    llvm::Value *val = nullptr;
    if (lhs->type == Int) {
      switch (op) {
        CASE(AddEq, Add, "at");
        CASE(SubEq, Sub, "st");
        CASE(MulEq, Mul, "mt");
        CASE(DivEq, SDiv, "dt");
        CASE(ModEq, SRem, "rt");
      default: assert(false && "Invalid operator");
      }

    } else if (lhs->type == Uint) {
      switch (op) {
        CASE(AddEq, Add, "at");
        CASE(SubEq, Sub, "st");
        CASE(MulEq, Mul, "mt");
        CASE(DivEq, UDiv, "dt");
        CASE(ModEq, URem, "rt");
      default: assert(false && "Invalid operator");
      }

    } else if (lhs->type == Real) {
      switch (op) {
        CASE(AddEq, FAdd, "at");
        CASE(SubEq, FSub, "st");
        CASE(MulEq, FMul, "mt");
        CASE(DivEq, FDiv, "dt");
      default: assert(false && "Invalid operator");
      }
    } else {
      assert(false && "Not yet implemented");
    }

    builder.CreateStore(val, lval);
    return nullptr;
  }

  return generate_assignment_code(lhs, rhs);
}
#undef CASE

llvm::Value *Declaration::generate_code() {
  // In the case of something like
  // foo: [10; char], an actual allocation needs to occur.
  // TODO maybe this should be moved into the scope?
  // Or maybe declarations in scope should be moved here?
  if (decl_type == DeclType::Std && type.is_array()) {
    // TODO uninitialize previous value
    assert(type_expr->is_array_type() && "Not array type");
    auto len = static_cast<ArrayType *>(type_expr)->length->generate_code();
    static_cast<Array *>(type.get)
        ->initialize_literal(identifier->alloc, len);
  }

  if (decl_type == DeclType::Std || type.get->time() == Time::compile) {
    return nullptr;
  }
  // For the most part, declarations are preallocated at the beginning
  // of each scope, so there's no need to do anything if a heap allocation
  // isn't required.

  // Remember, type_expr is not really the right name in the inference case.
  // It's the thing whose type we are inferring.
  //
  // TODO change the name of this member variable to describe what it actually
  // is in both ':' and ':=" cases
  return generate_assignment_code(identifier, type_expr);
}

// TODO cleanup. Nothing incorrect here that I know of, just can be simplified
llvm::Value *Case::generate_code() {
  auto parent_fn = builder.GetInsertBlock()->getParent();
  // Condition blocks - The ith block is what you reach when you've
  // failed the ith condition, where conditions are labelled starting at zero.
  std::vector<llvm::BasicBlock *> case_blocks(kv->pairs.size() - 1);

  for (auto &block : case_blocks) {
    block = make_block("case.block", parent_fn);
  }

  // Landing blocks
  auto current_block = builder.GetInsertBlock();
  auto case_landing = make_block("case.landing", parent_fn);
  builder.SetInsertPoint(case_landing);
  llvm::PHINode *phi_node = builder.CreatePHI(
      type, static_cast<unsigned int>(kv->pairs.size()), "phi");
  builder.SetInsertPoint(current_block);

  for (size_t i = 0; i < case_blocks.size(); ++i) {
    auto cmp_val    = kv->pairs[i].first->generate_code();
    auto true_block = make_block("land_true", parent_fn);

    // If it's false, move on to the next block
    builder.CreateCondBr(cmp_val, true_block, case_blocks[i]);
    builder.SetInsertPoint(true_block);
    auto output_val = kv->pairs[i].second->generate_code();

    // NOTE: You may be tempted to state that you are coming from the
    // block 'true_block'. However, if the code generated for the right-hand
    // side of the '=>' node is not just a single basic block, this will not
    // be the case.
    phi_node->addIncoming(output_val, builder.GetInsertBlock());
    builder.CreateBr(case_landing);

    builder.SetInsertPoint(case_blocks[i]);
  }
  auto output_val = kv->pairs.back().second->generate_code();

  phi_node->addIncoming(output_val, builder.GetInsertBlock());
  builder.CreateBr(case_landing);
  builder.SetInsertPoint(case_landing);

  return phi_node;
}

llvm::Value *ArrayLiteral::generate_code() {
  // TODO if this is never assigned to anything, it will be leaked. This should
  // be verified.

  auto type_as_array = static_cast<Array *>(type.get);
  auto element_type  = type_as_array->data_type;
  size_t num_elems   = elems.size();

  auto array_data = type.get->allocate();

  type_as_array->initialize_literal(array_data, num_elems);

  auto head_ptr = builder.CreateLoad(builder.CreateGEP(
      array_data, {data::const_uint(0), data::const_uint(1)}));

  // Initialize the literal
  for (size_t i = 0; i < num_elems; ++i) {
    auto data_ptr = builder.CreateGEP(head_ptr, {data::const_uint(i)});
    element_type.get->call_init(data_ptr);

    builder.CreateCall(element_type.get->assign(),
                       {elems[i]->generate_code(), data_ptr});
  }

  assert(scope_->is_block_scope());
  static_cast<BlockScope *>(scope_)->defer_uninit(type, array_data);

  return array_data;
}

llvm::Value *KVPairList::generate_code() { return nullptr; }

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
        builder.CreateLoad(body_scopes[i]->containing_function_->exit_flag);

    assert(body_scopes[i]->parent->is_block_scope());
    auto parent_block_scope = static_cast<BlockScope *>(body_scopes[i]->parent);
    auto is_zero = builder.CreateICmpEQ(exit_flag, data::const_char('\00'));
    builder.CreateCondBr(is_zero, land_block, parent_block_scope->exit);

    Scope::Stack.pop();
  }

  builder.SetInsertPoint(land_block);
  return nullptr;
}

// No code to generate for this, constants added automatically.
llvm::Value *EnumLiteral::generate_code() { return nullptr; }
llvm::Value *StructLiteral::generate_code() { return nullptr; }

llvm::Value *Jump::generate_code() {
  llvm::Value *exit_flag_alloc =
      CurrentScope()->is_function_scope()
          ? static_cast<FnScope *>(CurrentScope())->exit_flag
          : CurrentScope()->containing_function_->exit_flag;

  builder.CreateStore(is_break ? BREAK_FLAG : CONTINUE_FLAG, exit_flag_alloc);
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
      builder.CreateLoad(while_scope->containing_function_->exit_flag);

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

void For::GenerateLoopBodyCode(llvm::Function *parent_fn) {
  auto loop_block = make_block("loop.body", parent_fn);

  Scope::Stack.push(for_scope);
  for_scope->initialize();
  builder.CreateBr(loop_block);

  builder.SetInsertPoint(loop_block);
  statements->generate_code();
  builder.CreateBr(for_scope->exit);
  builder.SetInsertPoint(for_scope->exit);
}

void For::GenerateLoopExitCode(llvm::BasicBlock *reentry) {
  for_scope->uninitialize();

  auto exit_flag =
      builder.CreateLoad(for_scope->containing_function_->exit_flag);

  // Switch. By default go back to the start of the loop.
  auto switch_stmt = builder.CreateSwitch(exit_flag, reentry);
  switch_stmt->addCase(BREAK_FLAG, for_scope->land);
  assert(for_scope->parent->is_block_scope());
  auto parent_block_scope = static_cast<BlockScope *>(for_scope->parent);
  switch_stmt->addCase(RETURN_FLAG, parent_block_scope->exit);

  Scope::Stack.pop();

  builder.SetInsertPoint(for_scope->land);
}

llvm::Value *For::generate_code() {
  auto start_block = builder.GetInsertBlock();

  auto parent_fn = start_block->getParent();
  for_scope->set_parent_function(parent_fn);

  assert(!for_scope->land);

  for_scope->land = make_block("loop.land", parent_fn);

  if (container->type.is_array()) {
    auto container_val = container->generate_code();
    assert(container_val && "container_val is nullptr");

    auto len_ptr = builder.CreateGEP(
        container_val, {data::const_uint(0), data::const_uint(0)}, "len_ptr");
    auto len_val = builder.CreateLoad(len_ptr, "len");

    auto start_ptr = builder.CreateLoad(
        builder.CreateGEP(container_val,
                          {data::const_uint(0), data::const_uint(1)}),
        "start_ptr");
    auto end_ptr = builder.CreateGEP(start_ptr, len_val, "end_ptr");

    auto cond_block = make_block("loop.cond", parent_fn);

    builder.CreateBr(cond_block);
    builder.SetInsertPoint(cond_block);
    auto phi_node = builder.CreatePHI(*Ptr(iterator->type), 2, "phi");
    phi_node->addIncoming(start_ptr, start_block);

    iterator->identifier->alloc = phi_node;

    auto cmp = builder.CreateICmpEQ(phi_node, end_ptr);
    builder.CreateCondBr(cmp, for_scope->land, for_scope->entry);

    GenerateLoopBodyCode(parent_fn);

    auto next = builder.CreateGEP(phi_node, data::const_uint(1));
    phi_node->addIncoming(next, for_scope->exit); // Comes from exit block

    GenerateLoopExitCode(cond_block);

  } else if (container->type.is_range()) {
    bool double_ended_range;

    if (container->is_binop()) {
      assert(static_cast<Binop *>(container)->op == Language::Operator::Dots);
      double_ended_range = true;
    } else if (container->is_unop()) {
      assert(static_cast<Unop *>(container)->op == Language::Operator::Dots);
      double_ended_range = false;
    } else {
      assert(false);
    }

    llvm::Value *start_val =
        double_ended_range
            ? static_cast<Binop *>(container)->lhs->generate_code()
            : static_cast<Unop *>(container)->operand->generate_code();

    llvm::Value *end_val =
        double_ended_range
            ? static_cast<Binop *>(container)->rhs->generate_code()
            : nullptr;

    auto cond_block = make_block("loop.cond", parent_fn);
    builder.CreateBr(cond_block);
    builder.SetInsertPoint(cond_block);

    auto phi_node = builder.CreatePHI(iterator->type, 2, "phi");
    phi_node->addIncoming(start_val, start_block);
    builder.CreateStore(phi_node, iterator->identifier->alloc);

    if (double_ended_range) {
      llvm::Value *cmp = nullptr;
      if (iterator->type == Int) {
        cmp = builder.CreateICmpSGT(phi_node, end_val);
      } else if (iterator->type == Uint || iterator->type == Char) {
        cmp = builder.CreateICmpUGT(phi_node, end_val);
      }

      builder.CreateCondBr(cmp, for_scope->land, for_scope->entry);

    } else {
      builder.CreateBr(for_scope->entry);
    }

    GenerateLoopBodyCode(parent_fn);

    llvm::Value *next = builder.CreateAdd(
        builder.CreateLoad(iterator->identifier->alloc),
        iterator->type == Char ? data::const_char(1) : data::const_uint(1));

    phi_node->addIncoming(next, for_scope->exit); // Comes from exit block
    GenerateLoopExitCode(cond_block);

  } else {
    // TODO that condition should really be encoded into the loop somewhere to
    // make it faster to check.
    auto t = container->evaluate(scope_->context).as_type;

    if (t->is_enum()) {
      // NOTE: We assume that the entries are numbered starting at zero.

      auto enum_type = static_cast<Enumeration *>(t);
      // TODO get them by means other than string name

      auto cond_block = make_block("loop.cond", parent_fn);

      builder.CreateBr(cond_block);
      builder.SetInsertPoint(cond_block);
      auto phi_node = builder.CreatePHI(Uint, 2, "phi");
      phi_node->addIncoming(data::const_uint(0), start_block);
      builder.CreateStore(phi_node, iterator->identifier->alloc);

      auto cmp = builder.CreateICmpEQ(
          phi_node, data::const_uint(enum_type->int_values.size()));
      builder.CreateCondBr(cmp, for_scope->land, for_scope->entry);

      GenerateLoopBodyCode(parent_fn);

      auto next = builder.CreateAdd(
          builder.CreateLoad(iterator->identifier->alloc), data::const_uint(1));
      phi_node->addIncoming(next, for_scope->exit); // Comes from exit block

      GenerateLoopExitCode(cond_block);

    } else {
      assert(false && "Not yet implemented");
    }
  }

  return nullptr;
}

llvm::Value *DummyTypeExpr::generate_code() {
  assert(false);
  return nullptr;
}
} // namespace AST

#undef BREAK_FLAG
#undef CONTINUE_FLAG
#undef RETURN_FLAG
