#include "AST.h"
#include "Context.h"

// This is the only place that needs to know about operators, so rather than
// keep them in the header everywhere, we just put the necessary templates in
// this one header.
#include "Type/ops.h"

extern llvm::BasicBlock* make_block(const std::string& name, llvm::Function* fn);

extern ErrorLog error_log;

extern llvm::Module *global_module;

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
extern llvm::Value *null(Type *t);
extern llvm::Value *const_true();
extern llvm::Value *const_false();
extern llvm::Value *const_uint(size_t n);
extern llvm::Value *const_int(int n);
extern llvm::Value *const_char(char c);
extern llvm::Value *const_real(double d);
extern llvm::Value *global_string(llvm::IRBuilder<> &bldr,
                                  const std::string &s);
} // namespace data

llvm::Value *struct_memcpy(Type *type, llvm::Value *val,
                           llvm::IRBuilder<> &bldr) {
  auto arg_ptr  = bldr.CreateAlloca(*type, nullptr, "struct.tmp");
  auto tmp_raw  = bldr.CreateBitCast(arg_ptr, *RawPtr);
  auto val_raw  = bldr.CreateBitCast(val, *RawPtr);
  auto mem_copy = bldr.CreateCall(
      cstdlib::memcpy(), {tmp_raw, val_raw, data::const_uint(type->bytes())});

  return bldr.CreateBitCast(mem_copy, *Ptr(type));
}

namespace AST {
llvm::Value *Identifier::generate_code(Scope *scope) {
  if (type == Type_) {
    return nullptr;

  } else if (type->is_function()) {
    // TODO better way to get functions based on their name
    return global_module->getFunction(token());

  } else if (type->is_big()) {
    return alloc;

  } else {
    return scope->builder().CreateLoad(alloc, token());
  }
}

// Invariant:
// Only returns nullptr if the expression type is void or a type
llvm::Value *Terminal::generate_code(Scope *scope) {
  // TODO remove dependence on token() altogether
  switch (terminal_type) {
  case Language::Terminal::Type:
  case Language::Terminal::Return: {
    return nullptr;
  }
  case Language::Terminal::Null: {
    assert(type->is_pointer() && "Null pointer of non-pointer type ");
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
    auto str = data::global_string(scope->builder(), token());
    auto len = data::const_uint(token().size());

    auto str_alloc = scope->builder().CreateAlloca(*type);

    // TODO use field_num(). This gets the length
    auto len_ptr = scope->builder().CreateGEP(
        str_alloc, {data::const_uint(0), data::const_uint(1)});
    scope->builder().CreateStore(len, len_ptr);

    // TODO use field_num(). This gets the char array
    auto char_array_ptr = scope->builder().CreateGEP(
        str_alloc, {data::const_uint(0), data::const_uint(0)});

    // NOTE: no need to uninitialize because we never initialized it.
    auto char_ptr = Arr(Char)->initialize_literal(scope->builder(), token().size());

    scope->builder().CreateStore(char_ptr, char_array_ptr);
    scope->builder().CreateCall(cstdlib::memcpy(), {char_ptr, str, len});

    return str_alloc;
  }
  }
}

// Invariant:
// Only returns nullptr if the expression type is void or a type
llvm::Value *Unop::generate_code(Scope *scope) {
  // We first go through all the possible operators where we don't necessarily
  // need to generate code for the operand.
  switch (op) {
  case Language::Operator::And: {
    // TODO ensure that this has an l-value
    return operand->generate_lvalue(scope);
  }
  case Language::Operator::Print: {
    // NOTE: Type punning Type* -> llvm::Value*
    llvm::Value *val = (operand->type == Type_)
                           ? reinterpret_cast<llvm::Value *>(
                                 operand->evaluate(scope->context()).as_type)
                           : operand->generate_code(scope);

    if (operand->type->is_struct()) {
      // TODO maybe callees should be responsible for the struct memcpy?
      val = struct_memcpy(operand->type, val, scope->builder());
    }

    operand->type->call_print(scope->builder(), val);
    return nullptr;
  }
  default:;
  }

  llvm::Value *val        = operand->generate_code(scope);
  llvm::IRBuilder<> &bldr = scope->builder();
  switch (op) {
  case Language::Operator::Sub: {
    return operand->type->call_neg(bldr, val);
  }
  case Language::Operator::Not: {
    return operand->type->call_not(bldr, val);
  }
  case Language::Operator::Free: {
    bldr.CreateCall(cstdlib::free(), {bldr.CreateBitCast(val, *RawPtr)});
    // TODO only if it has an l-value
    bldr.CreateStore(data::null(operand->type),
                     operand->generate_lvalue(scope));
    return nullptr;
  }
  case Language::Operator::Return: {
    scope->make_return(val);
    return nullptr;
  }
  case Language::Operator::At: {
    return type->is_big() ? val : bldr.CreateLoad(val);
  }
  case Language::Operator::Call: {
    assert(operand->type->is_function() && "Operand should be a function.");
    auto out_type = static_cast<Function *>(operand->type)->output;
    // TODO this whole section needs an overhaul when we totally settle on how
    // to pass large things.
    if (out_type->is_struct()) {
      // TODO move this outside of any potential loops
      auto local_ret = scope->builder().CreateAlloca(*out_type);

      scope->builder().CreateCall(static_cast<llvm::Function *>(val),
                                  local_ret);
      return local_ret;

    } else {
      return scope->builder().CreateCall(static_cast<llvm::Function *>(val));
    }
  }
  default: assert(false && "Unimplemented unary operator codegen");
  }
}

llvm::Value *Access::generate_code(Scope *scope) {
  if (operand->type == Type_) {
    // As of 2/26/16, the only way an operand could be a type is if it's an
    // enum, as in `Color.Red`. Here Color is an enum with the access
    // parameter Red.
    //
    // NOTE: this will likely change if we implement UFCS. In that case, this
    // whole method will probably be out of date.
    auto expr_as_type = operand->evaluate(scope->context()).as_type;
    assert(expr_as_type->is_enum() && "Expression should be an enum");
    return static_cast<Enumeration *>(expr_as_type)->get_value(member_name);
  }

  // Generate the code for the operand
  auto eval = operand->generate_code(scope);

  // To make access pass through all layers of pointers, we loop through
  // loading values while we're looking at pointers.
  auto base_type = operand->type;
  while (base_type->is_pointer()) {
    base_type = static_cast<Pointer *>(base_type)->pointee;
    if (!base_type->is_big()) eval = scope->builder().CreateLoad(eval);
  }

  if (base_type->is_struct()) {
    auto struct_type = static_cast<Structure *>(base_type);

    if (!type->stores_data()) {
      assert(false && "Not yet implemented");
    }

    auto elem_ptr = scope->builder().CreateGEP(
        eval, {data::const_uint(0), struct_type->field_num(member_name)});
    return type->is_big() ? elem_ptr : scope->builder().CreateLoad(elem_ptr);

  } else {
    assert(false && "Not yet implemented");
  }
}

llvm::Value *Binop::generate_code(Scope *scope) {
  if (time() == Time::compile) {
    return llvm_value(evaluate(scope->context()));
  }

  auto lhs_val = lhs->generate_code(scope);
  llvm::IRBuilder<> &bldr = scope->builder();

  switch (op) {
  case Language::Operator::Index: {
    if (lhs->type->is_array()) {
      auto data_ptr = scope->builder().CreateLoad(scope->builder().CreateGEP(
          lhs_val, {data::const_uint(0), data::const_uint(1)}));
      if (type->is_big()) {
        return scope->builder().CreateGEP(data_ptr, {rhs->generate_code(scope)},
                                          "array_val");
      } else {
        return scope->builder().CreateLoad(scope->builder().CreateGEP(
            data_ptr, {rhs->generate_code(scope)}, "array_val"));
      }
    }
    assert(false && "Not yet implemented");
  }
  case Language::Operator::Cast: {
    return lhs->type->call_cast(bldr, lhs_val,
                                rhs->evaluate(scope->context()).as_type);
  }
  case Language::Operator::Call: {
    if (lhs->type->is_function()) {
      std::vector<llvm::Value *> arg_vals;
      // This whole section should be pulled out into a function called
      // "collate_for_function_call" or something like that.
      if (rhs->is_comma_list()) {
        auto &arg_exprs = static_cast<ChainOp *>(rhs)->exprs;
        arg_vals.resize(arg_exprs.size(), nullptr);
        for (size_t i = 0; i < arg_exprs.size(); ++i) {
          arg_vals[i] = arg_exprs[i]->generate_code(scope);
          assert(arg_vals[i] && "Argument value was null");

          if (arg_exprs[i]->type->is_struct()) {
            // TODO be sure to allocate this ahead of all loops and reuse it
            // when possible. Also, do this for arrays?
            arg_vals[i] = struct_memcpy(arg_exprs[i]->type, arg_vals[i],
                                        scope->builder());
          }
        }
      } else {
        auto rhs_val = rhs->generate_code(scope);
        assert(rhs_val && "Argument value was null");

        if (rhs->type->is_struct()) {
          // TODO be sure to allocate this ahead of all loops and reuse it
          // when possible. Also, do this for arrays?
          rhs_val = struct_memcpy(rhs->type, rhs_val, scope->builder());
        }

        arg_vals = {rhs_val};
      }

      if (type == Void) {
        scope->builder().CreateCall(static_cast<llvm::Function *>(lhs_val),
                                    arg_vals);
        return nullptr;

      } else {
        return scope->builder().CreateCall(
            static_cast<llvm::Function *>(lhs_val), arg_vals, "calltmp");
      }

    } else if (lhs->type->is_dependent_type()) {
      // TODO make this generic. right now dependent_type implies alloc(...)
      auto t         = rhs->evaluate(scope->context()).as_type;
      auto alloc_ptr = bldr.CreateCall(lhs_val, {data::const_uint(t->bytes())});
      return bldr.CreateBitCast(alloc_ptr, *type);
    }
  }
  default:;
  }

  auto rhs_val = rhs->generate_code(scope);
  switch (op) {
  case Language::Operator::Add: return type->call_add(bldr, lhs_val, rhs_val);
  case Language::Operator::Sub: return type->call_sub(bldr, lhs_val, rhs_val);
  case Language::Operator::Mul: return type->call_mul(bldr, lhs_val, rhs_val);
  case Language::Operator::Div: return type->call_div(bldr, lhs_val, rhs_val);
  case Language::Operator::Mod: return type->call_mod(bldr, lhs_val, rhs_val);
  default:;
  }

  assert(false && "Reached end of Binop::generate_code");
}

// TODO rename ArrayType as ShorthandArray. These represent the type of an array
// as well as a shorthand array. During code-gen, it's treated as the latter,
// because we never need to code-gen types.
llvm::Value *ArrayType::generate_code(Scope *scope) {
  // TODO This doesn't work if len is a chain of lengths.
  auto len                = length->generate_code(scope);
  auto data_ty            = data_type->type;
  auto data               = data_type->generate_code(scope);
  llvm::IRBuilder<> &bldr = scope->builder();

  auto alloc_size = bldr.CreateMul(len, data::const_uint(data_ty->bytes()));
  auto alloc_ptr = bldr.CreateCall(cstdlib::malloc(), alloc_size);

  auto tmp_array = bldr.CreateAlloca(*type, nullptr, "array.tmp");
  bldr.CreateStore(len, bldr.CreateGEP(tmp_array, {data::const_uint(0),
                                                    data::const_uint(0)}));
  bldr.CreateStore(alloc_ptr, bldr.CreateGEP(tmp_array, {data::const_uint(0),
                                                         data::const_uint(1)}));

  auto end_ptr   = bldr.CreateGEP(alloc_ptr, len);

  auto prev_block = bldr.GetInsertBlock();
  auto parent_fn  = bldr.GetInsertBlock()->getParent();

  auto loop_block = make_block("loop.block", parent_fn);
  auto loop_end   = make_block("loop.end", parent_fn);

  bldr.CreateBr(loop_block);
  bldr.SetInsertPoint(loop_block);
  auto phi_node = bldr.CreatePHI(*Ptr(data_ty), 2, "phi");
  phi_node->addIncoming(alloc_ptr, prev_block);
  bldr.CreateCall(data_ty->assign(), {data, phi_node});

  auto next_ptr = bldr.CreateGEP(phi_node, data::const_uint(1));
  phi_node->addIncoming(next_ptr, loop_block);

  bldr.CreateCondBr(bldr.CreateICmpEQ(next_ptr, end_ptr), loop_end, loop_block);
  bldr.SetInsertPoint(loop_end);

  // TODO If you never assign this, the allocation is leaked. It should be
  // verified before code-gen that this is leaked
  return tmp_array;
}

llvm::Value *Statements::generate_code(Scope *scope) {
  for (auto &stmt : statements) stmt->generate_code(scope);
  return nullptr;
}

#define BEGIN_SHORT_CIRCUIT                                                    \
  bldr.SetInsertPoint(curr_block);                                             \
  auto lhs_val = exprs[0]->generate_code(scope);                               \
  for (size_t i = 1; i < exprs.size(); ++i) {                                  \
    bldr.SetInsertPoint(curr_block);                                           \
    auto rhs_val = exprs[i]->generate_code(scope);                             \
                                                                               \
    llvm::Value *cmp_val = nullptr;                                            \
    switch (ops[i - 1]) {

#define CASE(cmp, llvm_call, op_name)                                          \
  case Operator::op_name: {                                                    \
    cmp = bldr.Create##llvm_call##op_name(lhs_val, rhs_val, #op_name "tmp");   \
  } break

#define END_SHORT_CIRCUIT                                                      \
  default: assert(false && "Invalid operator");                                \
    }                                                                          \
    assert(cmp_val && "cmp_val is nullptr");                                   \
                                                                               \
    auto next_block = make_block("next", parent_fn);                           \
    bldr.CreateCondBr(cmp_val, next_block, landing);                           \
    phi->addIncoming(data::const_false(), curr_block);                         \
    curr_block = next_block;                                                   \
    lhs_val    = rhs_val;                                                      \
    }                                                                          \
                                                                               \
    bldr.SetInsertPoint(curr_block);                                           \
    phi->addIncoming(data::const_true(), curr_block);                          \
    bldr.CreateBr(landing);                                                    \
    bldr.SetInsertPoint(landing);                                              \
    return phi;


llvm::Value *ChainOp::generate_code(Scope *scope) {
  // TODO eval of enums at compile-time is wrong. This could be
  // 1. That the eval function is wrong, or
  // 2. That they shouldn't be determined at compile-time
  if (time() == Time::compile) {
    return llvm_value(evaluate(scope->context()));
  }

  using Language::Operator;

  auto expr_type = exprs[0]->type;
  auto &bldr     = scope->builder();

  // Boolean xor is separate because it can't be short-circuited
  if (expr_type == Bool && ops.front() == Operator::Xor) {
    llvm::Value *cmp_val = exprs[0]->generate_code(scope);
    for (size_t i = 1; i < exprs.size(); ++i) {
      cmp_val = bldr.CreateXor(cmp_val, exprs[i]->generate_code(scope));
    }
    return cmp_val;
  }

  auto parent_fn  = bldr.GetInsertBlock()->getParent();
  auto landing    = make_block("land", parent_fn);
  auto curr_block = bldr.GetInsertBlock();

  // Count the number of incoming branches into the phi node. This is equal to
  // the number of exprs, unless it's & or |. In those instances, it is one more
  // than the number of expressions.
  auto num_incoming = static_cast<unsigned int>(exprs.size());
  if (expr_type == Bool) ++num_incoming;

  // Create the phi node
  bldr.SetInsertPoint(landing);
  llvm::PHINode *phi = bldr.CreatePHI(*Bool, num_incoming, "phi");
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

  } else if (expr_type == Real) {
    BEGIN_SHORT_CIRCUIT
    CASE(cmp_val, FCmpO, LT);
    CASE(cmp_val, FCmpO, LE);
    CASE(cmp_val, FCmpO, EQ);
    CASE(cmp_val, FCmpO, NE);
    CASE(cmp_val, FCmpO, GE);
    CASE(cmp_val, FCmpO, GT);
    END_SHORT_CIRCUIT

  } else if (expr_type->is_enum()) {
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
      for (const auto& ex : exprs) {
        bldr.SetInsertPoint(curr_block);
        auto next_block = make_block("next", parent_fn);
        bldr.CreateCondBr(ex->generate_code(scope), next_block, landing);
        phi->addIncoming(data::const_false(), bldr.GetInsertBlock());
        curr_block = next_block;
      }

      bldr.SetInsertPoint(curr_block);
      phi->addIncoming(data::const_true(), curr_block);

    } else if (ops.front() == Operator::Or) {
      for (const auto& ex : exprs) {
        bldr.SetInsertPoint(curr_block);
        auto next_block = make_block("next", parent_fn);
        bldr.CreateCondBr(ex->generate_code(scope), landing, next_block);
        phi->addIncoming(data::const_true(), bldr.GetInsertBlock());
        curr_block = next_block;
      }

      bldr.SetInsertPoint(curr_block);
      phi->addIncoming(data::const_false(), curr_block);

    } else {
      assert(false && "invalid operand in short-circuiting");
    }

    bldr.CreateBr(landing);
    bldr.SetInsertPoint(landing);
    return phi;

  } else {
    assert(false && "Invalid type in ChainOp::generate_code");
  }
}

#undef BEGIN_SHORT_CIRCUIT
#undef CASE
#undef END_SHORT_CIRCUIT

llvm::Value *FunctionLiteral::generate_code(Scope *scope) {
  if (llvm_fn == nullptr) {
    // NOTE: This means a function is not assigned, but has been declared.
    llvm_fn = llvm::Function::Create(
        static_cast<llvm::FunctionType *>(type->llvm_type),
        llvm::Function::ExternalLinkage, "__anon_fn", global_module);
  }

  // Name the inputs
  auto arg_iter = llvm_fn->args().begin();
  for (const auto &input_iter : inputs) {
    arg_iter->setName(input_iter->identifier->token());
    // Set alloc
    auto decl_id   = input_iter->identifier;
    auto decl_type = decl_id->type;
    if (decl_type->is_big()) { decl_id->alloc = arg_iter; }

    ++arg_iter;
  }

  auto ret_type = return_type_expr->evaluate(scope->context()).as_type;
  if (ret_type->is_struct()) { arg_iter->setName("retval"); }

  auto old_block = scope->builder().GetInsertBlock();

  fn_scope->set_parent_function(llvm_fn);
  fn_scope->set_type(static_cast<Function *>(type));

  fn_scope->enter();
  auto arg = llvm_fn->args().begin();
  for (auto &input_iter : inputs) {
    auto decl_id = input_iter->identifier;

    if (!decl_id->type->is_struct()) {
      fn_scope->builder().CreateCall(decl_id->type->assign(),
                                     {arg, input_iter->identifier->alloc});
    }
    ++arg;
  }

  statements->generate_code(fn_scope);

  fn_scope->exit();

  scope->builder().SetInsertPoint(old_block);
  return llvm_fn;
}

// This function exists because both '=' and ':=' need to call some version of
// the same code. it's been factored out here.
llvm::Value *generate_assignment_code(Scope *scope, Expression *lhs,
                                      Expression *rhs) {
  llvm::Value *var = nullptr;
  llvm::Value *val = nullptr;

  // Treat functions special
  if (lhs->is_identifier() && rhs->type->is_function()) {
    if (lhs->token() == "__print__" || lhs->token() == "__assign__") {
      val = rhs->generate_code(scope);
      assert(val && "RHS of assignment generated null code");

      // TODO verify that you are defining print for this struct type.
      auto rhs_as_func = static_cast<Function *>(rhs->type);
      auto arg_type    = static_cast<Structure *>(rhs_as_func->input);

      arg_type->set_print(static_cast<llvm::Function *>(val));

    } else {
      auto fn = static_cast<FunctionLiteral *>(rhs);
      // TODO TOKENREMOVAL. Get the function via some unique name (probably
      // mangled somehow)
      fn->llvm_fn = global_module->getFunction(lhs->token());

      val = rhs->generate_code(scope);
      assert(val && "RHS of assignment generated null code");
      val->setName(lhs->token());
    }
  } else {
    var = lhs->generate_lvalue(scope);
    assert(var && "LHS of assignment generated null code");

    val = rhs->generate_code(scope);
    assert(val && "RHS of assignment generated null code");

    lhs->type->call_uninit(scope->builder(), var);

    scope->builder().CreateCall(lhs->type->assign(), {val, var});
  }

  return nullptr;
}

#define CASE(op, llvm_op, symbol)                                              \
  case Operator::op: {                                                         \
    val = scope->builder().Create##llvm_op(lhs_val, rhs_val, symbol);          \
  } break

llvm::Value *Assignment::generate_code(Scope *scope) {
  // The left-hand side may be a declaration
  if (lhs->is_declaration()) {
    // TODO maybe the declarations generate_code ought to return an l-value for
    // the thing it declares?
    return generate_assignment_code(
        scope, static_cast<Declaration *>(lhs)->identifier, rhs);
  }


  using Language::Operator;
  if (op == Operator::OrEq || op == Operator::XorEq || op == Operator::AndEq ||
      op == Operator::AddEq || op == Operator::SubEq || op == Operator::MulEq ||
      op == Operator::DivEq || op == Operator::ModEq) {

    auto lhs_val = lhs->generate_code(scope);
    assert(lhs_val && "LHS of assignment generated null code");

    auto lval = lhs->generate_lvalue(scope);
    assert(lval && "LHS lval of assignment generated null code");

    if (lhs->type == Bool) {
      switch (op) {
      case Operator::XorEq: {
        auto rhs_val = rhs->generate_code(scope);
        assert(rhs_val && "RHS of assignment generated null code");

        scope->builder().CreateStore(
            scope->builder().CreateXor(lhs_val, rhs_val, "xortmp"), lval);
      } break;
      case Operator::AndEq: {
        auto parent_fn   = scope->builder().GetInsertBlock()->getParent();
        auto more_block  = make_block("more", parent_fn);
        auto merge_block = make_block("merge", parent_fn);
        scope->builder().CreateCondBr(lhs_val, more_block, merge_block);

        scope->builder().SetInsertPoint(more_block);
        auto rhs_val = rhs->generate_code(scope);
        assert(rhs_val && "RHS of assignment generated null code");

        scope->builder().CreateStore(rhs_val, lval);
        scope->builder().CreateBr(merge_block);
        scope->builder().SetInsertPoint(merge_block);
      } break;
      case Operator::OrEq: {
        auto parent_fn   = scope->builder().GetInsertBlock()->getParent();
        auto more_block  = make_block("more", parent_fn);
        auto merge_block = make_block("merge", parent_fn);
        scope->builder().CreateCondBr(lhs_val, merge_block, more_block);

        scope->builder().SetInsertPoint(more_block);
        auto rhs_val = rhs->generate_code(scope);
        assert(rhs_val && "RHS of assignment generated null code");

        scope->builder().CreateStore(rhs_val, lval);
        scope->builder().CreateBr(merge_block);
        scope->builder().SetInsertPoint(merge_block);
      } break;
      default: assert(false && "Invalid assignment operator for boolean type");
      }
      return nullptr;
    }

    auto rhs_val = rhs->generate_code(scope);
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

    scope->builder().CreateStore(val, lval);
    return nullptr;
  }

  return generate_assignment_code(scope, lhs, rhs);
}
#undef CASE

llvm::Value *Declaration::generate_code(Scope *scope) {
  if (!is_inferred || type == Type_) return nullptr;
  // For the most part, declarations are preallocated at the beginning
  // of each scope, so there's no need to do anything if a heap allocation
  // isn't required.

  // Remember, type_expr is not really the right name in the inference case.
  // It's the thing whose type we are inferring.
  //
  // TODO change the name of this member variable to describe what it actually
  // is in both ':' and ':=" cases
  return generate_assignment_code(scope, identifier, type_expr);
}

// TODO cleanup. Nothing incorrect here that I know of, just can be simplified
llvm::Value *Case::generate_code(Scope *scope) {
  auto parent_fn = scope->builder().GetInsertBlock()->getParent();
  // Condition blocks - The ith block is what you reach when you've
  // failed the ith condition, where conditions are labelled starting at zero.
  std::vector<llvm::BasicBlock *> case_blocks(kv->pairs.size() - 1);

  for (auto &block : case_blocks) {
    block = make_block("case.block", parent_fn);
  }

  // Landing blocks
  auto current_block = scope->builder().GetInsertBlock();
  auto case_landing = make_block("case.landing", parent_fn);
  scope->builder().SetInsertPoint(case_landing);
  llvm::PHINode *phi_node = scope->builder().CreatePHI(
      *type, static_cast<unsigned int>(kv->pairs.size()), "phi");
  scope->builder().SetInsertPoint(current_block);

  for (size_t i = 0; i < case_blocks.size(); ++i) {
    auto cmp_val    = kv->pairs[i].first->generate_code(scope);
    auto true_block = make_block("land_true", parent_fn);

    // If it's false, move on to the next block
    scope->builder().CreateCondBr(cmp_val, true_block, case_blocks[i]);
    scope->builder().SetInsertPoint(true_block);
    auto output_val = kv->pairs[i].second->generate_code(scope);

    // NOTE: You may be tempted to state that you are coming from the
    // block 'true_block'. However, if the code generated for the right-hand
    // side of the '=>' node is not just a single basic block, this will not
    // be the case.
    phi_node->addIncoming(output_val, scope->builder().GetInsertBlock());
    scope->builder().CreateBr(case_landing);

    scope->builder().SetInsertPoint(case_blocks[i]);
  }
  auto output_val = kv->pairs.back().second->generate_code(scope);

  phi_node->addIncoming(output_val, scope->builder().GetInsertBlock());
  scope->builder().CreateBr(case_landing);
  scope->builder().SetInsertPoint(case_landing);

  return phi_node;
}

llvm::Value *ArrayLiteral::generate_code(Scope *scope) {
  // TODO if this is never assigned to anything, it will be leaked. This should
  // be verified.

  auto type_as_array = static_cast<Array *>(type);
  auto element_type  = type_as_array->data_type;
  size_t num_elems   = elems.size();

  auto array_data =
      type_as_array->initialize_literal(scope->builder(), num_elems);
  auto head_ptr = scope->builder().CreateLoad(scope->builder().CreateGEP(
      array_data, {data::const_uint(0), data::const_uint(1)}));

  if (element_type->is_big()) {
    assert(false && "Not yet implemented");
  } else {
    for (size_t i = 0; i < num_elems; ++i) {
      auto data_ptr =
          scope->builder().CreateGEP(head_ptr, {data::const_uint(i)});

      scope->builder().CreateCall(element_type->assign(),
                                  {elems[i]->generate_code(scope), data_ptr});
    }

  }

  return array_data;
}

llvm::Value *KVPairList::generate_code(Scope *) { return nullptr; }

llvm::Value *While::generate_code(Scope *scope) {
  auto parent_fn = scope->builder().GetInsertBlock()->getParent();

  auto while_stmt_block = make_block("while.stmt", parent_fn);

  while_scope->set_parent_function(parent_fn);

  scope->builder().CreateBr(while_scope->entry_block());
  while_scope->enter();
  auto cond = condition->generate_code(while_scope);
  while_scope->builder().CreateCondBr(cond, while_stmt_block,
                                      while_scope->landing_block());

  while_scope->builder().SetInsertPoint(while_stmt_block);
  statements->generate_code(while_scope);
  while_scope->exit();
  scope->builder().SetInsertPoint(while_scope->landing_block());

  return nullptr;
}

llvm::Value *Conditional::generate_code(Scope *scope) {
  auto parent_fn = scope->builder().GetInsertBlock()->getParent();

  // Last block is either the else-block or the landing block if
  // no else-block exists.
  std::vector<llvm::BasicBlock *> conditionblocks(conditions.size() + 1,
                                                  nullptr);

  for (size_t i = 0; i < conditionblocks.size(); ++i) {
    conditionblocks[i] = make_block("cond.block", parent_fn);
  }

  llvm::BasicBlock *landing =
      has_else() ? make_block("land", parent_fn) : conditionblocks.back();

  scope->builder().CreateBr(conditionblocks[0]);

  for (size_t i = 0; i < conditions.size(); ++i) {
    scope->builder().SetInsertPoint(conditionblocks[i]);
    auto condition = conditions[i]->generate_code(scope);
    scope->builder().CreateCondBr(condition, body_scopes[i]->entry_block(),
                                  conditionblocks[i + 1]);
  }

  scope->builder().SetInsertPoint(conditionblocks.back());
  if (has_else()) {
    scope->builder().CreateBr(body_scopes.back()->entry_block());
  }

  for (size_t i = 0; i < statements.size(); ++i) {
    body_scopes[i]->set_parent_function(parent_fn);
    body_scopes[i]->enter();
    statements[i]->generate_code(body_scopes[i]);
    body_scopes[i]->exit();
    body_scopes[i]->builder().CreateBr(landing);
  }

  scope->builder().SetInsertPoint(landing);

  return nullptr;
}

// No code to generate for this, constants added automatically.
llvm::Value *EnumLiteral::generate_code(Scope *scope) { return nullptr; }
llvm::Value *TypeLiteral::generate_code(Scope *scope) { return nullptr; }

llvm::Value *Break::generate_code(Scope *scope) {
  auto scope_ptr = scope;

  auto prev_insert = scope->builder().GetInsertBlock();

  auto parent_fn                  = scope->builder().GetInsertBlock()->getParent();
  llvm::BasicBlock *dealloc_block = make_block("dealloc.block", parent_fn);
  scope->builder().CreateBr(dealloc_block);

  while (!scope_ptr->is_loop_scope()) {
    auto prev_block = scope_ptr->builder().GetInsertBlock();
    scope_ptr->builder().SetInsertPoint(dealloc_block);
    scope_ptr->uninitialize();
    scope_ptr->builder().SetInsertPoint(prev_block);

    // Go to parent block
    scope_ptr = scope_ptr->parent();
    if (scope_ptr == nullptr) break;
    if (scope_ptr->is_function_scope()) break;
  }

  if (scope_ptr == nullptr || scope_ptr->is_function_scope()) {
    error_log.log(line_num,
                  "A `break` command was encountered outside of a loop.");

  } else {
    auto while_scope = static_cast<WhileScope *>(scope_ptr);
    // TODO if this is in another scope, break up out of those too.
    // For example, a conditional inside a loop.
    scope->builder().SetInsertPoint(dealloc_block);
    auto while_scope_insert = while_scope->builder().GetInsertBlock();
    while_scope->builder().SetInsertPoint(dealloc_block);
    while_scope->uninitialize();
    while_scope->builder().SetInsertPoint(while_scope_insert);
    scope->builder().CreateBr(while_scope->landing_block());
    scope->builder().SetInsertPoint(prev_insert);
  }

  return nullptr;
}

} // namespace AST
