#include "AST.h"
#include "Context.h"

// This is the only place that needs to know about operators, so rather than
// keep them in the header everywhere, we just put the necessary templates in
// this one header.
#include "Type/ops.h"

// TODO
// We often have if (val == nullptr) return nullptr to propogate nullptrs. In
// what situations is a nullptr actually possible to start with? If we can do
// all checks on the AST before code generation, then maybe we can remove these
// and thereby streamline the architecture.

extern llvm::BasicBlock* make_block(const std::string& name, llvm::Function* fn);

extern ErrorLog error_log;

extern llvm::Module* global_module;

namespace cstdlib {
  extern llvm::Constant* free();
  extern llvm::Constant* memcpy();
  extern llvm::Constant* malloc();
  extern llvm::Constant* printf();

}  // namespace cstdlib

namespace builtin {
  extern llvm::Function* ascii();
}  // namespace builtin

namespace data {
  extern llvm::Value* null_pointer(Type* t);
  extern llvm::Value* const_true();
  extern llvm::Value* const_false();
  extern llvm::Value* const_uint(size_t n);
  extern llvm::Value* const_int(int n);
  extern llvm::Value* const_char(char c);
  extern llvm::Value* const_real(double d);
  extern llvm::Value* global_string(llvm::IRBuilder<>& bldr, const std::string& s);
}  // namespace data

llvm::Value* struct_memcpy(Type* type, llvm::Value* val, llvm::IRBuilder<>& bldr) {
  auto arg_ptr = bldr.CreateAlloca(*type, nullptr, "struct.tmp");
  auto tmp_raw = bldr.CreateBitCast(arg_ptr, *RawPtr);
  auto val_raw = bldr.CreateBitCast(val, *RawPtr);
  auto mem_copy = bldr.CreateCall(cstdlib::memcpy(),
      { tmp_raw, val_raw, data::const_uint(type->bytes()) });

  return bldr.CreateBitCast(mem_copy, *Ptr(type));
}

namespace AST {
  llvm::Value* Identifier::generate_code(Scope* scope) {
    if (type == Type_) {
      return nullptr;
    } else if (type->is_function()) {
      return global_module->getFunction(token());

    } else if (type->is_struct()) {
      return alloc;

    } else {
      return scope->builder().CreateLoad(alloc, token());
    }
  }

  // Invariant:
  // Only returns nullptr if the expression type is void or a type
  llvm::Value *Terminal::generate_code(Scope *scope) {
    // TODO remove dependence on token() altogether

    using Language::Terminal;
    switch (terminal_type) {
    case Terminal::Null:
      // null_pointer() automatically adds Ptr() so we need to remove it here
      // TODO is there a better API for this? Almost certainly yes.
      assert(type->is_pointer() && "Null pointer of non-pointer type ");
      return data::null_pointer(static_cast<Pointer *>(type)->pointee);
    case Terminal::ASCII:
      return builtin::ascii();
    case Terminal::True:
      return data::const_true();
    case Terminal::False:
      return data::const_false();
    case Terminal::Else:
      // Because in the case where else represents a terminal, it's
      // value is true
      return data::const_true();
    case Terminal::Char:
      return data::const_char(token()[0]);
    case Terminal::Int:
      return data::const_int(std::stoi(token()));
    case Terminal::Real:
      return data::const_real(std::stod(token()));
    case Terminal::UInt:
      return data::const_uint(std::stoul(token()));
    case Terminal::Alloc:
      return cstdlib::malloc();
    case Terminal::StringLiteral: {
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
      auto char_ptr = Arr(Char)->initialize_literal(scope->builder(), len);

      scope->builder().CreateStore(char_ptr, char_array_ptr);
      scope->builder().CreateCall(cstdlib::memcpy(), {char_ptr, str, len});

      return str_alloc;
    }
    case Terminal::Type:
      return nullptr;
    case Terminal::Return:
      return nullptr;
    }
  }

  // Invariant:
  // Only returns nullptr if the expression type is void or a type
  llvm::Value *Unop::generate_code(Scope *scope) {
    using Language::Operator;
    // Cases where we don't want to generate code for the node
    if (op == Operator::And)
      return operand->generate_lvalue(scope);
    if (op == Operator::Print && operand->type == Type_) {
      // NOTE: BE VERY CAREFUL HERE. YOU ARE TYPE PUNNING!
      // TODO use corrent scope

      auto val = reinterpret_cast<llvm::Value *>(
          operand->evaluate(scope->context()).as_type);
      operand->type->call_print(scope->builder(), val);
      return nullptr;
    }

    llvm::Value *val = operand->generate_code(scope);
    llvm::IRBuilder<> &bldr = scope->builder();
    switch (op) {
    case Operator::Sub:
      return operand->type->call_neg(bldr, val);

    case Operator::Not:
      return operand->type->call_not(bldr, val);
    case Operator::Free: {
      bldr.CreateCall(cstdlib::free(), {bldr.CreateBitCast(val, *RawPtr)});
      // Reset pointer to null
      auto ptee_type = static_cast<Pointer *>(operand->type)->pointee;
      bldr.CreateStore(data::null_pointer(ptee_type),
                       operand->generate_lvalue(scope));
      return nullptr;
    }
    case Operator::Return:
      scope->make_return(val);
      return nullptr;

    case Operator::At:
      if (type->is_struct()) {
        return val;
      } else {
        return bldr.CreateLoad(bldr.CreateGEP(val, {data::const_uint(0)}));
      }

    case Operator::Call: {
      auto fn_type = static_cast<Function *>(operand->type);
      if (fn_type->output->is_struct()) {
        // TODO move this outside of any potential loops
        auto local_ret = scope->builder().CreateAlloca(*fn_type->output);

        scope->builder().CreateCall(static_cast<llvm::Function *>(val),
                                    {local_ret});
        return local_ret;

      } else {
        return scope->builder().CreateCall(static_cast<llvm::Function *>(val));
      }
    }
    case Operator::Print:
      operand->type->call_print(
          scope->builder(),
          operand->type->is_struct()
              ? struct_memcpy(operand->type, val, scope->builder())
              : val);
      return nullptr;

    default:
      return nullptr;
    }
  }

  llvm::Value *Access::generate_code(Scope *scope) {
    auto etype = operand->type;
    if (etype == Type_) {
      auto expr_as_type = operand->evaluate(scope->context()).as_type;
      if (expr_as_type->is_enum()) {
        auto enum_type = static_cast<Enumeration *>(expr_as_type);
        return enum_type->get_value(member_name);
      }
    }

    auto eval = operand->generate_code(scope);
    while (etype->is_pointer()) {
      etype = static_cast<Pointer *>(etype)->pointee;
      if (!etype->is_struct()) {
        eval = scope->builder().CreateLoad(eval);
      }
    }

    auto struct_type = static_cast<Structure *>(etype);

    auto retval = scope->builder().CreateGEP(
        eval, {data::const_uint(0), struct_type->field_num(member_name)});
    return (type->is_struct()) ? retval : scope->builder().CreateLoad(retval);
  }

  llvm::Value *Binop::generate_code(Scope *scope) {
    if (time() == Time::compile) {
      return llvm_value(evaluate(scope->context()));
    }

    using Language::Operator;
    if (op == Operator::Index) {
      return scope->builder().CreateLoad(generate_lvalue(scope), "array_val");
    }

    auto lhs_val = lhs->generate_code(scope);
    if (lhs_val == nullptr)
      return nullptr;

    switch (op) {
    case Operator::Cast:
      return lhs->type->call_cast(scope->builder(), lhs_val,
                                  rhs->evaluate(scope->context()).as_type);

    case Operator::Call:
      if (lhs->type->is_function()) {
        std::vector<llvm::Value *> arg_vals;
        if (rhs->is_comma_list()) {
          auto arg_chainop = std::static_pointer_cast<ChainOp>(rhs);
          arg_vals.resize(arg_chainop->exprs.size(), nullptr);
          size_t i = 0;
          for (const auto &expr : arg_chainop->exprs) {
            arg_vals[i] = expr->generate_code(scope);
            if (arg_vals[i] == nullptr)
              return nullptr;

            if (expr->type->is_struct()) {
              arg_vals[i] =
                  struct_memcpy(expr->type, arg_vals[i], scope->builder());
            }

            ++i;
          }

        } else {
          auto rhs_val = rhs->generate_code(scope);
          if (rhs_val == nullptr)
            return nullptr;

          if (rhs->type->is_struct()) {
            // TODO be sure to allocate this ahead of all loops and reuse it
            // when possible
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
        // TODO make this generic. right now dependent_type implise alloc(...)
        auto t = rhs->evaluate(scope->context()).as_type;
        auto alloc_ptr = scope->builder().CreateCall(
            lhs_val, {data::const_uint(t->bytes())});
        return scope->builder().CreateBitCast(alloc_ptr, *type);
      }
    default:;
    }

    auto rhs_val = rhs->generate_code(scope);
    if (rhs_val == nullptr)
      return nullptr;

    llvm::IRBuilder<>& bldr = scope->builder();
    switch (op) {
      case Operator::Add: return type->call_add(bldr, lhs_val, rhs_val);
      case Operator::Sub: return type->call_sub(bldr, lhs_val, rhs_val);
      case Operator::Mul: return type->call_mul(bldr, lhs_val, rhs_val);
      case Operator::Div: return type->call_div(bldr, lhs_val, rhs_val);
      case Operator::Mod: return type->call_mod(bldr, lhs_val, rhs_val);
      default:;
    }

    return nullptr;
  }

  // If you do generate the code here, it is a shorthand array literal
  llvm::Value* ArrayType::generate_code(Scope* scope) {
    // TODO arrays can only take primitive types currently.
    auto len = length->generate_code(scope);
    auto data_ty = data_type->type;
    auto data = data_type->generate_code(scope);
    llvm::IRBuilder<>& bldr = scope->builder();

    auto alloc_size = bldr.CreateAdd(data::const_uint(Uint->bytes()),
        bldr.CreateMul(len, data::const_uint(data_ty->bytes())));
    auto alloc_ptr = bldr.CreateCall(cstdlib::malloc(), { alloc_size });
    bldr.CreateStore(len, bldr.CreateBitCast(alloc_ptr, *Ptr(Uint)));

    auto start_ptr = bldr.CreateBitCast(
        bldr.CreateGEP(alloc_ptr, { data::const_uint(Uint->bytes()) }), *Ptr(data_ty), "startptr");
    auto end_ptr = bldr.CreateGEP(start_ptr, { len }); 

    auto prev_block = bldr.GetInsertBlock();
    auto parent_fn = bldr.GetInsertBlock()->getParent();

    auto loop_block = make_block("loop.block", parent_fn);
    auto loop_end = make_block("loop.end", parent_fn);

    bldr.CreateBr(loop_block);
    bldr.SetInsertPoint(loop_block);
    auto phi_node = bldr.CreatePHI(*Ptr(data_ty), 2, "phi");
    phi_node->addIncoming(start_ptr, prev_block);
    bldr.CreateCall(data_ty->assign(), { data, phi_node });

    auto next_ptr = bldr.CreateGEP(phi_node, { data::const_uint(1) });
    phi_node->addIncoming(next_ptr, loop_block);

    auto cmp = bldr.CreateICmpEQ(next_ptr, end_ptr);
    bldr.CreateCondBr(cmp, loop_end, loop_block);
    bldr.SetInsertPoint(loop_end);
    // TODO If you never assign this, the allocation is leaked

    return start_ptr;
  }

  llvm::Value* Statements::generate_code(Scope* scope) {
    for (auto& stmt : statements) {
      stmt->generate_code(scope);
    }
    return nullptr;
  }

  llvm::Value* ChainOp::generate_code(Scope* scope) {
    if (time() == Time::compile) {
      return llvm_value(evaluate(scope->context()));
    }

    using Language::Operator;
    auto lhs_val = exprs[0]->generate_code(scope);
    llvm::Value* ret_val = nullptr;

    auto& bldr = scope->builder();

    if (exprs[0]->type == Int) {
      for (size_t i = 1; i < exprs.size(); ++i) {
        auto rhs_val = exprs[i]->generate_code(scope);

        llvm::Value* cmp_val;

        // TODO early exit
        switch (ops[i - 1]) {
          case Operator::LessThan:
            cmp_val = bldr.CreateICmpSLT(lhs_val, rhs_val, "lttmp"); break;
          case Operator::LessEq:
            cmp_val = bldr.CreateICmpSLE(lhs_val, rhs_val, "letmp"); break;
          case Operator::Equal:
            cmp_val = bldr.CreateICmpEQ(lhs_val, rhs_val, "eqtmp"); break;
          case Operator::NotEqual:
            cmp_val = bldr.CreateICmpNE(lhs_val, rhs_val, "netmp"); break;
          case Operator::GreaterEq:
            cmp_val = bldr.CreateICmpSGE(lhs_val, rhs_val, "getmp"); break;
          case Operator::GreaterThan:
            cmp_val = bldr.CreateICmpSGT(lhs_val, rhs_val, "gttmp"); break;
          default:;
        }

        ret_val = (i != 1) ? bldr.CreateAnd(ret_val, cmp_val, "booltmp") : cmp_val;
        lhs_val = rhs_val;
      }

    } else if (exprs[0]->type == Uint) {
      for (size_t i = 1; i < exprs.size(); ++i) {
        auto rhs_val = exprs[i]->generate_code(scope);
        llvm::Value* cmp_val;

        // TODO early exit
        switch (ops[i - 1]) {
          case Operator::LessThan:
            cmp_val = bldr.CreateICmpULT(lhs_val, rhs_val, "lttmp"); break;
          case Operator::LessEq:
            cmp_val = bldr.CreateICmpULE(lhs_val, rhs_val, "letmp"); break;
          case Operator::Equal:
            cmp_val = bldr.CreateICmpEQ(lhs_val, rhs_val, "eqtmp"); break;
          case Operator::NotEqual:
            cmp_val = bldr.CreateICmpNE(lhs_val, rhs_val, "netmp"); break;
          case Operator::GreaterEq:
            cmp_val = bldr.CreateICmpUGE(lhs_val, rhs_val, "getmp"); break;
          case Operator::GreaterThan:
            cmp_val = bldr.CreateICmpUGT(lhs_val, rhs_val, "gttmp"); break;
          default:;
        }

        ret_val = (i != 1) ? bldr.CreateAnd(ret_val, cmp_val, "booltmp") : cmp_val;
        lhs_val = rhs_val;
      }

    } else if (exprs[0]->type == Real) {
      for (size_t i = 1; i < exprs.size(); ++i) {
        auto rhs_val = exprs[i]->generate_code(scope);
        llvm::Value* cmp_val;

        // TODO early exit
        // TODO should these be ordered, or can they be QNAN? probably.
        switch (ops[i - 1]) {
          case Operator::LessThan:
            cmp_val = bldr.CreateFCmpOLT(lhs_val, rhs_val, "lttmp"); break;
          case Operator::LessEq:
            cmp_val = bldr.CreateFCmpOLE(lhs_val, rhs_val, "letmp"); break;
          case Operator::Equal:
            cmp_val = bldr.CreateFCmpOEQ(lhs_val, rhs_val, "eqtmp"); break;
          case Operator::NotEqual:
            cmp_val = bldr.CreateFCmpONE(lhs_val, rhs_val, "netmp"); break;
          case Operator::GreaterEq:
            cmp_val = bldr.CreateFCmpOGE(lhs_val, rhs_val, "getmp"); break;
          case Operator::GreaterThan:
            cmp_val = bldr.CreateFCmpOGT(lhs_val, rhs_val, "gttmp"); break;
          default:;
        }

        ret_val = (i != 1) ? bldr.CreateAnd(ret_val, cmp_val, "booltmp") : cmp_val;
        lhs_val = rhs_val;
      }
    } else if (exprs[0]->type->is_enum()) {
      for (size_t i = 1; i < exprs.size(); ++i) {
        auto rhs_val = exprs[i]->generate_code(scope);
        llvm::Value* cmp_val;

        // TODO early exit
        switch (ops[i - 1]) {
          case Operator::Equal:
            cmp_val = bldr.CreateICmpEQ(lhs_val, rhs_val, "eqtmp"); break;
          case Operator::NotEqual:
            cmp_val = bldr.CreateICmpNE(lhs_val, rhs_val, "netmp"); break;
          default:;
        }

        ret_val = (i != 1) ? bldr.CreateAnd(ret_val, cmp_val, "booltmp") : cmp_val;
        lhs_val = rhs_val;
      }
    } else if (exprs[0]->type == Bool) {
      // For boolean expression, the chain must be a single consistent operation
      // because '&', '^', and '|' all have different precedence levels.
      auto cmp_val = lhs_val;
      if (ops.front() == Language::Operator::Xor) {
        for (size_t i = 1; i < exprs.size(); ++i) {
          auto expr = exprs[i];
          auto rhs_val = expr->generate_code(scope);
          cmp_val = bldr.CreateXor(cmp_val, rhs_val);
        }
      } else {
        auto parent_fn = bldr.GetInsertBlock()->getParent();
        // Condition blocks
        std::vector<llvm::BasicBlock*> conditionblocks(ops.size());
        for (auto& block : conditionblocks) {
          block = make_block("cond.block", parent_fn);
        }

        // Landing blocks
        auto land_true_block = make_block("land.true", parent_fn);
        auto land_false_block = make_block("land.false", parent_fn);
        auto merge_block = make_block("merge.block", parent_fn);

        if (ops.front() == Language::Operator::And) {
          for (size_t i = 0; i < ops.size(); ++i) {
            bldr.CreateCondBr(cmp_val, conditionblocks[i], land_false_block);
            bldr.SetInsertPoint(conditionblocks[i]);
            cmp_val = exprs[i + 1]->generate_code(scope);
          }
        } else {  // if (ops.front() == Language::Operator::Or) {
          for (size_t i = 0; i < ops.size(); ++i) {
            bldr.CreateCondBr(cmp_val, land_true_block, conditionblocks[i]);
            bldr.SetInsertPoint(conditionblocks[i]);
            cmp_val = exprs[i + 1]->generate_code(scope);
          }
        }

        bldr.CreateCondBr(cmp_val, land_true_block, land_false_block);

        bldr.SetInsertPoint(land_true_block);
        bldr.CreateBr(merge_block);

        bldr.SetInsertPoint(land_false_block);
        bldr.CreateBr(merge_block);

        bldr.SetInsertPoint(merge_block);
        // Join two cases
        llvm::PHINode* phi_node = bldr.CreatePHI(*Bool, 2, "merge");
        phi_node->addIncoming(data::const_true(), land_true_block);
        phi_node->addIncoming(data::const_false(), land_false_block);
        cmp_val = phi_node;
        }
        ret_val = cmp_val;
      }

      return ret_val;
    }

    llvm::Value* FunctionLiteral::generate_code(Scope* scope) {
      if (*type == nullptr) return nullptr;

      if (llvm_fn == nullptr) {
        // NOTE: This means a function is not assigned.
        llvm_fn = llvm::Function::Create(
            static_cast<llvm::FunctionType*>(type->llvm()),
            llvm::Function::ExternalLinkage, "__anon_fn", global_module);
      }

      // Name the inputs
      auto arg_iter = llvm_fn->args().begin();
      for (const auto& input_iter : inputs) {
        arg_iter->setName(input_iter->identifier->token());
        // Set alloc
        auto decl_id = input_iter->identifier;
        auto decl_type = decl_id->type;
        if (decl_type->is_struct()) {
          decl_id->alloc = arg_iter;
        }

        ++arg_iter;
      }


      auto ret_type = return_type_expr->evaluate(scope->context()).as_type;
      if (ret_type->is_struct()) {
        arg_iter->setName("retval");
      }

      auto old_block = scope->builder().GetInsertBlock();

      fn_scope->set_parent_function(llvm_fn);
      fn_scope->set_type(static_cast<Function*>(type));

      fn_scope->enter();
      auto arg = llvm_fn->args().begin();
      for (auto& input_iter : inputs) {
        auto decl_id = input_iter->identifier;

        if (!decl_id->type->is_struct()) {
          fn_scope->builder().CreateCall(decl_id->type->assign(),
              { arg, input_iter->identifier->alloc });
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
    llvm::Value* generate_assignment_code(Scope* scope, EPtr lhs, EPtr rhs) {
      llvm::Value* var = nullptr;
      llvm::Value* val = nullptr;

      // Treat functions special
      if (lhs->is_identifier() && rhs->type->is_function()) {
        if (lhs->token() == "__print__" || lhs->token() == "__assign__") {
          val = rhs->generate_code(scope);
          if (val == nullptr) return nullptr;

          // NOTE: Type verification asserts that the first argument of
          // each of these is the correct type.
          // TODO This verification hasn't yet been implemented and that it is a struct
          auto rhs_as_func = static_cast<Function*>(rhs->type);
          auto arg_type = static_cast<Structure*>(rhs_as_func->input);

          arg_type->set_print(static_cast<llvm::Function*>(val));

        } else {
          auto fn = std::static_pointer_cast<FunctionLiteral>(rhs);
          // TODO TOKENREMOVAL
          fn->llvm_fn = global_module->getFunction(lhs->token());

          val = rhs->generate_code(scope);
          if (val == nullptr) return nullptr;
          val->setName(lhs->token());
        }
      } else {
        val = rhs->generate_code(scope);
        if (val == nullptr) return nullptr;

        var = lhs->generate_lvalue(scope);
        if (var == nullptr) return nullptr;


        if (rhs->is_array_literal()) {
          scope->builder().CreateStore(val, var);

        } else {
          scope->builder().CreateCall(lhs->type->assign(), { val, var });
        }
      }

      return nullptr;
    }

    llvm::Value* Assignment::generate_code(Scope* scope) {
      using Language::Operator;
      if (   op == Operator::OrEq  || op == Operator::XorEq
          || op == Operator::AndEq || op == Operator::AddEq
          || op == Operator::SubEq || op == Operator::MulEq
          || op == Operator::DivEq || op == Operator::ModEq) {

        auto lhs_val = lhs->generate_code(scope);
        if (lhs_val == nullptr) return nullptr;

        auto lval = lhs->generate_lvalue(scope);
        if (lval == nullptr) return nullptr;

        auto rhs_val = rhs->generate_code(scope);
        if (rhs_val == nullptr) return nullptr;

        if (lhs->type == Bool) {
          switch (op) {
            case Operator::XorEq:
              scope->builder().CreateStore(
                  scope->builder().CreateXor(lhs_val, rhs_val, "xortmp"), lval);
            case Operator::AndEq:
            case Operator::OrEq:
              {
                auto parent_fn = scope->builder().GetInsertBlock()->getParent();
                auto more_block = make_block("more", parent_fn);
                auto merge_block = make_block("merge", parent_fn);

                // Assumption is that only operators of type (bool, bool) -> bool are '&', '|', and '^'
                auto true_block  = (op == Operator::AndEq) ? more_block : merge_block;
                auto false_block = (op == Operator::OrEq)  ? more_block : merge_block;

                scope->builder().CreateCondBr(lhs_val, true_block, false_block);
                scope->builder().SetInsertPoint(more_block);

                // Generating lvalue for storage
                scope->builder().CreateStore(rhs_val, lval);
                scope->builder().CreateBr(merge_block);

                scope->builder().SetInsertPoint(merge_block);
              }
            default:;
          }

          return nullptr;

        } else if (lhs->type == Int) {
          llvm::Value* val = nullptr;
          auto& bldr = scope->builder();
          switch (op) {
            case Operator::AddEq: val = bldr.CreateAdd (lhs_val, rhs_val, "at"); break;
            case Operator::SubEq: val = bldr.CreateSub (lhs_val, rhs_val, "st"); break;
            case Operator::MulEq: val = bldr.CreateMul (lhs_val, rhs_val, "mt"); break;
            case Operator::DivEq: val = bldr.CreateSDiv(lhs_val, rhs_val, "dt"); break;
            case Operator::ModEq: val = bldr.CreateSRem(lhs_val, rhs_val, "rt"); break;
            default: return nullptr;
          }
          bldr.CreateStore(val, lval);
          return nullptr;

        } else if (lhs->type == Uint) {
          auto& bldr = scope->builder();
          llvm::Value* val = nullptr;
          switch (op) {
            case Operator::AddEq: val = bldr.CreateAdd (lhs_val, rhs_val, "at"); break;
            case Operator::SubEq: val = bldr.CreateSub (lhs_val, rhs_val, "st"); break;
            case Operator::MulEq: val = bldr.CreateMul (lhs_val, rhs_val, "mt"); break;
            case Operator::DivEq: val = bldr.CreateUDiv(lhs_val, rhs_val, "dt"); break;
            case Operator::ModEq: val = bldr.CreateURem(lhs_val, rhs_val, "rt"); break;
            default: return nullptr;
          }
          bldr.CreateStore(val, lval);
          return nullptr;

        } else if (type == Real) {
          llvm::Value* val = nullptr;
          auto& bldr = scope->builder();
          switch (op) {
            case Operator::AddEq: val = bldr.CreateFAdd(lhs_val, rhs_val, "at"); break;
            case Operator::SubEq: val = bldr.CreateFSub(lhs_val, rhs_val, "st"); break;
            case Operator::MulEq: val = bldr.CreateFMul(lhs_val, rhs_val, "mt"); break;
            case Operator::DivEq: val = bldr.CreateFDiv(lhs_val, rhs_val, "dt"); break;
            default: return nullptr;
          }
          bldr.CreateStore(val, lval);
          return nullptr;
        }
      }

      // The left-hand side may be a declaration
      if (lhs->is_declaration()) {
        // TODO maybe the declarations generate_code ought to return an l-value for the thing it declares?
        return generate_assignment_code(scope, std::static_pointer_cast<Declaration>(lhs)->identifier, rhs);
      }

      return generate_assignment_code(scope, lhs, rhs);
    }

    llvm::Value* Declaration::generate_code(Scope* scope) {
      // For the most part, declarations are preallocated at the beginning
      // of each scope, so there's no need to do anything if a heap allocation
      // isn't required.

      if (type_expr->is_array_type()) {
        std::vector<llvm::Value*> init_args = { identifier->alloc };

        // Push the array lengths onto the vector for calling args
        EPtr next_ptr = type_expr;
        while (next_ptr->is_array_type()) {
          auto length =
              std::static_pointer_cast<AST::ArrayType>(next_ptr)->length;

          next_ptr =
              std::static_pointer_cast<AST::ArrayType>(next_ptr)->data_type;

          init_args.push_back(length->generate_code(scope));
        }

        auto array_type = static_cast<Array*>(type);
        scope->builder().CreateCall(array_type->initialize(), init_args);
      }

      if (!is_inferred || type == Type_) return nullptr;

      // Remember, type_expr is not really the right name in the inference case.
      // It's the thing whose type we are inferring.
      //
      // TODO change the name of this member variable to describe what it actually
      // is in both ':' and ':=" cases
      return generate_assignment_code(scope, identifier, type_expr);
    }

    llvm::Value* Case::generate_code(Scope* scope) {
      auto parent_fn = scope->builder().GetInsertBlock()->getParent();
      // Condition blocks - The ith block is what you reach when you've
      // failed the ith condition, where conditions are labelled starting at zero.
      std::vector<llvm::BasicBlock*> case_blocks(kv->pairs.size() - 1);

      for (auto& block : case_blocks) {
        block = make_block("case.block", parent_fn);
      }

      // Landing blocks
      auto current_block = scope->builder().GetInsertBlock();
      auto case_landing = make_block("case.landing", parent_fn);
      scope->builder().SetInsertPoint(case_landing);
      llvm::PHINode* phi_node = scope->builder().CreatePHI(*type,
          static_cast<unsigned int>(kv->pairs.size()), "phi");
      scope->builder().SetInsertPoint(current_block);

      for (size_t i = 0; i < case_blocks.size(); ++i) {
        auto cmp_val = kv->pairs[i].first->generate_code(scope);
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

    llvm::Value* ArrayLiteral::generate_code(Scope* scope) {
      // TODO if this is never assigned to anything, it will be leaked

      auto type_as_array = static_cast<Array*>(type);
      auto element_type = type_as_array->data_type;

      size_t num_elems = elems.size();

      auto array_data = type_as_array->initialize_literal(
          scope->builder(), data::const_uint(num_elems));

      if (!element_type->is_array()) {
        for (size_t i = 0; i < num_elems; ++i) {
          auto data_ptr = scope->builder().CreateGEP(
              *element_type, array_data, { data::const_uint(i) });

          scope->builder().CreateCall(element_type->assign(),
              { elems[i]->generate_code(scope), data_ptr });
        }

      } else {
        for (size_t i = 0; i < num_elems; ++i) {
          auto data_ptr = scope->builder().CreateGEP(
              *element_type, array_data, { data::const_uint(i) });
          scope->builder().CreateStore(elems[i]->generate_code(scope), data_ptr);
        }
      }

      return array_data;
    }

    llvm::Value* KVPairList::generate_code(Scope*) { return nullptr; }

    llvm::Value* While::generate_code(Scope* scope) {
      auto parent_fn = scope->builder().GetInsertBlock()->getParent();

      auto while_stmt_block = make_block("while.stmt", parent_fn);

      while_scope->set_parent_function(parent_fn);

      scope->builder().CreateBr(while_scope->entry_block());
      while_scope->enter();
      auto cond = condition->generate_code(while_scope);
      while_scope->builder().CreateCondBr(cond,
          while_stmt_block, while_scope->landing_block());

      while_scope->builder().SetInsertPoint(while_stmt_block);
      statements->generate_code(while_scope);
      while_scope->exit();
      scope->builder().SetInsertPoint(while_scope->landing_block());

      return nullptr;
    }

    llvm::Value* Conditional::generate_code(Scope* scope) {
      auto parent_fn = scope->builder().GetInsertBlock()->getParent();

      // Last block is either the else-block or the landing block if
      // no else-block exists.
      std::vector<llvm::BasicBlock*> conditionblocks(conditions.size() + 1,
          nullptr);

      for (size_t i = 0; i < conditionblocks.size(); ++i) {
        conditionblocks[i] = make_block("cond.block", parent_fn);
      }

      llvm::BasicBlock* landing = has_else()
        ? make_block("land", parent_fn)
        : conditionblocks.back();

      scope->builder().CreateBr(conditionblocks[0]);

      for (size_t i = 0; i < conditions.size(); ++i) {
        scope->builder().SetInsertPoint(conditionblocks[i]);
        auto condition = conditions[i]->generate_code(scope);
        scope->builder().CreateCondBr(condition,
            body_scopes[i]->entry_block(), conditionblocks[i + 1]);
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
    llvm::Value* EnumLiteral::generate_code(Scope* scope) { return nullptr; }
    llvm::Value* TypeLiteral::generate_code(Scope* scope) { return nullptr; }

    llvm::Value* Break::generate_code(Scope* scope) {
      auto scope_ptr = scope;

      auto prev_insert = scope->builder().GetInsertBlock();

      auto parent_fn = scope->builder().GetInsertBlock()->getParent();
      llvm::BasicBlock* dealloc_block = make_block("dealloc.block", parent_fn);
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
        error_log.log(line_num, "A `break` command was encountered outside of a loop.");

      } else {
        auto while_scope = static_cast<WhileScope*>(scope_ptr);
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


  }  // namespace AST
