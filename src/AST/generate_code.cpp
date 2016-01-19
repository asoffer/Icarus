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
extern llvm::Function* global_function;

namespace builtin {
  extern llvm::Function* ascii();
}  // namespace builtin

namespace data {
  extern llvm::Value* const_true();
  extern llvm::Value* const_false();
  extern llvm::Value* const_uint(size_t n);
  extern llvm::Value* const_int(int n);
  extern llvm::Value* const_char(char c);
  extern llvm::Value* const_real(double d);
  extern llvm::Value* global_string(llvm::IRBuilder<>& bldr, const std::string& s);
}  // namespace data

namespace AST {
  llvm::Value* Identifier::generate_code(Scope* scope) {
    if (type()->is_function()) {
      return global_module->getFunction(token());
    }

    return scope->builder().CreateLoad(alloc_, token());
  }

  // Invariant:
  // Only returns nullptr if the expression type is void or a type
  llvm::Value* Terminal::generate_code(Scope* scope) {
    //TODO remove dependence on token() altogether

    switch (terminal_type_) {
      using Language::Terminal;
      case Terminal::ASCII:  return builtin::ascii();
      case Terminal::True:   return data::const_true();
      case Terminal::False:  return data::const_false();
      case Terminal::Char:   return data::const_char(token()[0]);
      case Terminal::Int:    return data::const_int(std::stoi(token()));
      case Terminal::Real:   return data::const_real(std::stod(token()));
      case Terminal::UInt:   return data::const_uint(std::stoul(token()));
      case Terminal::Type:   return nullptr;
      case Terminal::Return: return nullptr;
    }
  }

  // Invariant:
  // Only returns nullptr if the expression type is void or a type
  llvm::Value* Unop::generate_code(Scope* scope) {
    if (op_ == Language::Operator::And) {
      return expr_->generate_lvalue(scope);
    }

    llvm::Value* val = expr_->generate_code(scope);
    llvm::IRBuilder<>& bldr = scope->builder();
    using Language::Operator;
    switch (op_) { 
      case Operator::Sub:
        return expr_->type()->call_neg(bldr, val);

      case Operator::Not:
        return expr_->type()->call_not(bldr, val);

      case Operator::Return:
        scope->make_return(val);
        return nullptr;

      case Operator::At:
        return bldr.CreateLoad(bldr.CreateGEP(val, { data::const_uint(0) }));

      case Operator::Call:
        return scope->builder().CreateCall(static_cast<llvm::Function*>(val));

      case Operator::Print:
        // NOTE: BE VERY CAREFUL HERE. YOU ARE TYPE PUNNING!
        if (expr_->type() == Type::get_type()) {
          val = reinterpret_cast<llvm::Value*>(expr_->interpret_as_type());
        }
        expr_->type()->call_print(scope->builder(), val);
        return nullptr;

      default:
        return nullptr;
    }
  }

  llvm::Value* Binop::generate_code(Scope* scope) {
    if (time() == Time::compile) {
      Context ctx = Context::GlobalContext.spawn();
      return llvm_value(evaluate(ctx));
    }

    using Language::Operator;
    switch (op_) {
      case Operator::Index:
        return scope->builder().CreateLoad(generate_lvalue(scope), "array_val");
      case Operator::Access:
        return (lhs_->type()->is_user_defined())
          ? scope->builder().CreateLoad(generate_lvalue(scope))
          : nullptr;
      default:;
    }

    auto lhs_val = lhs_->generate_code(scope);
    if (lhs_val == nullptr) return nullptr;

    switch (op_) {
      case Operator::Cast:
        return lhs_->type()->call_cast(scope->builder(), lhs_val, rhs_->interpret_as_type());
    
      case Operator::Call:
        if (lhs_->type()->is_function()) {
          std::vector<llvm::Value*> arg_vals;
          if (rhs_->is_comma_list()) {
            auto arg_chainop = std::static_pointer_cast<ChainOp>(rhs_);
            arg_vals.resize(arg_chainop->exprs_.size(), nullptr);
            size_t i = 0;
            for (const auto& expr : arg_chainop->exprs_) {
              arg_vals[i] = expr->generate_code(scope);
              if (arg_vals[i] == nullptr) return nullptr;
              ++i;
            }

          } else {
            auto rhs_val = rhs_->generate_code(scope);
            if (rhs_val == nullptr) return nullptr;
            arg_vals = { rhs_val };
          }

          if (type()->is_void()) {
            scope->builder().CreateCall(static_cast<llvm::Function*>(lhs_val), arg_vals);
            return nullptr;

          } else {
            return scope->builder().CreateCall(static_cast<llvm::Function*>(lhs_val), arg_vals, "calltmp");
          }
        }
      default:;
    }

    auto rhs_val = rhs_->generate_code(scope);
    if (rhs_val == nullptr) return nullptr;

    llvm::IRBuilder<>& bldr = scope->builder();
    switch (op_) {
      case Operator::Add: return type()->call_add(bldr, lhs_val, rhs_val);
      case Operator::Sub: return type()->call_sub(bldr, lhs_val, rhs_val);
      case Operator::Mul: return type()->call_mul(bldr, lhs_val, rhs_val);
      case Operator::Div: return type()->call_div(bldr, lhs_val, rhs_val);
      case Operator::Mod: return type()->call_mod(bldr, lhs_val, rhs_val);
      default:;
    }

    return nullptr;
  }

  llvm::Value* ArrayType::generate_code(Scope* scope) { return nullptr; }

  llvm::Value* Statements::generate_code(Scope* scope) {
    for (auto& stmt : statements_) {
      stmt->generate_code(scope);
    }
    return nullptr;
  }

  llvm::Value* ChainOp::generate_code(Scope* scope) {
    if (time() == Time::compile) {
      Context ctx = Context::GlobalContext.spawn();
      return llvm_value(evaluate(ctx));
    }

    using Language::Operator;
    auto lhs_val = exprs_[0]->generate_code(scope);
    llvm::Value* ret_val = nullptr;

    auto& bldr = scope->builder();

    if (exprs_[0]->type() == Type::get_int()) {
      for (size_t i = 1; i < exprs_.size(); ++i) {
        auto rhs_val = exprs_[i]->generate_code(scope);
        llvm::Value* cmp_val;

        // TODO early exit
        switch (ops_[i - 1]) {
          case Operator::LessThan:    cmp_val = bldr.CreateICmpSLT(lhs_val, rhs_val, "lttmp");
          case Operator::LessEq:      cmp_val = bldr.CreateICmpSLE(lhs_val, rhs_val, "letmp");
          case Operator::Equal:       cmp_val = bldr.CreateICmpEQ(lhs_val, rhs_val, "eqtmp");
          case Operator::NotEqual:    cmp_val = bldr.CreateICmpNE(lhs_val, rhs_val, "netmp");
          case Operator::GreaterEq:   cmp_val = bldr.CreateICmpSGE(lhs_val, rhs_val, "getmp");
          case Operator::GreaterThan: cmp_val = bldr.CreateICmpSGT(lhs_val, rhs_val, "gttmp");
          default:;
        }

        ret_val = (i != 1) ? bldr.CreateAnd(ret_val, cmp_val, "booltmp") : cmp_val;
        lhs_val = rhs_val;
      }

    } else if (exprs_[0]->type() == Type::get_uint()) {
      for (size_t i = 1; i < exprs_.size(); ++i) {
        auto rhs_val = exprs_[i]->generate_code(scope);
        llvm::Value* cmp_val;

        // TODO early exit
        switch (ops_[i - 1]) {
          case Operator::LessThan:    cmp_val = bldr.CreateICmpULT(lhs_val, rhs_val, "lttmp");
          case Operator::LessEq:      cmp_val = bldr.CreateICmpULE(lhs_val, rhs_val, "letmp");
          case Operator::Equal:       cmp_val = bldr.CreateICmpEQ(lhs_val, rhs_val, "eqtmp");
          case Operator::NotEqual:    cmp_val = bldr.CreateICmpNE(lhs_val, rhs_val, "netmp");
          case Operator::GreaterEq:   cmp_val = bldr.CreateICmpUGE(lhs_val, rhs_val, "getmp");
          case Operator::GreaterThan: cmp_val = bldr.CreateICmpUGT(lhs_val, rhs_val, "gttmp");
          default:;
        }

        ret_val = (i != 1) ? bldr.CreateAnd(ret_val, cmp_val, "booltmp") : cmp_val;
        lhs_val = rhs_val;

      }

    } else if (exprs_[0]->type() == Type::get_real()) {
      for (size_t i = 1; i < exprs_.size(); ++i) {
        auto rhs_val = exprs_[i]->generate_code(scope);
        llvm::Value* cmp_val;

        // TODO early exit
        // TODO should these be ordered, or can they be QNAN? probably.
        switch (ops_[i - 1]) {
          case Operator::LessThan:    cmp_val = bldr.CreateFCmpOLT(lhs_val, rhs_val, "lttmp");
          case Operator::LessEq:      cmp_val = bldr.CreateFCmpOLE(lhs_val, rhs_val, "letmp");
          case Operator::Equal:       cmp_val = bldr.CreateFCmpOEQ(lhs_val, rhs_val, "eqtmp");
          case Operator::NotEqual:    cmp_val = bldr.CreateFCmpONE(lhs_val, rhs_val, "netmp");
          case Operator::GreaterEq:   cmp_val = bldr.CreateFCmpOGE(lhs_val, rhs_val, "getmp");
          case Operator::GreaterThan: cmp_val = bldr.CreateFCmpOGT(lhs_val, rhs_val, "gttmp");
          default:;
        }

        ret_val = (i != 1) ? bldr.CreateAnd(ret_val, cmp_val, "booltmp") : cmp_val;
        lhs_val = rhs_val;
      }
    } else if (exprs_[0]->type() == Type::get_bool()) {
      // For boolean expression, the chain must be a single consistent operation
      // because '&', '^', and '|' all have different precedence levels.
      auto cmp_val = lhs_val;
      if (ops_.front() == Language::Operator::Xor) {
        for (size_t i = 1; i < exprs_.size(); ++i) {
          auto expr = exprs_[i];
          auto rhs_val = expr->generate_code(scope);
          cmp_val = bldr.CreateXor(cmp_val, rhs_val);
        }
      } else {
        auto parent_fn = bldr.GetInsertBlock()->getParent();
        // Condition blocks
        std::vector<llvm::BasicBlock*> cond_blocks(ops_.size());
        for (auto& block : cond_blocks) {
          block = make_block("cond.block", parent_fn);
        }

        // Landing blocks
        auto land_true_block = make_block("land.true", parent_fn);
        auto land_false_block = make_block("land.false", parent_fn);
        auto merge_block = make_block("merge.block", parent_fn);

        if (ops_.front() == Language::Operator::And) {
          for (size_t i = 0; i < ops_.size(); ++i) {
            bldr.CreateCondBr(cmp_val, cond_blocks[i], land_false_block);
            bldr.SetInsertPoint(cond_blocks[i]);
            cmp_val = exprs_[i + 1]->generate_code(scope);
          }
        } else {  // if (ops_.front() == Language::Operator::Or) {
          for (size_t i = 0; i < ops_.size(); ++i) {
            bldr.CreateCondBr(cmp_val, land_true_block, cond_blocks[i]);
            bldr.SetInsertPoint(cond_blocks[i]);
            cmp_val = exprs_[i + 1]->generate_code(scope);
          }
        }

        bldr.CreateCondBr(cmp_val, land_true_block, land_false_block);

        bldr.SetInsertPoint(land_true_block);
        bldr.CreateBr(merge_block);

        bldr.SetInsertPoint(land_false_block);
        bldr.CreateBr(merge_block);

        bldr.SetInsertPoint(merge_block);
        // Join two cases
        llvm::PHINode* phi_node = bldr.CreatePHI(
            Type::get_bool()->llvm(), 2, "merge");
        phi_node->addIncoming(data::const_true(), land_true_block);
        phi_node->addIncoming(data::const_false(), land_false_block);
        cmp_val = phi_node;
      }
      ret_val = cmp_val;
    }

    return ret_val;
  }

  llvm::Value* FunctionLiteral::generate_code(Scope* scope) {
    if (type()->llvm() == nullptr) return nullptr;

    if (llvm_function_ == nullptr) {
      // NOTE: This means a function is not assigned.
      llvm_function_ = llvm::Function::Create(
          static_cast<llvm::FunctionType*>(type()->llvm()),
          llvm::Function::ExternalLinkage, "__anon_fn", global_module);
    }

    // Name the inputs
    auto input_iter = inputs_.begin();
    for (auto& arg : llvm_function_->args()) {
      arg.setName((*input_iter)->identifier_string());
      ++input_iter;
    }

    auto old_block = scope->builder().GetInsertBlock();

    fn_scope_->set_parent_function(llvm_function_);
    fn_scope_->set_type(static_cast<Function*>(expr_type_));

    fn_scope_->enter();
    
    // TODO move this to fn_scope_.enter()
    input_iter = inputs_.begin();
    for (auto& arg : llvm_function_->args()) {
      auto decl_id = (*input_iter)->declared_identifier();
      fn_scope_->builder().CreateCall(decl_id->type()->assign(),
          { &arg, (*input_iter)->declared_identifier()->alloc_ });
      ++input_iter;
    }

    statements_->generate_code(fn_scope_);

    fn_scope_->exit();

    scope->builder().SetInsertPoint(old_block);
    return llvm_function_;
  }

  // This function exists because both '=' and ':=' need to call some version of
  // the same code. it's been factored out here.
  llvm::Value* generate_assignment_code(Scope* scope, EPtr lhs, EPtr rhs) {
    llvm::Value* var = nullptr;
    llvm::Value* val = nullptr;

    // Treat functions special
    if (lhs->is_identifier() && rhs->type()->is_function()) {
      auto fn = std::static_pointer_cast<FunctionLiteral>(rhs);
      // TODO TOKENREMOVAL
      fn->llvm_function_ = global_module->getFunction(lhs->token());

      val = rhs->generate_code(scope);
      if (val == nullptr) return nullptr;
      val->setName(lhs->token());

    } else {
      val = rhs->generate_code(scope);
      if (val == nullptr) return nullptr;

      var = lhs->generate_lvalue(scope);
      if (var == nullptr) return nullptr;


      if (rhs->is_array_literal()) {
        scope->builder().CreateStore(val, var);
      } else {
        scope->builder().CreateCall(lhs->type()->assign(), { val, var });
      }
    }

    return nullptr;
  }

  llvm::Value* Assignment::generate_code(Scope* scope) {
    using Language::Operator;
    if (   op_ == Operator::OrEq  || op_ == Operator::XorEq
        || op_ == Operator::AndEq || op_ == Operator::AddEq
        || op_ == Operator::SubEq || op_ == Operator::MulEq
        || op_ == Operator::DivEq || op_ == Operator::ModEq) {

      auto lhs_val = lhs_->generate_code(scope);
      if (lhs_val == nullptr) return nullptr;

      auto lval = lhs_->generate_lvalue(scope);
      if (lval == nullptr) return nullptr;

      auto rhs_val = rhs_->generate_code(scope);
      if (rhs_val == nullptr) return nullptr;

      if (lhs_->type() == Type::get_bool()) {
        switch (op_) {
          case Operator::XorEq:
            scope->builder().CreateStore(scope->builder().CreateXor(lhs_val, rhs_val, "xortmp"), lval);
          case Operator::AndEq:
          case Operator::OrEq:
            {
              auto parent_fn = scope->builder().GetInsertBlock()->getParent();
              auto more_block = make_block("more", parent_fn);
              auto merge_block = make_block("merge", parent_fn);

              // Assumption is that only operators of type (bool, bool) -> bool are '&', '|', and '^'
              auto true_block  = (op_ == Operator::AndEq) ? more_block : merge_block;
              auto false_block = (op_ == Operator::OrEq)  ? more_block : merge_block;

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

      } else if (lhs_->type() == Type::get_int()) {
        llvm::Value* comp_val = nullptr;
        switch (op_) {
          case Operator::AddEq: comp_val = scope->builder().CreateAdd(lhs_val, rhs_val, "addtmp");  break;
          case Operator::SubEq: comp_val = scope->builder().CreateSub(lhs_val, rhs_val, "subtmp");  break;
          case Operator::MulEq: comp_val = scope->builder().CreateMul(lhs_val, rhs_val, "multmp");  break;
          case Operator::DivEq: comp_val = scope->builder().CreateSDiv(lhs_val, rhs_val, "divtmp"); break;
          case Operator::ModEq: comp_val = scope->builder().CreateSRem(lhs_val, rhs_val, "remtmp"); break;
          default: return nullptr;
        }
        scope->builder().CreateStore(comp_val, lval);
        return nullptr;

      } else if (lhs_->type() == Type::get_uint()) {
        llvm::Value* comp_val = nullptr;
        switch (op_) {
          case Operator::AddEq: comp_val = scope->builder().CreateAdd(lhs_val, rhs_val, "addtmp");  break;
          case Operator::SubEq: comp_val = scope->builder().CreateSub(lhs_val, rhs_val, "subtmp");  break;
          case Operator::MulEq: comp_val = scope->builder().CreateMul(lhs_val, rhs_val, "multmp");  break;
          case Operator::DivEq: comp_val = scope->builder().CreateUDiv(lhs_val, rhs_val, "divtmp"); break;
          case Operator::ModEq: comp_val = scope->builder().CreateURem(lhs_val, rhs_val, "remtmp"); break;
          default: return nullptr;
        }
        scope->builder().CreateStore(comp_val, lval);
        return nullptr;

      } else if (type() == Type::get_real()) {
        llvm::Value* comp_val = nullptr;
        switch (op_) {
          case Operator::AddEq: comp_val = scope->builder().CreateFAdd(lhs_val, rhs_val, "addtmp"); break;
          case Operator::SubEq: comp_val = scope->builder().CreateFSub(lhs_val, rhs_val, "subtmp"); break;
          case Operator::MulEq: comp_val = scope->builder().CreateFMul(lhs_val, rhs_val, "multmp"); break;
          case Operator::DivEq: comp_val = scope->builder().CreateFDiv(lhs_val, rhs_val, "divtmp"); break;
          default: return nullptr;
        }
        scope->builder().CreateStore(comp_val, lval);
        return nullptr;
      }
    }

    // The left-hand side may be a declaration
    if (lhs_->is_declaration()) {
      // TODO maybe the declarations generate_code ought to return an l-value for the thing it declares?
      return generate_assignment_code(scope, std::static_pointer_cast<Declaration>(lhs_)->id_, rhs_);
    }

    return generate_assignment_code(scope, lhs_, rhs_);
  }

  llvm::Value* Declaration::generate_code(Scope* scope) {
    // For the most part, declarations are preallocated at the beginning
    // of each scope, so there's no need to do anything if a heap allocation
    // isn't required.

    if (declared_type()->is_array_type()) {
      std::vector<llvm::Value*> init_args = { declared_identifier()->alloc_ };

      // Push the array lengths onto the vector for calling args
      EPtr next_ptr = declared_type();
      while (next_ptr->is_array_type()) {
        auto length =
          std::static_pointer_cast<AST::ArrayType>(next_ptr)->length();

        next_ptr =
          std::static_pointer_cast<AST::ArrayType>(next_ptr)->data_type();

        init_args.push_back(length->generate_code(scope));
      }

      auto array_type = static_cast<Array*>(type());
      scope->builder().CreateCall(array_type->initialize(), init_args);
    }

    if (!infer_type_) return nullptr;

    // Remember, decl_type_ is not really the right name in the inference case.
    // It's the thing whose type we are inferring.
    //
    // TODO change the name of this member variable to describe what it actually
    // is in both ':' and ':=" cases
    return generate_assignment_code(scope, id_, decl_type_);
  }

  llvm::Value* Case::generate_code(Scope* scope) {
    auto parent_fn = scope->builder().GetInsertBlock()->getParent();
    // Condition blocks - The ith block is what you reach when you've
    // failed the ith condition, where conditions are labelled starting at zero.
    std::vector<llvm::BasicBlock*> case_blocks(pairs_->kv_pairs_.size() - 1);

    for (auto& block : case_blocks) {
      block = make_block("case.block", parent_fn);
    }

    // Landing blocks
    auto current_block = scope->builder().GetInsertBlock();
    auto case_landing = make_block("case.landing", parent_fn);
    scope->builder().SetInsertPoint(case_landing);
    llvm::PHINode* phi_node = scope->builder().CreatePHI(type()->llvm(),
          static_cast<unsigned int>(pairs_->kv_pairs_.size()), "phi");
    scope->builder().SetInsertPoint(current_block);

    for (size_t i = 0; i < case_blocks.size(); ++i) {
      auto cmp_val = pairs_->kv_pairs_[i].first->generate_code(scope);
      auto true_block = make_block("land_true", parent_fn);
 
      // If it's false, move on to the next block
      scope->builder().CreateCondBr(cmp_val, true_block, case_blocks[i]);
      scope->builder().SetInsertPoint(true_block);
      auto output_val = pairs_->kv_pairs_[i].second->generate_code(scope);

      // NOTE: You may be tempted to state that you are coming from the
      // block 'true_block'. However, if the code generated for the right-hand
      // side of the '=>' node is not just a single basic block, this will not
      // be the case.
      phi_node->addIncoming(output_val, scope->builder().GetInsertBlock());
      scope->builder().CreateBr(case_landing);

      scope->builder().SetInsertPoint(case_blocks[i]);
    }
    auto output_val = pairs_->kv_pairs_.back().second->generate_code(scope);

    phi_node->addIncoming(output_val, scope->builder().GetInsertBlock());
    scope->builder().CreateBr(case_landing);
    scope->builder().SetInsertPoint(case_landing);

    return phi_node;
  }

  llvm::Value* ArrayLiteral::generate_code(Scope* scope) {
    // TODO if this is never assigned to anything, it will be leaked

    auto type_as_array = static_cast<Array*>(expr_type_);
    auto element_type = type_as_array->data_type();

    size_t elems_size = elems_.size();

    auto array_data = type_as_array->initialize_literal(scope->builder(), data::const_uint(elems_size));

    if (!element_type->is_array()) {
      for (size_t i = 0; i < elems_size; ++i) {
        auto data_ptr = scope->builder().CreateGEP(element_type->llvm(),
            array_data, { data::const_uint(i) });

        scope->builder().CreateCall(element_type->assign(),
            { elems_[i]->generate_code(scope), data_ptr });
      }

    } else {
      for (size_t i = 0; i < elems_size; ++i) {
        auto data_ptr = scope->builder().CreateGEP(element_type->llvm(),
            array_data, { data::const_uint(i) });
        scope->builder().CreateStore(elems_[i]->generate_code(scope), data_ptr);
      }
    }

    return array_data;
  }

  llvm::Value* KVPairList::generate_code(Scope*) { return nullptr; }

  llvm::Value* While::generate_code(Scope* scope) {
    auto parent_fn = scope->builder().GetInsertBlock()->getParent();

    auto while_stmt_block = make_block("while.stmt", parent_fn);

    body_scope_->set_parent_function(parent_fn);

    scope->builder().CreateBr(body_scope_->entry_block());
    body_scope_->enter();
    auto cond = cond_->generate_code(body_scope_);
    body_scope_->builder().CreateCondBr(cond,
        while_stmt_block, body_scope_->landing_block());

    body_scope_->builder().SetInsertPoint(while_stmt_block);
    statements_->generate_code(body_scope_);
    body_scope_->exit();
    scope->builder().SetInsertPoint(body_scope_->landing_block());

    return nullptr;
  }

  llvm::Value* Conditional::generate_code(Scope* scope) {
    auto parent_fn = scope->builder().GetInsertBlock()->getParent();

    // Last block is either the else-block or the landing block if
    // no else-block exists.
    std::vector<llvm::BasicBlock*> cond_blocks(conds_.size() + 1,
        nullptr);
    
    for (size_t i = 0; i < cond_blocks.size(); ++i) {
      cond_blocks[i] = make_block("cond.block", parent_fn);
    }

    llvm::BasicBlock* landing = has_else()
      ? make_block("land", parent_fn)
      : cond_blocks.back();

    scope->builder().CreateBr(cond_blocks[0]);

    for (size_t i = 0; i < conds_.size(); ++i) {
      scope->builder().SetInsertPoint(cond_blocks[i]);
      auto condition = conds_[i]->generate_code(scope);
      scope->builder().CreateCondBr(condition,
          body_scopes_[i]->entry_block(), cond_blocks[i + 1]);
    }

    scope->builder().SetInsertPoint(cond_blocks.back());
    if (has_else()) {
      scope->builder().CreateBr(body_scopes_.back()->entry_block());
    }

    for (size_t i = 0; i < statements_.size(); ++i) {
      body_scopes_[i]->set_parent_function(parent_fn);
      body_scopes_[i]->enter();
      statements_[i]->generate_code(body_scopes_[i]);
      body_scopes_[i]->exit();
      body_scopes_[i]->builder().CreateBr(landing);
    }

    scope->builder().SetInsertPoint(landing);

    return nullptr;
  }

  llvm::Value* TypeLiteral::generate_code(Scope* scope) { return nullptr; }
  llvm::Value* EnumLiteral::generate_code(Scope* scope) { return nullptr; }

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
      error_log.log(line_num(),
          "A `break` command was encountered outside of a loop.");

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
