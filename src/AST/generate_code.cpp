#include "AST.h"

// TODO
// We often have if (val == nullptr) return nullptr to propogate nullptrs. In
// what situations is a nullptr actually possible to start with? If we can do
// all checks on the AST before code generation, then maybe we can remove these
// and thereby streamline the architecture.

extern llvm::Module* global_module;
extern llvm::IRBuilder<> builder;

namespace cstdlib {
  extern llvm::Constant* putchar;
  extern llvm::Constant* puts;
  extern llvm::Constant* printf;
  extern llvm::Value* format_d;
  extern llvm::Value* format_f;
  extern llvm::Value* format_s;
}  //namespace cstdlib

namespace AST {
  llvm::Value* Identifier::generate_code(Scope* scope) {
    if (type()->is_function()) {
      return global_module->getFunction(token());
    }
    return builder.CreateLoad(alloc_, token());
  }

  llvm::Value* Terminal::generate_code(Scope* scope) {
    // TODO Do I want to use string-to-X functions, or should I roll my own?
    //
    // The benefits are clear, but this ties me to using the same representation
    // that C++ uses.

    // TODO move this to Type
    if (type() == Type::get_void()) {
      if (token() == "return") {
        builder.CreateRetVoid();
      }
      return nullptr;

    } else if (type() == Type::get_unknown() || type() == Type::get_type_error()) {
      return nullptr;

    } else if (type() == Type::get_bool()) {
      // A bool is an unsigned 1-bit integer
      return llvm::ConstantInt::get(llvm::getGlobalContext(),
          llvm::APInt(1, token() == "true" ? 1 : 0, false));

    } else if (type() == Type::get_char()) {
      // A character is an unsigend 8-bit integer
      return llvm::ConstantInt::get(llvm::getGlobalContext(),
          llvm::APInt(8, static_cast<unsigned int>(token()[0]), false));

    } else if (type() == Type::get_int()) {
      // An int is a 32-bit signed integer
      return llvm::ConstantInt::get(llvm::getGlobalContext(),
          llvm::APInt(32, std::stoul(token()), true));

    } else if (type() == Type::get_real()) {
      return llvm::ConstantFP::get(llvm::getGlobalContext(),
          llvm::APFloat(std::stod(token())));

    } else if (type() == Type::get_type()) {
      return nullptr;

    } else if (type() == Type::get_uint()) {
      // A uint is a 64-bit unsigned integer
      return llvm::ConstantInt::get(llvm::getGlobalContext(),
          llvm::APInt(32, std::stoul(token()), false));
    } else {
      std::cerr << "FATAL: Terminal type is not a primitive type" << std::endl;
      return nullptr;
    }
  }

  llvm::Value* Unop::generate_code(Scope* scope) {
    // TODO don't figure out what it is from the tokens.
    llvm::Value* val = expr_->generate_code(scope);

    if (is_return()) {
      builder.CreateRet(val);

    } else if (token() == "()") {
      return builder.CreateCall(static_cast<llvm::Function*>(val));

    } else if(expr_->type() == Type::get_bool() && token() == "!") {
      return builder.CreateNot(val, "nottmp");

    } else if (is_print()) {
      if (expr_->type() == Type::get_bool()) {
        auto true_str = builder.CreateGlobalStringPtr("true");
        auto false_str = builder.CreateGlobalStringPtr("false");

        auto parent_block = builder.GetInsertBlock()->getParent();

        auto true_block = llvm::BasicBlock::Create(
            llvm::getGlobalContext(), "true_block", parent_block);
        auto false_block = llvm::BasicBlock::Create(
            llvm::getGlobalContext(), "false_block", parent_block);
        auto merge_block = llvm::BasicBlock::Create(
            llvm::getGlobalContext(), "merge_block", parent_block);


        builder.CreateCondBr(val, true_block, false_block);

        builder.SetInsertPoint(true_block);
        builder.CreateBr(merge_block);

        builder.SetInsertPoint(false_block);
        builder.CreateBr(merge_block);


        builder.SetInsertPoint(merge_block);
        llvm::PHINode* phi_node =
          builder.CreatePHI(llvm::Type::getInt8PtrTy(llvm::getGlobalContext()), 2, "merge");
        phi_node->addIncoming(true_str, true_block);
        phi_node->addIncoming(false_str, false_block);

        builder.CreateCall(cstdlib::puts, phi_node);

      } else if (expr_->type() == Type::get_char()) {
        builder.CreateCall(cstdlib::putchar, { val });

      } else if (expr_->type() == Type::get_int()
          || expr_->type() == Type::get_uint()) {
        builder.CreateCall(cstdlib::printf, { cstdlib::format_d, val });

      } else if (expr_->type() == Type::get_real()) {
        builder.CreateCall(cstdlib::printf, { cstdlib::format_f, val });

      } else if (expr_->type() == Type::get_type()) {
        auto type_as_string = builder.CreateGlobalStringPtr(expr_->interpret_as_type()->to_string());
        builder.CreateCall(cstdlib::printf, { cstdlib::format_s, type_as_string });

      } else if (expr_->type()->is_function()) {
        auto fn_str = builder.CreateGlobalStringPtr("{" + expr_->type()->to_string() + "}");

        builder.CreateCall(cstdlib::printf, { cstdlib::format_s, fn_str });

      } else if (expr_->type()->is_tuple()) {
        // TODO

      } else if (expr_->type()->is_pointer()) {
        // TODO

      } else if (expr_->type()->is_array()) {
        // TODO

      } else {
        // TODO
      }
    }

    return nullptr;
  }

  llvm::Value* Binop::generate_code(Scope* scope) {
    if (token() == "[]") {
      return builder.CreateLoad(generate_lvalue(scope), "array_val");
    }

    auto lhs_val = lhs_->generate_code(scope);
    if (lhs_val == nullptr) return nullptr;

    auto rhs_val = rhs_->generate_code(scope);
    if (rhs_val == nullptr) return nullptr;


    if (token() == "()") {
      // TODO multiple arguments
      std::vector<llvm::Value*> arg_vals = { rhs_val };
      return builder.CreateCall(static_cast<llvm::Function*>(lhs_val), arg_vals, "calltmp");
    }

    if (type() == Type::get_int()) {
      if (token() == "+") { return builder.CreateAdd(lhs_val, rhs_val, "addtmp"); }
      else if (token() == "-") { return builder.CreateSub(lhs_val, rhs_val, "subtmp"); }
      else if (token() == "*") { return builder.CreateMul(lhs_val, rhs_val, "multmp"); }
      else if (token() == "/") { return builder.CreateSDiv(lhs_val, rhs_val, "divtmp"); }
      else if (token() == "%") { return builder.CreateSRem(lhs_val, rhs_val, "remtmp"); }

     
      return nullptr;

    } else if (type() == Type::get_real()) {
      if (token() == "+") { return builder.CreateFAdd(lhs_val, rhs_val, "addtmp"); }
      else if (token() == "-") { return builder.CreateFSub(lhs_val, rhs_val, "subtmp"); }
      else if (token() == "*") { return builder.CreateFMul(lhs_val, rhs_val, "multmp"); }
      else if (token() == "/") { return builder.CreateFDiv(lhs_val, rhs_val, "divtmp"); }

//      auto lval = lhs_->generate_lvalue(scope);
//      if (lval == nullptr) return nullptr;
//
//      if (token() == "+=") { builder.CreateStore(builder.CreateFAdd(lhs_val, rhs_val, "addtmp"), lval); }
//      else if (token() == "-=") { builder.CreateStore(builder.CreateFSub(lhs_val, rhs_val, "subtmp"), lval); }
//      else if (token() == "*=") { builder.CreateStore(builder.CreateFMul(lhs_val, rhs_val, "multmp"), lval); }
//      else if (token() == "/=") { builder.CreateStore(builder.CreateFDiv(lhs_val, rhs_val, "divtmp"), lval); }

      return nullptr;

    } else if (type() == Type::get_uint()) {
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
    // TODO short-circuiting
    auto lhs_val = exprs_[0]->generate_code(scope);
    llvm::Value* ret_val = nullptr;

    if (exprs_[0]->type() == Type::get_int()) {
      for (size_t i = 1; i < exprs_.size(); ++i) {
        auto rhs_val = exprs_[i]->generate_code(scope);
        llvm::Value* cmp_val;

        if (ops_[i - 1]->token() == "<") {
          cmp_val = builder.CreateICmpSLT(lhs_val, rhs_val, "lttmp");

        } else if (ops_[i - 1]->token() == "<=") {
          cmp_val = builder.CreateICmpSLE(lhs_val, rhs_val, "letmp");

        } else if (ops_[i - 1]->token() == "==") {
          cmp_val = builder.CreateICmpEQ(lhs_val, rhs_val, "eqtmp");

        } else if (ops_[i - 1]->token() == "!=") {
          cmp_val = builder.CreateICmpNE(lhs_val, rhs_val, "netmp");

        } else if (ops_[i - 1]->token() == ">=") {
          cmp_val = builder.CreateICmpSGE(lhs_val, rhs_val, "getmp");

        } else if (ops_[i - 1]->token() == ">") {
          cmp_val = builder.CreateICmpSGT(lhs_val, rhs_val, "gttmp");

        } else {
          error_log.log(ops_[i - 1]->line_num(),
              "Invalid operator: " + ops_[i - 1]->token());
          return nullptr;
        }

        // TODO early exit
        ret_val = (i != 1) ? builder.CreateAnd(ret_val, cmp_val, "booltmp") : cmp_val;
        lhs_val = rhs_val;

      }
    } else if (exprs_[0]->type() == Type::get_real()) {
      for (size_t i = 1; i < exprs_.size(); ++i) {
        auto rhs_val = exprs_[i]->generate_code(scope);
        llvm::Value* cmp_val;

        if (ops_[i - 1]->token() == "<") {
          cmp_val = builder.CreateFCmpOLT(lhs_val, rhs_val, "lttmp");

        } else if (ops_[i - 1]->token() == "<=") {
          cmp_val = builder.CreateFCmpOLE(lhs_val, rhs_val, "letmp");

        } else if (ops_[i - 1]->token() == "==") {
          cmp_val = builder.CreateFCmpOEQ(lhs_val, rhs_val, "eqtmp");

        } else if (ops_[i - 1]->token() == "!=") {
          cmp_val = builder.CreateFCmpONE(lhs_val, rhs_val, "netmp");

        } else if (ops_[i - 1]->token() == ">=") {
          cmp_val = builder.CreateFCmpOGE(lhs_val, rhs_val, "getmp");

        } else if (ops_[i - 1]->token() == ">") {
          cmp_val = builder.CreateFCmpOGT(lhs_val, rhs_val, "gttmp");

        } else {
          // TODO 
          error_log.log(ops_[i - 1]->line_num(),
              "Invalid operator: " + ops_[i - 1]->token());
          return nullptr;
        }
        // TODO should these be ordered, or can they be QNAN? probably.

        // TODO early exit
        ret_val = (i != 1) ? builder.CreateAnd(ret_val, cmp_val, "booltmp") : cmp_val;
        lhs_val = rhs_val;
      }
    } else if (exprs_[0]->type() == Type::get_bool()) {
      // For boolean expression, the chain must be a single consistent operation
      // because '&', '^', and '|' all have different precedence levels.
      auto cmp_val = lhs_val;
      if (ops_.front()->token() == "^") {
        for (size_t i = 1; i < exprs_.size(); ++i) {
          auto expr = exprs_[i];
          auto rhs_val = expr->generate_code(scope);
          cmp_val = builder.CreateXor(cmp_val, rhs_val);
        }
      } else {
        auto parent_fn = builder.GetInsertBlock()->getParent();
        // Condition blocks
        std::vector<llvm::BasicBlock*> cond_blocks(ops_.size());
        for (auto& block : cond_blocks) {
          block = llvm::BasicBlock::Create(
            llvm::getGlobalContext(), "cond_block", parent_fn);
        }

        // Landing blocks
        auto land_true_block = llvm::BasicBlock::Create(
            llvm::getGlobalContext(), "land_true", parent_fn);
        auto land_false_block = llvm::BasicBlock::Create(
            llvm::getGlobalContext(), "land_false", parent_fn);
        auto merge_block = llvm::BasicBlock::Create(
            llvm::getGlobalContext(), "merge_block", parent_fn);

        if (ops_.front()->token() == "&") {
          for (size_t i = 0; i < ops_.size(); ++i) {
            builder.CreateCondBr(cmp_val, cond_blocks[i], land_false_block);
            builder.SetInsertPoint(cond_blocks[i]);
            cmp_val = exprs_[i + 1]->generate_code(scope);
          }
        } else {  // if (ops_.front()->token() == "|") {
          for (size_t i = 0; i < ops_.size(); ++i) {
            builder.CreateCondBr(cmp_val, land_true_block, cond_blocks[i]);
            builder.SetInsertPoint(cond_blocks[i]);
            cmp_val = exprs_[i + 1]->generate_code(scope);
          }
        }

        builder.CreateCondBr(cmp_val, land_true_block, land_false_block);

        builder.SetInsertPoint(land_true_block);
        builder.CreateBr(merge_block);

        builder.SetInsertPoint(land_false_block);
        builder.CreateBr(merge_block);

        builder.SetInsertPoint(merge_block);
        // Join two cases
        llvm::PHINode* phi_node = builder.CreatePHI(
            Type::get_bool()->llvm(), 2, "merge");
        phi_node->addIncoming(llvm::ConstantInt::get(llvm::getGlobalContext(),
              llvm::APInt(1, 1, false)), land_true_block);
        phi_node->addIncoming(llvm::ConstantInt::get(llvm::getGlobalContext(),
              llvm::APInt(1, 0, false)), land_false_block);
        cmp_val = phi_node;
      }
      ret_val = cmp_val;
    }

    return ret_val;
  }

  llvm::Value* FunctionLiteral::generate_code(Scope* scope) {

    if (llvm_function_ == nullptr) {
      // For now, this should never happen
      std::cout << "??? WTF ???" << std::endl;
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



    fn_scope_->set_entry(llvm::BasicBlock::Create(
          llvm::getGlobalContext(), "entry", llvm_function_));

    auto old_block = builder.GetInsertBlock();
    builder.SetInsertPoint(fn_scope_->entry());

    fn_scope_->entry()->removeFromParent();
    fn_scope_->entry()->insertInto(llvm_function_);
    fn_scope_->enter();
    
    input_iter = inputs_.begin();
    for (auto& arg : llvm_function_->args()) {
      builder.CreateStore(&arg,
          (*input_iter)->declared_identifier()->alloc_);
      ++input_iter;
    }

    statements_->generate_code(fn_scope_);
    fn_scope_->exit();

    builder.SetInsertPoint(old_block);
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
      fn->llvm_function_ = global_module->getFunction(lhs->token());
      val = rhs->generate_code(scope);
      if (val == nullptr) return nullptr;
      val->setName(lhs->token());
    } else {
      val = rhs->generate_code(scope);
      if (val == nullptr) return nullptr;

      var = lhs->generate_lvalue(scope);
      if (var == nullptr) return nullptr;

      builder.CreateStore(val, var);
    }
    return nullptr;
  }

  llvm::Value* Assignment::generate_code(Scope* scope) {

    if (token().size() == 2) {  // +=, &=, etc
      char main_op = token()[0];

      auto lhs_val = lhs_->generate_code(scope);
      if (lhs_val == nullptr) return nullptr;

      if (lhs_->type() == Type::get_bool()) {
        if (main_op == '^') {
          auto lval = lhs_->generate_lvalue(scope);
          if (lval == nullptr) return nullptr;

          auto rhs_val = rhs_->generate_code(scope);
          if (rhs_val == nullptr) return nullptr;

          builder.CreateStore(builder.CreateXor(lhs_val, rhs_val, "xortmp"), lval);
        } else {
          //TODO An optimization technique would be to only do short-circuiting
          // here if the thing we're avoiding is more expensive than the branch.

          auto parent_fn = builder.GetInsertBlock()->getParent();
          auto more_block = llvm::BasicBlock::Create(llvm::getGlobalContext(), "more", parent_fn);
          auto merge_block = llvm::BasicBlock::Create(llvm::getGlobalContext(), "merge", parent_fn);

          // Assumption is that only operators of type (bool, bool) -> bool are
          // '&', '|', and '^'
          llvm::BasicBlock* true_block = (main_op == '&' ? more_block : merge_block);
          llvm::BasicBlock* false_block = (main_op == '|' ? more_block : merge_block);

          builder.CreateCondBr(lhs_val, true_block, false_block);

          builder.SetInsertPoint(more_block);

          // Generating lvalue for storage
          auto lval = lhs_->generate_lvalue(scope);
          if (lval == nullptr) return nullptr;

          auto rhs_val = rhs_->generate_code(scope);
          if (rhs_val == nullptr) return nullptr;

          builder.CreateStore(rhs_val, lval);
          builder.CreateBr(merge_block);

          builder.SetInsertPoint(merge_block);
        }

        return nullptr;
      }

      auto lval = lhs_->generate_lvalue(scope);
      if (lval == nullptr) return nullptr;

      auto rhs_val = rhs_->generate_code(scope);
      if (rhs_val == nullptr) return nullptr;

      if (lhs_->type() == Type::get_int()) {
        llvm::Value* computed_val = nullptr;
        switch (main_op) {
          case '+': computed_val = builder.CreateAdd(lhs_val, rhs_val, "addtmp"); break;
          case '-': computed_val = builder.CreateSub(lhs_val, rhs_val, "subtmp"); break;
          case '*': computed_val = builder.CreateMul(lhs_val, rhs_val, "multmp"); break;
          case '/': computed_val = builder.CreateSDiv(lhs_val, rhs_val, "divtmp"); break;
          case '%': computed_val = builder.CreateSRem(lhs_val, rhs_val, "remtmp"); break;
          default: return nullptr;
        }
        builder.CreateStore(computed_val, lval);
        return nullptr;

      } else if (type() == Type::get_real()) {
        switch (main_op) {
          case '+': builder.CreateStore(builder.CreateFAdd(lhs_val, rhs_val, "addtmp"), lval); break;
          case '-': builder.CreateStore(builder.CreateFSub(lhs_val, rhs_val, "subtmp"), lval); break;
          case '*': builder.CreateStore(builder.CreateFMul(lhs_val, rhs_val, "multmp"), lval); break;
          case '/': builder.CreateStore(builder.CreateFDiv(lhs_val, rhs_val, "divtmp"), lval); break;
          default:;
        }
        return nullptr;
      }
    }

    return generate_assignment_code(scope, lhs_, rhs_);
  }

  llvm::Value* Declaration::generate_code(Scope* scope) {
    // Declarations are preallocated at the beginning of each scope, so there's
    // no need to do anything if there isn't also an assignment occurring
    if (!infer_type_) return nullptr;

    // Remember, decl_type_ is not really the right name in the inference case.
    // It's the thing whose type we are inferring.
    //
    // TODO change the name of this member variable to describe what it actually
    // is in both ':' and ':=" cases
    return generate_assignment_code(scope, std::static_pointer_cast<Expression>(id_), decl_type_);
  }

  llvm::Value* Case::generate_code(Scope* scope) {
    auto parent_fn = builder.GetInsertBlock()->getParent();
    // Condition blocks - The ith block is what you reach when you've
    // failed the ith condition, where conditions are labelled starting at zero.
    std::vector<llvm::BasicBlock*> case_blocks(pairs_->kv_pairs_.size() - 1);

    for (auto& block : case_blocks) {
      block = llvm::BasicBlock::Create(
          llvm::getGlobalContext(), "case_block", parent_fn);
    }

    // Landing blocks
    auto current_block = builder.GetInsertBlock();
    auto case_landing = llvm::BasicBlock::Create(
        llvm::getGlobalContext(), "case_landing", parent_fn);
    builder.SetInsertPoint(case_landing);
    llvm::PHINode* phi_node = builder.CreatePHI(type()->llvm(),
          static_cast<unsigned int>(pairs_->kv_pairs_.size()), "phi");
    builder.SetInsertPoint(current_block);

    for (size_t i = 0; i < case_blocks.size(); ++i) {
      auto cmp_val = pairs_->kv_pairs_[i].first->generate_code(scope);
      auto true_block = llvm::BasicBlock::Create(
        llvm::getGlobalContext(), "land_true", parent_fn);
 
      // If it's false, move on to the next block
      builder.CreateCondBr(cmp_val, true_block, case_blocks[i]);
      builder.SetInsertPoint(true_block);
      auto output_val = pairs_->kv_pairs_[i].second->generate_code(scope);

      // NOTE: You may be tempted to state that you are coming from the
      // block 'true_block'. However, if the code generated for the right-hand
      // side of the '=>' node is not just a single basic block, this will not
      // be the case.
      phi_node->addIncoming(output_val, builder.GetInsertBlock());
      builder.CreateBr(case_landing);

      builder.SetInsertPoint(case_blocks[i]);
    }
    auto output_val = pairs_->kv_pairs_.back().second->generate_code(scope);
    phi_node->addIncoming(output_val, builder.GetInsertBlock());
    builder.CreateBr(case_landing);
    builder.SetInsertPoint(case_landing);

    return phi_node;
  }

  llvm::Value* While::generate_code(Scope* scope) {
    auto parent_fn = builder.GetInsertBlock()->getParent();

    auto head_block = llvm::BasicBlock::Create(
        llvm::getGlobalContext(), "while_head", parent_fn);
    auto body_block = llvm::BasicBlock::Create(
        llvm::getGlobalContext(), "while_body", parent_fn);
    auto foot_block = llvm::BasicBlock::Create(
        llvm::getGlobalContext(), "while_foot", parent_fn);

    builder.CreateBr(head_block);
    builder.SetInsertPoint(head_block);
    body_scope_->set_entry(head_block);
    body_scope_->entry()->removeFromParent();
    body_scope_->entry()->insertInto(parent_fn);
    body_scope_->enter();
    builder.CreateCondBr(cond_->generate_code(scope), body_block, foot_block);

    builder.SetInsertPoint(body_block);
    statements_->generate_code(body_scope_);
    body_scope_->exit();
    builder.CreateBr(head_block);

    builder.SetInsertPoint(foot_block);

    return nullptr;
  }

  llvm::Value* Conditional::generate_code(Scope* scope) {
    auto parent_fn = builder.GetInsertBlock()->getParent();

    auto head_block = llvm::BasicBlock::Create(
        llvm::getGlobalContext(), "cond_head", parent_fn);
    auto body_block = llvm::BasicBlock::Create(
        llvm::getGlobalContext(), "cond_body", parent_fn);
    auto foot_block = llvm::BasicBlock::Create(
        llvm::getGlobalContext(), "cond_foot", parent_fn);

    builder.CreateBr(head_block);
    builder.SetInsertPoint(head_block);
    body_scope_->set_entry(head_block);
    body_scope_->entry()->removeFromParent();
    body_scope_->entry()->insertInto(parent_fn);
    body_scope_->enter();
    builder.CreateCondBr(cond_->generate_code(scope), body_block, foot_block);

    builder.SetInsertPoint(body_block);
    statements_->generate_code(body_scope_);
    builder.CreateBr(foot_block);

    body_scope_->exit();
    builder.SetInsertPoint(foot_block);

    return nullptr;
  }

}  // namespace AST
