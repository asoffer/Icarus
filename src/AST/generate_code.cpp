#include "AST.h"

// TODO
// We often have if (val == nullptr) return nullptr to propogate nullptrs. In
// what situations is a nullptr actually possible to start with? If we can do
// all checks on the AST before code generation, then maybe we can remove these
// and thereby streamline the architecture.

extern llvm::Module* global_module;
extern llvm::IRBuilder<> builder;
extern llvm::Function* global_print_char;

namespace AST {
  llvm::Value* Identifier::generate_code(Scope* scope) {
    if (type()->is_function()) {
      return global_module->getFunction(token());
    }

    return builder.CreateLoad(alloca_, token());
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
      if (expr_->type() == Type::get_char()) {
        builder.CreateCall(global_print_char, { val });
      }
    }

    return nullptr;
  }

  llvm::Value* Binop::generate_code(Scope* scope) {
    if (token() == "()") {
      llvm::Value* lhs_val = lhs_->generate_code(scope);
      llvm::Value* rhs_val = rhs_->generate_code(scope);

      if (lhs_val == nullptr || rhs_val == nullptr) {
        return nullptr;
      }

      // TODO multiple arguments
      std::vector<llvm::Value*> arg_vals = { rhs_val };
      return builder.CreateCall(static_cast<llvm::Function*>(lhs_val), arg_vals, "calltmp");
    }

    if (type() == Type::get_bool()) {
      llvm::Value* lhs_val = lhs_->generate_code(scope);
      if (lhs_val == nullptr) return nullptr;

      // TODO this check can be sped up, at the cost of debugging robustness
      if (token() == "&" || token() == "|" || token() == "&=") {
        auto parent_fn = builder.GetInsertBlock()->getParent();

        auto more_block = llvm::BasicBlock::Create(
            llvm::getGlobalContext(), "more", parent_fn);
        auto merge_block = llvm::BasicBlock::Create(
            llvm::getGlobalContext(), "merge", parent_fn);
        auto short_circuit_entry = builder.GetInsertBlock();

        llvm::BasicBlock* true_block = nullptr;
        llvm::BasicBlock* false_block = nullptr;

        if (token() == "&" || token() == "&=") {
          true_block = more_block;
          false_block = merge_block;
 
        } else if (token() == "|" || token() == "|=") {
          false_block = more_block;
          true_block = merge_block;
        }

        builder.CreateCondBr(lhs_val, true_block, false_block);

        builder.SetInsertPoint(more_block);
        llvm::Value* rhs_val = rhs_->generate_code(scope);
        builder.CreateBr(merge_block);

        builder.SetInsertPoint(merge_block);
        llvm::PHINode* phi_node =
          builder.CreatePHI(Type::get_bool()->llvm(), 2, "merge");

        if (rhs_val == nullptr) return nullptr;

        if (token() == "&" || token() == "&=") {
          phi_node->addIncoming(rhs_val, more_block);
          phi_node->addIncoming(
              llvm::ConstantInt::get(llvm::getGlobalContext(),
                llvm::APInt(1, 0, false)), short_circuit_entry);

        } else if (token() == "|" || token() == "|=") {
          phi_node->addIncoming(
              llvm::ConstantInt::get(llvm::getGlobalContext(),
                llvm::APInt(1, 1, false)), short_circuit_entry);
          phi_node->addIncoming(rhs_val, more_block);
        }

        if (token() == "&=" || token() == "|=") {
          llvm::AllocaInst* var = nullptr;
          if (lhs_->is_identifier()) {
            auto id_ptr = std::static_pointer_cast<Identifier>(lhs_);
            var = id_ptr->alloca_;
          }

          // TODO remove this/robustify it
          if (var != nullptr) {
            builder.CreateStore(phi_node, var);
          }

          // Assignment versions do not return anything
          return nullptr;
        }

        return phi_node;

      }

      // Already defined lhs_val
      llvm::Value* rhs_val = rhs_->generate_code(scope);
      if (rhs_val == nullptr) {
        return nullptr;
      }

      if (token() == "^") {
        return builder.CreateXor(lhs_val, rhs_val, "xortmp");

      } else if (token() == "^=") {
        llvm::AllocaInst* var = nullptr;
        if (lhs_->is_identifier()) {
          auto id_ptr = std::static_pointer_cast<Identifier>(lhs_);
          var = id_ptr->alloca_;
        }

        // TODO remove this/robustify it
        if (var == nullptr) return nullptr;

        builder.CreateStore(builder.CreateXor(lhs_val, rhs_val, "subtmp"), var);
        return nullptr;
      }

    }

    llvm::Value* lhs_val = lhs_->generate_code(scope);
    llvm::Value* rhs_val = rhs_->generate_code(scope);

    if (lhs_val == nullptr || rhs_val == nullptr) {
      return nullptr;
    }

    if (type() == Type::get_int()) {
      if (token() == "+") {
        return builder.CreateAdd(lhs_val, rhs_val, "addtmp");

      } else if (token() == "-") {
        return builder.CreateSub(lhs_val, rhs_val, "subtmp");

      } else if (token() == "*") {
        return builder.CreateMul(lhs_val, rhs_val, "multmp");

      } else if (token() == "/") {
        return builder.CreateSDiv(lhs_val, rhs_val, "divtmp");

      } else if (token() == "%") {
        return builder.CreateSRem(lhs_val, rhs_val, "remtmp");

      } else if (token() == "+=") {
        llvm::AllocaInst* var = nullptr;
        if (lhs_->is_identifier()) {
          auto id_ptr = std::static_pointer_cast<Identifier>(lhs_);
          var = id_ptr->alloca_;
        }

        // TODO remove this/robustify it
        if (var == nullptr) return nullptr;

        builder.CreateStore(builder.CreateAdd(lhs_val, rhs_val, "addtmp"), var);
        return nullptr;

      } else if (token() == "-=") {
        llvm::AllocaInst* var = nullptr;
        if (lhs_->is_identifier()) {
          auto id_ptr = std::static_pointer_cast<Identifier>(lhs_);
          var = id_ptr->alloca_;
        }

        // TODO remove this/robustify it
        if (var == nullptr) return nullptr;

        builder.CreateStore(builder.CreateSub(lhs_val, rhs_val, "subtmp"), var);
        return nullptr;

      } else if (token() == "*=") {
        llvm::AllocaInst* var = nullptr;
        if (lhs_->is_identifier()) {
          auto id_ptr = std::static_pointer_cast<Identifier>(lhs_);
          var = id_ptr->alloca_;
        }

        // TODO remove this/robustify it
        if (var == nullptr) return nullptr;

        builder.CreateStore(builder.CreateMul(lhs_val, rhs_val, "multmp"), var);
        return nullptr;

      } else if (token() == "/=") {
        llvm::AllocaInst* var = nullptr;
        if (lhs_->is_identifier()) {
          auto id_ptr = std::static_pointer_cast<Identifier>(lhs_);
          var = id_ptr->alloca_;
        }

        // TODO remove this/robustify it
        if (var == nullptr) return nullptr;

        builder.CreateStore(builder.CreateSDiv(lhs_val, rhs_val, "divtmp"), var);
        return nullptr;
      
      } else if (token() == "%=") {
        llvm::AllocaInst* var = nullptr;
        if (lhs_->is_identifier()) {
          auto id_ptr = std::static_pointer_cast<Identifier>(lhs_);
          var = id_ptr->alloca_;
        }

        // TODO remove this/robustify it
        if (var == nullptr) return nullptr;

        builder.CreateStore(builder.CreateSRem(lhs_val, rhs_val, "remtmp"), var);
        return nullptr;
      }

    } else if (type() == Type::get_real()) {
      if (token() == "+") {
        return builder.CreateFAdd(lhs_val, rhs_val, "addtmp");

      } else if (token() == "-") {
        return builder.CreateFSub(lhs_val, rhs_val, "subtmp");

      } else if (token() == "*") {
        return builder.CreateFMul(lhs_val, rhs_val, "multmp");

      } else if (token() == "/") {
        return builder.CreateFDiv(lhs_val, rhs_val, "divtmp");

      } else if (token() == "+=") {
        llvm::AllocaInst* var = nullptr;
        if (lhs_->is_identifier()) {
          auto id_ptr = std::static_pointer_cast<Identifier>(lhs_);
          var = id_ptr->alloca_;
        }

        // TODO remove this/robustify it
        if (var == nullptr) return nullptr;

        builder.CreateStore(builder.CreateFAdd(lhs_val, rhs_val, "addtmp"), var);
        return nullptr;

      } else if (token() == "-=") {
        llvm::AllocaInst* var = nullptr;
        if (lhs_->is_identifier()) {
          auto id_ptr = std::static_pointer_cast<Identifier>(lhs_);
          var = id_ptr->alloca_;
        }

        // TODO remove this/robustify it
        if (var == nullptr) return nullptr;

        builder.CreateStore(builder.CreateFSub(lhs_val, rhs_val, "subtmp"), var);
        return nullptr;

      } else if (token() == "*=") {
        llvm::AllocaInst* var = nullptr;
        if (lhs_->is_identifier()) {
          auto id_ptr = std::static_pointer_cast<Identifier>(lhs_);
          var = id_ptr->alloca_;
        }

        // TODO remove this/robustify it
        if (var == nullptr) return nullptr;

        builder.CreateStore(builder.CreateFMul(lhs_val, rhs_val, "multmp"), var);
        return nullptr;

      } else if (token() == "/=") {
        llvm::AllocaInst* var = nullptr;
        if (lhs_->is_identifier()) {
          auto id_ptr = std::static_pointer_cast<Identifier>(lhs_);
          var = id_ptr->alloca_;
        }

        // TODO remove this/robustify it
        if (var == nullptr) return nullptr;

        builder.CreateStore(builder.CreateFDiv(lhs_val, rhs_val, "divtmp"), var);
        return nullptr;
      }



    } else if (type() == Type::get_uint()) {
    }


    return nullptr;
  }


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
    fn_scope_->allocate();
    
    // Because all variables in this scope, including inputs, are allocated, we
    // go through the inputs and store in the allocated location. While this is
    // not an optimized way to do things, we trust mem2reg to optimize this
    // away.
    //
    // TODO implement a mem2reg pass.

    input_iter = inputs_.begin();
    for (auto& arg : llvm_function_->args()) {
      builder.CreateStore(&arg,
          (*input_iter)->declared_identifier()->alloca_);
      ++input_iter;
    }

    statements_->generate_code(fn_scope_);

    builder.SetInsertPoint(fn_scope_->entry());

    builder.SetInsertPoint(old_block);
    return llvm_function_;
  }

  llvm::Value* generate_assignment_code(Scope* scope, EPtr lhs, EPtr rhs) {
    llvm::Value* var = nullptr;
    llvm::Value* val = nullptr;

    if (lhs->is_identifier()) {
      // Treat functions special
      if (rhs->type()->is_function()) {
        auto fn = std::static_pointer_cast<FunctionLiteral>(rhs);
        fn->llvm_function_ = global_module->getFunction(lhs->token());
        val = rhs->generate_code(scope);
        if (val == nullptr) return nullptr;
        val->setName(lhs->token());

        return nullptr;
      }

      val = rhs->generate_code(scope);
      if (val == nullptr) return nullptr;

      auto id_ptr = std::static_pointer_cast<Identifier>(lhs);
      var = id_ptr->alloca_;
    } else {
      val = rhs->generate_code(scope);
      if (val == nullptr) return nullptr;

      // TODO This situation could also come up for instance if I assign through
      // a pointer (or any lvalue that isnt an identifier.
      // Example:
      //   x : int
      //   y := &x
      //   @y = 3  // <--- HERE
      var = lhs->generate_code(scope);
    }

    if (var == nullptr) return nullptr;
    builder.CreateStore(val, var);

    return nullptr;
  }

  llvm::Value* Assignment::generate_code(Scope* scope) {
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
    auto landing_block = llvm::BasicBlock::Create(
        llvm::getGlobalContext(), "case_landing", parent_fn);
    auto start_block = builder.GetInsertBlock();
    builder.SetInsertPoint(landing_block);
    llvm::PHINode* phi_node =
      builder.CreatePHI(type()->llvm(), static_cast<unsigned int>(pairs_->size()), "phi");

    builder.SetInsertPoint(start_block);

    auto countdown = pairs_->size() - 1;
    auto iter = pairs_->kv_pairs_.begin();

    while (countdown != 0) {
      auto bool_val = iter->first->generate_code(scope);
      if (bool_val == nullptr) return nullptr;

      auto true_block = llvm::BasicBlock::Create(
          llvm::getGlobalContext(), "case_true", parent_fn);
      auto next_block = llvm::BasicBlock::Create(
          llvm::getGlobalContext(), "case_next", parent_fn);

      builder.CreateCondBr(bool_val, true_block, next_block);

      // Need to set the insert point to true_block so that when we generate
      // code for the true case, it gets put in that block.
      builder.SetInsertPoint(true_block);
      phi_node->addIncoming(iter->second->generate_code(scope), true_block);
      builder.CreateBr(landing_block);

      builder.SetInsertPoint(next_block);

      ++iter; --countdown;
    }

    // Unroll the last level of the loop because it must end with
    //   else => ...
    // And this is a special case. When we get down here we are already pointing
    // to the correct block, and iter is already pointing to the right value.
    phi_node->addIncoming(iter->second->generate_code(scope), builder.GetInsertBlock());
    builder.CreateBr(landing_block);

    // Ensure that we end pointing to the right place
    builder.SetInsertPoint(landing_block);

    return phi_node;
  }
}  // namespace AST
