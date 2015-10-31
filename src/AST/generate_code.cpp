#include "AST.h"

extern llvm::Module* global_module;
extern llvm::IRBuilder<> builder;

namespace AST {
  llvm::Value* Identifier::generate_code(Scope* scope) {
    return builder.CreateLoad(alloca_, token());
  }

  llvm::Value* Terminal::generate_code(Scope* scope) {
    // TODO Do I want to use string-to-X functions, or should I roll my own?
    //
    // The benefits are clear, but this ties me to using the same representation
    // that C++ uses.

    // TODO move this to Type
    if (expr_type_ == Type::get_unknown() || expr_type_ == Type::get_type_error()) {
      return nullptr;

    } else if (expr_type_ == Type::get_bool()) {
      // A bool is an unsigned 1-bit integer
      return llvm::ConstantInt::get(llvm::getGlobalContext(),
          llvm::APInt(1, token() == "true" ? 1 : 0, false));

    } else if (expr_type_ == Type::get_char()) {
      // A character is an unsigend 8-bit integer
      return llvm::ConstantInt::get(llvm::getGlobalContext(),
          llvm::APInt(8, static_cast<unsigned int>(token()[0]), false));

    } else if (expr_type_ == Type::get_int()) {
      // An int is a 32-bit signed integer
      return llvm::ConstantInt::get(llvm::getGlobalContext(),
          llvm::APInt(32, std::stoul(token()), true));

    } else if (expr_type_ == Type::get_real()) {
      return llvm::ConstantFP::get(llvm::getGlobalContext(),
          llvm::APFloat(std::stod(token())));

    } else if (expr_type_ == Type::get_type()) {
      return nullptr;

    } else if (expr_type_ == Type::get_uint()) {
      // A uint is a 64-bit unsigned integer
      return llvm::ConstantInt::get(llvm::getGlobalContext(),
          llvm::APInt(32, std::stoul(token()), false));
    } else {
      std::cerr << "FATAL: Terminal type is not a primitive type" << std::endl;
      return nullptr;
    }
  }

  llvm::Value* Unop::generate_code(Scope* scope) {
    llvm::Value* val = expr_->generate_code(scope);

    if (is_return()) {
      builder.CreateRet(val);

    } else if(expr_->expr_type_ == Type::get_bool() && token() == "!") {
      return builder.CreateNot(val, "nottmp");

    } else if (is_print()) {
      // builder.CreateCall(global_printd, val, "print");
    }

    return nullptr;
  }

  llvm::Value* Binop::generate_code(Scope* scope) {
    llvm::Value* lhs_val = lhs_->generate_code(scope);
    llvm::Value* rhs_val = rhs_->generate_code(scope);

    if (lhs_val == nullptr || rhs_val == nullptr) {
      return nullptr;
    }

    if (expr_type_ == Type::get_bool()) {
      if (token() == "&") {
        return builder.CreateAnd(lhs_val, rhs_val, "andtmp");

      } else if (token() == "|") {
        return builder.CreateOr(lhs_val, rhs_val, "ortmp");

      } else if (token() == "^") {
        return builder.CreateXor(lhs_val, rhs_val, "xortmp");

      } else if (token() == "==") {
        return builder.CreateICmpEQ(lhs_val, rhs_val, "eqtmp");

      } else if (token() == "!=") {
        return builder.CreateXor(lhs_val, rhs_val, "netmp");

      } else if (token() == "&=") {
        llvm::AllocaInst* var = nullptr;
        if (lhs_->is_identifier()) {
          auto id_ptr = std::static_pointer_cast<Identifier>(lhs_);
          var = id_ptr->alloca_;
        }

        // TODO remove this/robustify it
        if (var == nullptr) return nullptr;

        builder.CreateStore(builder.CreateAnd(lhs_val, rhs_val, "subtmp"), var);
        return nullptr;

      } else if (token() == "|=") {
        return builder.CreateOr(lhs_val, rhs_val, "ortmp");
        llvm::AllocaInst* var = nullptr;
        if (lhs_->is_identifier()) {
          auto id_ptr = std::static_pointer_cast<Identifier>(lhs_);
          var = id_ptr->alloca_;
        }

        // TODO remove this/robustify it
        if (var == nullptr) return nullptr;

        builder.CreateStore(builder.CreateOr(lhs_val, rhs_val, "subtmp"), var);
        return nullptr;

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

    if (expr_type_ == Type::get_int()) {
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

    } else if (expr_type_ == Type::get_real()) {
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



    } else if (expr_type_ == Type::get_uint()) {
    }

    return nullptr;
  }


  llvm::Value* Statements::generate_code(Scope* scope) {
    for (auto& stmt : statements_) {
      // We pre-allocate declarations at the beginning of each block, so we
      // don't need to do that here.
      if (stmt->is_declaration()) continue;

      stmt->generate_code(scope);
    }
    return nullptr;
  }

  llvm::Value* ChainOp::generate_code(Scope* scope) {
    auto lhs_val = exprs_[0]->generate_code(scope);
    llvm::Value* ret_val = nullptr;

    if (exprs_[0]->expr_type_ == Type::get_int()) {
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
          std::cerr << "Invalid operator: " << ops_[i - 1];
          return nullptr;
        }

        // TODO early exit
        ret_val = (i != 1) ? builder.CreateAnd(ret_val, cmp_val, "booltmp") : cmp_val;
        lhs_val = rhs_val;

      }
    } else if (exprs_[0]->expr_type_ == Type::get_real()) {
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
          std::cerr << "Invalid operator: " << ops_[i - 1];
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

    llvm_function_ = llvm::Function::Create(
        static_cast<llvm::FunctionType*>(expr_type_->llvm()),
        llvm::Function::ExternalLinkage, "__anon_fn", global_module);


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

  llvm::Value* Assignment::generate_code(Scope* scope) {
    llvm::Value* var;
    llvm::Value* val;

    val = rhs_->generate_code(scope);
    if (val == nullptr) return nullptr;

    if (lhs_->is_identifier()) {

      // Treat functions special
      if (rhs_->type()->is_function()) {
        val->setName(lhs_->token());
        return nullptr;
      }

      auto id_ptr = std::static_pointer_cast<Identifier>(lhs_);
      var = id_ptr->alloca_;
    } else {
      // TODO This situation could also come up for instance if I assign through
      // a pointer (or any lvalue that isnt an identifier.
      // Example:
      //   x : int
      //   y : &x
      //   @y = 3  // <--- HERE
      var = lhs_->generate_code(scope);
    }

    if (var == nullptr) return nullptr;
    builder.CreateStore(val, var);

    return nullptr;
  }

  llvm::Value* Declaration::generate_code(Scope* scope) {
    return nullptr;
  }

  llvm::Value* Case::generate_code(Scope* scope) {
    return nullptr;
  }
}  // namespace AST
