#include "AST.h"

// TODO
// We often have if (val == nullptr) return nullptr to propogate nullptrs. In
// what situations is a nullptr actually possible to start with? If we can do
// all checks on the AST before code generation, then maybe we can remove these
// and thereby streamline the architecture.

extern ErrorLog error_log;

extern llvm::Module* global_module;
extern llvm::IRBuilder<> global_builder;

namespace builtin {
  extern llvm::Function* ascii();
}  // namespace builtin

namespace data {
  extern llvm::Value* const_true();
  extern llvm::Value* const_false();
  extern llvm::Value* const_uint(size_t n);
  extern llvm::Value* const_int(int n, bool is_signed = false);
  extern llvm::Value* const_char(char c);
  extern llvm::Value* global_string(const std::string& s);
}  // namespace data

namespace AST {
  llvm::Value* Identifier::generate_code(Scope* scope) {
    if (type()->is_function()) {
      return global_module->getFunction(token());
    }

    return scope->builder().CreateLoad(alloc_, token());
  }

  llvm::Value* Terminal::generate_code(Scope* scope) {
    // TODO Do I want to use string-to-X functions, or should I roll my own?
    //
    // The benefits are clear, but this ties me to using the same representation
    // that C++ uses.

    // Built in function of type uint -> char
    if (token() == "ascii") {
     return builtin::ascii();
    }

    // TODO move this to Type
    if (type() == Type::get_void()) {
      if (token() == "return") {
        scope->make_return_void();
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
      return data::const_int(std::stoi(token()), true);

    } else if (type() == Type::get_real()) {
      return llvm::ConstantFP::get(llvm::getGlobalContext(),
          llvm::APFloat(std::stod(token())));

    } else if (type() == Type::get_type()) {
      return nullptr;

    } else if (type() == Type::get_uint()) {
      return data::const_uint(std::stoul(token()));

    } else {
      std::cerr << "FATAL: Terminal type is not a primitive type" << std::endl;
      return nullptr;
    }
  }

  llvm::Value* Unop::generate_code(Scope* scope) {
    // TODO don't figure out what it is from the tokens.
    llvm::Value* val = expr_->generate_code(scope);

    if (token() == "-") {
      if (expr_type_ == Type::get_int()) {
        return scope->builder().CreateNeg(val);

      } else if (expr_type_ == Type::get_real()) {
        return scope->builder().CreateFNeg(val);
      }

    } else if (is_return()) {
      scope->make_return(val);

    } else if (token() == "()") {
      return scope->builder().CreateCall(static_cast<llvm::Function*>(val));

    } else if(expr_->type() == Type::get_bool() && token() == "!") {
      return scope->builder().CreateNot(val, "nottmp");

    } else if (is_print()) {
      if (expr_->type() == Type::get_type()) {
        val = data::global_string(expr_->interpret_as_type()->to_string());
      }

      scope->builder().CreateCall(expr_->type()->print(), { val });
    }

    return nullptr;
  }

  llvm::Value* Binop::generate_code(Scope* scope) {
    if (token() == "[]") {
      return scope->builder().CreateLoad(generate_lvalue(scope), "array_val");
    }

    if (token() == ".") {
      if (lhs_->type()->is_user_defined()) {
        return scope->builder().CreateLoad(generate_lvalue(scope));
      } else {
        // TODO
      }
      return nullptr;
    }


    auto lhs_val = lhs_->generate_code(scope);
    if (lhs_val == nullptr) return nullptr;

    if (token() == ":>") {
      auto from_type = lhs_->expr_type_;
      auto to_type = rhs_->interpret_as_type();
      if (from_type == to_type) {
        return lhs_val;
      }

      if (from_type == Type::get_bool()) {
        if (to_type == Type::get_int() || to_type == Type::get_uint()) {
          return scope->builder().CreateZExt(lhs_val, to_type->llvm(), "ext_val");
        } else if (to_type == Type::get_real()) {
          return scope->builder().CreateUIToFP(lhs_val, to_type->llvm(), "ext_val");
        } else {
          return nullptr;
        }

      } else if (from_type == Type::get_int()) {
        if (to_type == Type::get_real()) {
          return scope->builder().CreateSIToFP(lhs_val, to_type->llvm(), "fp_val");
        } else if (to_type == Type::get_uint()) {
          return lhs_val;
        } else {
          return nullptr;
        }

      } else if (from_type == Type::get_uint()) {
        if (to_type == Type::get_real()) {
          return scope->builder().CreateUIToFP(lhs_val, to_type->llvm(), "fp_val");
        } else if (to_type == Type::get_int()) {
          return lhs_val;
        } else {
          return nullptr;
        }
      }
    }

    if (token() == "()") {
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
        return scope->builder().CreateCall(static_cast<llvm::Function*>(lhs_val), arg_vals);
      }

      return scope->builder().CreateCall(static_cast<llvm::Function*>(lhs_val), arg_vals, "calltmp");
    }

    auto rhs_val = rhs_->generate_code(scope);
    if (rhs_val == nullptr) return nullptr;


    if (type() == Type::get_int()) {
      if (token() == "+") { return scope->builder().CreateAdd(lhs_val, rhs_val, "addtmp"); }
      else if (token() == "-") { return scope->builder().CreateSub(lhs_val, rhs_val, "subtmp"); }
      else if (token() == "*") { return scope->builder().CreateMul(lhs_val, rhs_val, "multmp"); }
      else if (token() == "/") { return scope->builder().CreateSDiv(lhs_val, rhs_val, "divtmp"); }
      else if (token() == "%") { return scope->builder().CreateSRem(lhs_val, rhs_val, "remtmp"); }

     
      return nullptr;

    } else if (type() == Type::get_uint()) {
      if (token() == "+") { return scope->builder().CreateAdd(lhs_val, rhs_val, "addtmp"); }
      else if (token() == "-") { return scope->builder().CreateSub(lhs_val, rhs_val, "subtmp"); }
      else if (token() == "*") { return scope->builder().CreateMul(lhs_val, rhs_val, "multmp"); }
      else if (token() == "/") { return scope->builder().CreateUDiv(lhs_val, rhs_val, "divtmp"); }
      else if (token() == "%") { return scope->builder().CreateURem(lhs_val, rhs_val, "remtmp"); }


      return nullptr;

    } else if (type() == Type::get_real()) {
      if (token() == "+") { return scope->builder().CreateFAdd(lhs_val, rhs_val, "addtmp"); }
      else if (token() == "-") { return scope->builder().CreateFSub(lhs_val, rhs_val, "subtmp"); }
      else if (token() == "*") { return scope->builder().CreateFMul(lhs_val, rhs_val, "multmp"); }
      else if (token() == "/") { return scope->builder().CreateFDiv(lhs_val, rhs_val, "divtmp"); }

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
          cmp_val = scope->builder().CreateICmpSLT(lhs_val, rhs_val, "lttmp");

        } else if (ops_[i - 1]->token() == "<=") {
          cmp_val = scope->builder().CreateICmpSLE(lhs_val, rhs_val, "letmp");

        } else if (ops_[i - 1]->token() == "==") {
          cmp_val = scope->builder().CreateICmpEQ(lhs_val, rhs_val, "eqtmp");

        } else if (ops_[i - 1]->token() == "!=") {
          cmp_val = scope->builder().CreateICmpNE(lhs_val, rhs_val, "netmp");

        } else if (ops_[i - 1]->token() == ">=") {
          cmp_val = scope->builder().CreateICmpSGE(lhs_val, rhs_val, "getmp");

        } else if (ops_[i - 1]->token() == ">") {
          cmp_val = scope->builder().CreateICmpSGT(lhs_val, rhs_val, "gttmp");

        } else {
          error_log.log(ops_[i - 1]->line_num(),
              "Invalid operator: " + ops_[i - 1]->token());
          return nullptr;
        }

        // TODO early exit
        ret_val = (i != 1) ? scope->builder().CreateAnd(ret_val, cmp_val, "booltmp") : cmp_val;
        lhs_val = rhs_val;

      }
    } else if (exprs_[0]->type() == Type::get_real()) {
      for (size_t i = 1; i < exprs_.size(); ++i) {
        auto rhs_val = exprs_[i]->generate_code(scope);
        llvm::Value* cmp_val;

        if (ops_[i - 1]->token() == "<") {
          cmp_val = scope->builder().CreateFCmpOLT(lhs_val, rhs_val, "lttmp");

        } else if (ops_[i - 1]->token() == "<=") {
          cmp_val = scope->builder().CreateFCmpOLE(lhs_val, rhs_val, "letmp");

        } else if (ops_[i - 1]->token() == "==") {
          cmp_val = scope->builder().CreateFCmpOEQ(lhs_val, rhs_val, "eqtmp");

        } else if (ops_[i - 1]->token() == "!=") {
          cmp_val = scope->builder().CreateFCmpONE(lhs_val, rhs_val, "netmp");

        } else if (ops_[i - 1]->token() == ">=") {
          cmp_val = scope->builder().CreateFCmpOGE(lhs_val, rhs_val, "getmp");

        } else if (ops_[i - 1]->token() == ">") {
          cmp_val = scope->builder().CreateFCmpOGT(lhs_val, rhs_val, "gttmp");

        } else {
          // TODO 
          error_log.log(ops_[i - 1]->line_num(),
              "Invalid operator: " + ops_[i - 1]->token());
          return nullptr;
        }
        // TODO should these be ordered, or can they be QNAN? probably.

        // TODO early exit
        ret_val = (i != 1) ? scope->builder().CreateAnd(ret_val, cmp_val, "booltmp") : cmp_val;
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
          cmp_val = scope->builder().CreateXor(cmp_val, rhs_val);
        }
      } else {
        auto parent_fn = scope->builder().GetInsertBlock()->getParent();
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
            scope->builder().CreateCondBr(cmp_val, cond_blocks[i], land_false_block);
            scope->builder().SetInsertPoint(cond_blocks[i]);
            cmp_val = exprs_[i + 1]->generate_code(scope);
          }
        } else {  // if (ops_.front()->token() == "|") {
          for (size_t i = 0; i < ops_.size(); ++i) {
            scope->builder().CreateCondBr(cmp_val, land_true_block, cond_blocks[i]);
            scope->builder().SetInsertPoint(cond_blocks[i]);
            cmp_val = exprs_[i + 1]->generate_code(scope);
          }
        }

        scope->builder().CreateCondBr(cmp_val, land_true_block, land_false_block);

        scope->builder().SetInsertPoint(land_true_block);
        scope->builder().CreateBr(merge_block);

        scope->builder().SetInsertPoint(land_false_block);
        scope->builder().CreateBr(merge_block);

        scope->builder().SetInsertPoint(merge_block);
        // Join two cases
        llvm::PHINode* phi_node = scope->builder().CreatePHI(
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
    auto ret_type = static_cast<Function*>(expr_type_)->return_type();
    fn_scope_->set_return_type(ret_type);

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
      if (lhs->token() == "main") {
        fn->llvm_function_ = global_module->getFunction(lhs->token());
      }
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

          scope->builder().CreateStore(scope->builder().CreateXor(lhs_val, rhs_val, "xortmp"), lval);
        } else {
          //TODO An optimization technique would be to only do short-circuiting
          // here if the thing we're avoiding is more expensive than the branch.

          auto parent_fn = scope->builder().GetInsertBlock()->getParent();
          auto more_block = llvm::BasicBlock::Create(llvm::getGlobalContext(), "more", parent_fn);
          auto merge_block = llvm::BasicBlock::Create(llvm::getGlobalContext(), "merge", parent_fn);

          // Assumption is that only operators of type (bool, bool) -> bool are
          // '&', '|', and '^'
          llvm::BasicBlock* true_block = (main_op == '&' ? more_block : merge_block);
          llvm::BasicBlock* false_block = (main_op == '|' ? more_block : merge_block);

          scope->builder().CreateCondBr(lhs_val, true_block, false_block);

          scope->builder().SetInsertPoint(more_block);

          // Generating lvalue for storage
          auto lval = lhs_->generate_lvalue(scope);
          if (lval == nullptr) return nullptr;

          auto rhs_val = rhs_->generate_code(scope);
          if (rhs_val == nullptr) return nullptr;

          scope->builder().CreateStore(rhs_val, lval);
          scope->builder().CreateBr(merge_block);

          scope->builder().SetInsertPoint(merge_block);
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
          case '+': computed_val = scope->builder().CreateAdd(lhs_val, rhs_val, "addtmp"); break;
          case '-': computed_val = scope->builder().CreateSub(lhs_val, rhs_val, "subtmp"); break;
          case '*': computed_val = scope->builder().CreateMul(lhs_val, rhs_val, "multmp"); break;
          case '/': computed_val = scope->builder().CreateSDiv(lhs_val, rhs_val, "divtmp"); break;
          case '%': computed_val = scope->builder().CreateSRem(lhs_val, rhs_val, "remtmp"); break;
          default: return nullptr;
        }
        scope->builder().CreateStore(computed_val, lval);
        return nullptr;

      } else if (lhs_->type() == Type::get_uint()) {
        llvm::Value* computed_val = nullptr;
        switch (main_op) {
          case '+': computed_val = scope->builder().CreateAdd(lhs_val, rhs_val, "addtmp"); break;
          case '-': computed_val = scope->builder().CreateSub(lhs_val, rhs_val, "subtmp"); break;
          case '*': computed_val = scope->builder().CreateMul(lhs_val, rhs_val, "multmp"); break;
          case '/': computed_val = scope->builder().CreateUDiv(lhs_val, rhs_val, "divtmp"); break;
          case '%': computed_val = scope->builder().CreateURem(lhs_val, rhs_val, "remtmp"); break;
          default: return nullptr;
        }
        scope->builder().CreateStore(computed_val, lval);
        return nullptr;

      } else if (type() == Type::get_real()) {
        switch (main_op) {
          case '+': scope->builder().CreateStore(scope->builder().CreateFAdd(lhs_val, rhs_val, "addtmp"), lval); break;
          case '-': scope->builder().CreateStore(scope->builder().CreateFSub(lhs_val, rhs_val, "subtmp"), lval); break;
          case '*': scope->builder().CreateStore(scope->builder().CreateFMul(lhs_val, rhs_val, "multmp"), lval); break;
          case '/': scope->builder().CreateStore(scope->builder().CreateFDiv(lhs_val, rhs_val, "divtmp"), lval); break;
          default:;
        }
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
      block = llvm::BasicBlock::Create(
          llvm::getGlobalContext(), "case_block", parent_fn);
    }

    // Landing blocks
    auto current_block = scope->builder().GetInsertBlock();
    auto case_landing = llvm::BasicBlock::Create(
        llvm::getGlobalContext(), "case_landing", parent_fn);
    scope->builder().SetInsertPoint(case_landing);
    llvm::PHINode* phi_node = scope->builder().CreatePHI(type()->llvm(),
          static_cast<unsigned int>(pairs_->kv_pairs_.size()), "phi");
    scope->builder().SetInsertPoint(current_block);

    for (size_t i = 0; i < case_blocks.size(); ++i) {
      auto cmp_val = pairs_->kv_pairs_[i].first->generate_code(scope);
      auto true_block = llvm::BasicBlock::Create(
        llvm::getGlobalContext(), "land_true", parent_fn);
 
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

  llvm::Value* While::generate_code(Scope* scope) {
    auto parent_fn = scope->builder().GetInsertBlock()->getParent();

    auto while_stmt_block = llvm::BasicBlock::Create(
        llvm::getGlobalContext(), "while_stmt", parent_fn);

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
      cond_blocks[i] = llvm::BasicBlock::Create(
          llvm::getGlobalContext(), "cond_block", parent_fn);
    }

    llvm::BasicBlock* landing = has_else()
      ? llvm::BasicBlock::Create(llvm::getGlobalContext(), "land", parent_fn)
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
      body_scopes_[i]->exit(landing);
    }

    scope->builder().SetInsertPoint(landing);

    return nullptr;
  }

  llvm::Value* TypeLiteral::generate_code(Scope* scope) {
    return nullptr;
  }

  llvm::Value* EnumLiteral::generate_code(Scope* scope) {
    return nullptr;
  }

  llvm::Value* Break::generate_code(Scope* scope) {
    auto scope_ptr = scope;

    auto prev_insert = scope->builder().GetInsertBlock();

    auto parent_fn = scope->builder().GetInsertBlock()->getParent();
    llvm::BasicBlock* dealloc_block = llvm::BasicBlock::Create(
        llvm::getGlobalContext(), "dealloc_block", parent_fn);
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
