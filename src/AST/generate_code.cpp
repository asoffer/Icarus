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
    }

    return nullptr;
  }

  llvm::Value* Binop::generate_code(Scope* scope) {
    llvm::Value* lhs_val = lhs_->generate_code(scope);
    llvm::Value* rhs_val = rhs_->generate_code(scope);

    if (lhs_val == nullptr || rhs_val == nullptr) {
      return nullptr;
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
        return builder.CreateSRem(lhs_val, rhs_val, "divrem");

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
    return nullptr;
  }

  llvm::Value* FunctionLiteral::generate_code(Scope* scope) {
    builder.SetInsertPoint(fn_scope_->entry());

    auto fn_type = Type::get_function(Type::get_real(), Type::get_real());

    llvm::Function* fn = llvm::Function::Create(
        fn_type->llvm(),
        llvm::Function::ExternalLinkage, "__global_function", nullptr);

    fn_scope_->entry()->removeFromParent();
    fn_scope_->entry()->insertInto(fn);
    fn_scope_->allocate();

    statements_->generate_code(fn_scope_);

    fn->dump();

    builder.SetInsertPoint(scope->entry());
    return fn;
  }

  llvm::Value* Assignment::generate_code(Scope* scope) {
    llvm::Value* var;
    llvm::Value* val;

    val = rhs_->generate_code(scope);
    if (val == nullptr) return nullptr;

    if (lhs_->is_identifier()) {
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
