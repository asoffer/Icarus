#include "AST.h"

extern llvm::Module* global_module;

namespace AST {
  llvm::Value* Identifier::generate_code(Scope* scope, llvm::IRBuilder<>& builder) {

    // TODO alloc names not auto-avoiding collisions
    return builder.CreateLoad(alloca_, token());
  }

  llvm::Value* Terminal::generate_code(Scope* scope, llvm::IRBuilder<>& builder) {
    // TODO Do I want to use string-to-X functions, or should I roll my own?
    //
    // The benefits are clear, but this ties me to using the same representation
    // that C++ uses.

    if (expr_type_ == Type::Unknown || expr_type_ == Type::TypeError) {
      return nullptr;

    } else if (expr_type_ == Type::Bool) {
      // A bool is an unsigned 1-bit integer
      return llvm::ConstantInt::get(llvm::getGlobalContext(),
          llvm::APInt(1, token() == "true" ? 1 : 0, false));

    } else if (expr_type_ == Type::Char) {
      // A character is an unsigend 8-bit integer
      return llvm::ConstantInt::get(llvm::getGlobalContext(),
          llvm::APInt(8, static_cast<unsigned int>(token()[0]), false));

    } else if (expr_type_ == Type::Int) {
      // An int is a 64-bit signed integer
      return llvm::ConstantInt::get(llvm::getGlobalContext(),
          llvm::APInt(64, std::stoul(token()), true));

    } else if (expr_type_ == Type::Real) {
      return llvm::ConstantFP::get(llvm::getGlobalContext(),
          llvm::APFloat(std::stod(token())));

    } else if (expr_type_ == Type::String) {
      // TODO String should not be a primitive type
      return nullptr; 

    } else if (expr_type_ == Type::Type_) {
      return nullptr;

    } else if (expr_type_ == Type::UInt) {
      // A uint is a 64-bit unsigned integer
      return llvm::ConstantInt::get(llvm::getGlobalContext(),
          llvm::APInt(64, std::stoul(token()), false));
    } else {
      std::cerr << "FATAL: Terminal type is not a primitive type" << std::endl;
      return nullptr;
    }
  }

  llvm::Value* Unop::generate_code(Scope* scope, llvm::IRBuilder<>& builder) {
//    llvm::Value* val = expr_->generate_code(scope, builder);
//
//    if (is_return()) {
//      builder.CreateRet(val);
//    }
//    return val;
    return nullptr;
  }

  llvm::Value* Binop::generate_code(Scope* scope, llvm::IRBuilder<>& builder) {
    llvm::Value* lhs_val = lhs_->generate_code(scope, builder);
    llvm::Value* rhs_val = rhs_->generate_code(scope, builder);

    if (lhs_val == nullptr || rhs_val == nullptr) {
      return nullptr;
    }

    if (expr_type_ == Type::Int) {
      if (token() == "+") {
      } else if (token() == "-") {
      } else if (token() == "*") {
      } else if (token() == "/") {
      }
    } else if (expr_type_ == Type::Real) {
      if (token() == "+") {
        return builder.CreateFAdd(lhs_val, rhs_val, "addtmp");

      } else if (token() == "-") {
        return builder.CreateFSub(lhs_val, rhs_val, "subtmp");

      } else if (token() == "*") {
        return builder.CreateFMul(lhs_val, rhs_val, "multmp");

      }

    } else if (expr_type_ == Type::UInt) {
    }

    return nullptr;
  }


  llvm::Value* Statements::generate_code(Scope* scope, llvm::IRBuilder<>& builder) {
    for (auto& stmt : statements_) {
      // We pre-allocate declarations at the beginning of each block, so we
      // don't need to do that here.
      if (stmt->is_declaration()) continue;

      stmt->generate_code(scope, builder);
    }
    return nullptr;
  }

  llvm::Value* ChainOp::generate_code(Scope* scope, llvm::IRBuilder<>& builder) {
    return nullptr;
  }

  llvm::Value* FunctionLiteral::generate_code(Scope* scope, llvm::IRBuilder<>& builder) {
    return nullptr;
  }

  llvm::Value* Assignment::generate_code(Scope* scope, llvm::IRBuilder<>& builder) {
    llvm::Value* var;
    llvm::Value* val;

    val = rhs_->generate_code(scope, builder);
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
      var = lhs_->generate_code(scope, builder);
    }
    if (var == nullptr) return nullptr;

    builder.CreateStore(val, var);

    return nullptr;
  }

  llvm::Value* Declaration::generate_code(Scope* scope, llvm::IRBuilder<>& builder) {
    return nullptr;
  }

  llvm::Value* Case::generate_code(Scope* scope, llvm::IRBuilder<>& builder) {
    return nullptr;
  }
}  // namespace AST
