#include "AST.h"

extern llvm::Module* global_module;
extern llvm::IRBuilder<> builder;

namespace cstdlib {
  extern llvm::Constant* malloc;
}

namespace AST {
  llvm::Value* Identifier::generate_lvalue(Scope* scope) {
    if (alloc_ == nullptr) {
      if (type()->is_array()) {
        auto array_type = std::static_pointer_cast<ArrayType>(scope->get_declared_type(this));

        auto type_as_array = static_cast<Array*>(type());

        if (array_type->len_ != nullptr) {
          auto array_len = array_type->len_->generate_code(scope);
          auto bytes_per_elem = llvm::ConstantInt::get(llvm::getGlobalContext(),
              llvm::APInt(32, type()->bytes(), false));

          auto bytes_needed = builder.CreateMul(array_len, bytes_per_elem, "malloc_bytes");
          auto ptr_as_i8 = builder.CreateCall(cstdlib::malloc, { bytes_needed }, "array_ptr");

          auto ptr_to_mem = builder.CreateBitCast(ptr_as_i8,
              Type::get_pointer(type_as_array->type_)->llvm(), "array_ptr");

          alloc_ = builder.CreateAlloca(Type::get_pointer(type_as_array->type_)->llvm(), nullptr, token());
          builder.CreateStore(ptr_to_mem, alloc_);

          bytes_needed->dump();
        }

      }
    }
    return alloc_;
  } 

  // TODO
  llvm::Value* Unop::generate_lvalue(Scope*) { return nullptr; }
  llvm::Value* ChainOp::generate_lvalue(Scope*) { return nullptr; }
  llvm::Value* ArrayType::generate_lvalue(Scope*) { return nullptr; }
  llvm::Value* Terminal::generate_lvalue(Scope*) { return nullptr; }
  llvm::Value* FunctionLiteral::generate_lvalue(Scope*) { return nullptr; }
  llvm::Value* Case::generate_lvalue(Scope*) { return nullptr; }
  llvm::Value* Assignment::generate_lvalue(Scope*) { return nullptr; }
  llvm::Value* Declaration::generate_lvalue(Scope*) { return nullptr; }


  llvm::Value* Binop::generate_lvalue(Scope* scope) {
    if (token() == "[]" && lhs_->type()->is_array()) {
      auto lhs_val = lhs_->generate_lvalue(scope);
      auto rhs_val = rhs_->generate_code(scope);
      auto load_ptr = builder.CreateLoad(lhs_val);
      return builder.CreateGEP(type()->llvm(), load_ptr, { rhs_val }, "array_idx");
    }
    return nullptr;
  }

//
}  // namespace AST
