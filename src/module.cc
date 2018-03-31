#include "module.h"

#include "ir/func.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

// Can't declare this in header because unique_ptr's destructor needs to know
// the size of IR::Func which we want to forward declare.
Module::Module()
    : llvm_ctx_(std::make_unique<llvm::LLVMContext>()),
      llvm_(std::make_unique<llvm::Module>("my module", *llvm_ctx_)) {}
Module::~Module() = default;
Module::Module(Module&&) = default;

IR::Func* Module::AddFunc(
    const type::Function* fn_type,
    std::vector<std::pair<std::string, AST::Expression*>> args) {
  fns_.push_back(std::make_unique<IR::Func>(fn_type, std::move(args)));
  return fns_.back().get();
}

const type::Type* Module::GetType(const std::string& name) const {
  // TODO
  return type::Err;
}
