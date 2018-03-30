#ifndef ICARUS_MODULE_H
#define ICARUS_MODULE_H

#include <memory>
#include <vector>
#include <string>

#include "llvm/IR/Module.h"

namespace type {
struct Function;
}  // namespace type

namespace IR {
struct Func;
}  // namespace IR

namespace AST {
struct Expression;
}  // namespace AST

struct Module {
  Module();
  ~Module();

  IR::Func* AddFunc(const type::Function* fn_type,
      std::vector<std::pair<std::string, AST::Expression *>> args);

  llvm::LLVMContext llvm_ctx_;
  llvm::Module llvm_;
  std::vector<std::unique_ptr<IR::Func>> fns_;
};

#endif // ICARUS_MODULE_H
