#ifndef ICARUS_MODULE_H
#define ICARUS_MODULE_H

#include <memory>
#include <vector>
#include <string>

#include "scope.h"

namespace llvm {
class Module;
class LLVMContext;
}  // namespace llvm

namespace type {
struct Type;
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
  Module(Module&&);

  IR::Func* AddFunc(const type::Function* fn_type,
      std::vector<std::pair<std::string, AST::Expression *>> args);
  const type::Type* GetType(const std::string& name) const;

  DeclScope global_{nullptr};

  std::unique_ptr<llvm::LLVMContext> llvm_ctx_;
  std::unique_ptr<llvm::Module> llvm_;
  std::vector<std::unique_ptr<IR::Func>> fns_;
};

#endif // ICARUS_MODULE_H
