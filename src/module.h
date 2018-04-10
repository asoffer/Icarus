#ifndef ICARUS_MODULE_H
#define ICARUS_MODULE_H

#include <queue>
#include <memory>
#include <vector>
#include <string>

#include "ast/statements.h"
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
struct FunctionLiteral;
}  // namespace AST

struct Module {
  Module();
  ~Module();
  Module(Module&&) = delete;

  IR::Func* AddFunc(const type::Function* fn_type,
      std::vector<std::pair<std::string, AST::Expression *>> args);
  const type::Type* GetType(const std::string& name) const;
  AST::Declaration* GetDecl(const std::string& name) const;

  void Complete();

  std::queue<AST::FunctionLiteral*> to_complete_;
  std::unique_ptr<DeclScope> global_;

  // TODO long-term this is not a good way to store these. We should probably
  // extract the declarations determine which are public, etc.
  AST::Statements statements_;

  std::unique_ptr<llvm::LLVMContext> llvm_ctx_;
  std::unique_ptr<llvm::Module> llvm_;
  std::vector<std::unique_ptr<IR::Func>> fns_;
  std::vector<const Module*> embedded_modules_;
};

#endif // ICARUS_MODULE_H
