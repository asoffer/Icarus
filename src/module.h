#ifndef ICARUS_MODULE_H
#define ICARUS_MODULE_H

#include <memory>
#include <queue>
#include <string>
#include <unordered_set>
#include "base/container/vector.h"

#include "ast/bound_constants.h"
#include "ast/node_lookup.h"
#include "ast/statements.h"
#include "scope.h"

#ifdef ICARUS_USE_LLVM
namespace llvm {
class Module;
class LLVMContext;
}  // namespace llvm
#endif  // ICARUS_USE_LLVM

namespace type {
struct Type;
struct Function;
}  // namespace type

namespace IR {
struct Func;
}  // namespace IR

namespace AST {
struct Expression;
struct GeneratedFunction;
}  // namespace AST

struct Module {
  Module();
  ~Module();

  // We take pointers to the module, so it cannot be moved.
  Module(Module &&) = delete;

  static std::unique_ptr<Module> Compile(const frontend::Source::Name &src);

  IR::Func *AddFunc(
      AST::GeneratedFunction *fn_lit, type::Function const *fn_type,
      base::vector<std::pair<std::string, AST::Expression *>> args);
  IR::Func *AddFunc(
      const type::Function *fn_type,
      base::vector<std::pair<std::string, AST::Expression *>> args);
  const type::Type *GetType(const std::string &name) const;
  AST::Declaration *GetDecl(const std::string &name) const;

  void Complete();

  std::queue<AST::GeneratedFunction *> to_complete_;
  std::unique_ptr<DeclScope> global_;

  // Holds all constants defined in the module (both globals and scoped
  // constants).
  AST::BoundConstants bound_constants_;

  // TODO long-term this is not a good way to store these. We should probably
  // extract the declarations determine which are public, etc.
  AST::Statements statements_;

#ifdef ICARUS_USE_LLVM
  std::unique_ptr<llvm::LLVMContext> llvm_ctx_;
  std::unique_ptr<llvm::Module> llvm_;
#endif  // ICARUS_USE_LLVM

  base::vector<std::unique_ptr<IR::Func>> fns_;
  std::unordered_set<const Module *> embedded_modules_;

  void set_type(AST::BoundConstants const &bc, AST::Expression const *expr,
                type::Type const *);
  type::Type const *type_of(AST::BoundConstants const &bc,
                            AST::Expression const *expr) const;
  std::map<AST::BoundConstants, AST::NodeLookup<type::Type const *>> types_;
};

#endif  // ICARUS_MODULE_H
