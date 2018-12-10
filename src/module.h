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

namespace ir {
struct Func;
}  // namespace ir

namespace ast {
struct Expression;
struct FunctionLiteral;
}  // namespace ast

struct Module {
  Module();
  ~Module();

  // We take pointers to the module, so it cannot be moved.
  Module(Module &&) = delete;

  static std::unique_ptr<Module> Compile(const frontend::Source::Name &src);

  ir::Func *AddFunc(
      type::Function const *fn_type,
      base::vector<std::pair<std::string, ast::Expression *>> args);
  type::Type const *GetType(std::string const &name) const;
  ast::Declaration *GetDecl(std::string const &name) const;

  std::map<ast::BoundConstants, std::unordered_set<ast::Expression const *>>
      completed_;

  struct CompilationWorkItem {
    CompilationWorkItem(ast::BoundConstants bc, ast::Expression *e, Module *mod)
        : bound_constants_(std::move(bc)), expr_(e), mod_(mod) {}

    void Complete();

    ast::BoundConstants bound_constants_;
    ast::Expression *expr_;
    Module *mod_;
  };
  std::queue<CompilationWorkItem> to_complete_;
  void CompleteAll();

  std::unique_ptr<DeclScope> global_;

  // Holds all constants defined in the module (both globals and scoped
  // constants). These are the values in the map. They're keyed on conditional
  // constants. So we have options for mulitple meanings of things depending on
  // context.
  //
  // TODO Almost surely this needs to be even deeper, treating it as a tree
  // of arbitrary depth.
  base::map<ast::BoundConstants, ast::BoundConstants> constants_;

  // TODO long-term this is not a good way to store these. We should probably
  // extract the declarations determine which are public, etc.
  ast::Statements statements_;

#ifdef ICARUS_USE_LLVM
  std::unique_ptr<llvm::LLVMContext> llvm_ctx_;
  std::unique_ptr<llvm::Module> llvm_;
#endif  // ICARUS_USE_LLVM

  base::vector<std::unique_ptr<ir::Func>> fns_;

  type::Type const *set_type(ast::BoundConstants const &bc,
                             ast::Expression const *expr, type::Type const *);
  type::Type const *type_of(ast::BoundConstants const &bc,
                            ast::Expression const *expr) const;
  ir::Register addr(ast::BoundConstants const &bc,
                    ast::Declaration *decl) const;
  std::map<ast::BoundConstants, ast::NodeLookup<type::Type const *>> types_;
  std::map<ast::BoundConstants,
           base::unordered_map<ast::Declaration *, ir::Register>>
      addr_;
  std::map<ast::BoundConstants,
           std::unordered_set<ast::FunctionLiteral const *>>
      validated_;
  std::map<ast::BoundConstants,
           base::unordered_map<ast::Expression const *, ir::Func *>>
      ir_funcs_;
};

#endif  // ICARUS_MODULE_H
