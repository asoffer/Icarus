#ifndef ICARUS_COMPILER_COMPILER_H
#define ICARUS_COMPILER_COMPILER_H

#include <atomic>
#include <memory>

#include "absl/container/node_hash_map.h"
#include "ast/ast_fwd.h"
#include "ast/visitor.h"
#include "base/debug.h"
#include "base/guarded.h"
#include "base/move_func.h"
#include "base/tag.h"
#include "compiler/constant_binding.h"
#include "compiler/dependent_data.h"
#include "compiler/verify_result.h"
#include "error/log.h"
#include "frontend/source/source.h"
#include "ir/addr.h"
#include "ir/builder.h"
#include "ir/reg.h"
#include "ir/results.h"
#include "module/module.h"
#include "type/type_fwd.h"
#include "type/visitor.h"

namespace ir {
struct CompiledFn;
struct ScopeDef;
struct BlockDef;
}  // namespace ir

namespace compiler {
struct EmitRefTag {};
struct EmitCopyInitTag {};
struct EmitMoveInitTag {};
struct EmitValueTag {};
struct VerifyTypeTag {};
struct EmitPrintTag {};
struct EmitDestroyTag {};
struct EmitDefaultInitTag {};
struct EmitCopyAssignTag {};
struct EmitMoveAssignTag {};
std::unique_ptr<module::BasicModule> CompileModule(frontend::Source *src);

// These are the steps in a traditional compiler of verifying types and emitting
// code. They're tied together because they don't necessarily happen in a
// particular order. Certainly for any given AST node we need to verify its type
// before emitting code for it. However, we may need to emit and execute code
// for some nodes to compute a type at compile-time. For this reason these steps
// require the same contextual data and therefore should be placed into a simple
// visitor type.
//
// Note that tying these together has a cost. C++ ties together these steps as
// well as parsing and it has compile-time performance cost as well as language
// semantic restrictions. This was design was not chosen lightly. We believe
// that the primary problem with C++ is that parsing is lumped together with
// type-verification and code generation and that the primary benefits that
// surface from separation are from separating parsing from these two stages
// rather than separating all stages. In time we will see if this belief holds
// water.

struct Compiler
    : ast::Visitor<VerifyResult(VerifyTypeTag), ir::Results(EmitValueTag),
                   void(type::Typed<ir::Reg> reg, EmitMoveInitTag),
                   void(type::Typed<ir::Reg> reg, EmitCopyInitTag),
                   std::vector<ir::RegOr<ir::Addr>>(EmitRefTag)>,
      type::Visitor<void(ir::Results const &, EmitPrintTag),
                    void(ir::Reg reg, EmitDestroyTag),
                    void(ir::Reg reg, EmitDefaultInitTag),
                    void(ir::RegOr<ir::Addr>, type::Typed<ir::Results> const &,
                         EmitMoveAssignTag),
                    void(ir::RegOr<ir::Addr>, type::Typed<ir::Results> const &,
                         EmitCopyAssignTag)> {
  VerifyResult Visit(ast::Node const *node, VerifyTypeTag) {
    return ast::SingleVisitor<VerifyResult(VerifyTypeTag)>::Visit(
        node, VerifyTypeTag{});
  }

  ir::Results Visit(ast::Node const *node, EmitValueTag) {
    return ast::SingleVisitor<ir::Results(EmitValueTag)>::Visit(node,
                                                                EmitValueTag{});
  }

  void Visit(ast::Node const *node, type::Typed<ir::Reg> reg, EmitCopyInitTag) {
    ast::SingleVisitor<void(type::Typed<ir::Reg> reg, EmitCopyInitTag)>::Visit(
        node, reg, EmitCopyInitTag{});
  }

  void Visit(ast::Node const *node, type::Typed<ir::Reg> reg, EmitMoveInitTag) {
    ast::SingleVisitor<void(type::Typed<ir::Reg> reg, EmitMoveInitTag)>::Visit(
        node, reg, EmitMoveInitTag{});
  }

  std::vector<ir::RegOr<ir::Addr>> Visit(ast::Node const *node, EmitRefTag) {
    return ast::SingleVisitor<std::vector<ir::RegOr<ir::Addr>>(
        EmitRefTag)>::Visit(node, EmitRefTag{});
  }

  void Visit(type::Type const *t, ir::Results const &val, EmitPrintTag) {
    type::SingleVisitor<void(ir::Results const &, EmitPrintTag)>::Visit(
        t, val, EmitPrintTag{});
  }

  void Visit(type::Type const *t, ir::Reg r, EmitDestroyTag) {
    type::SingleVisitor<void(ir::Reg, EmitDestroyTag)>::Visit(t, r,
                                                              EmitDestroyTag{});
  }

  void Visit(type::Type const *t, ir::Reg r, EmitDefaultInitTag) {
    type::SingleVisitor<void(ir::Reg, EmitDefaultInitTag)>::Visit(
        t, r, EmitDefaultInitTag{});
  }

  void Visit(type::Type const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Results> const &from, EmitMoveAssignTag) {
    type::SingleVisitor<void(
        ir::RegOr<ir::Addr>, type::Typed<ir::Results> const &,
        EmitMoveAssignTag)>::Visit(t, to, from, EmitMoveAssignTag{});
  }

  void Visit(type::Type const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Results> const &from, EmitCopyAssignTag) {
    type::SingleVisitor<void(
        ir::RegOr<ir::Addr>, type::Typed<ir::Results> const &,
        EmitCopyAssignTag)>::Visit(t, to, from, EmitCopyAssignTag{});
  }

  Compiler(module::BasicModule *mod);
  ~Compiler();

  module::BasicModule *module() { return mod_; }
  ir::Builder &builder() { return bldr_; };

  // TODO Depending on if we're streaming or batching errors, we may want one
  // log per module, or one per compiler instance.
  error::Log *error_log() { return &error_log_; }
  size_t num_errors() { return error_log()->size(); }
  void DumpErrors() { error_log()->Dump(); }

  VerifyResult const *prior_verification_attempt(ast::ExprPtr expr);
  type::Type const *type_of(ast::Expression const *expr) const;
  void set_addr(ast::Declaration const *decl, ir::Reg addr);
  compiler::VerifyResult set_result(ast::ExprPtr expr,
                                    compiler::VerifyResult r);

  ir::Reg addr(ast::Declaration const *decl) const;
  void set_dispatch_table(ast::ExprPtr expr, ast::DispatchTable &&table);
  void set_jump_table(ast::ExprPtr jump_expr, ast::ExprPtr node,
                      ast::DispatchTable &&table);

  ir::CompiledFn *AddFunc(
      type::Function const *fn_type,
      core::FnParams<type::Typed<ast::Declaration const *>> params);
  ir::CompiledFn *AddJump(
      type::Jump const *jump_type,
      core::FnParams<type::Typed<ast::Declaration const *>> params);
  ir::ScopeDef *AddScope(
      std::vector<ir::JumpHandler const *> inits,
      std::vector<ir::AnyFunc> dones,
      absl::flat_hash_map<std::string_view, ir::BlockDef *> blocks);
  ir::BlockDef *AddBlock(std::vector<ir::AnyFunc> befores,
                         std::vector<ir::JumpHandler const *> afters);

  ast::DispatchTable const *dispatch_table(ast::ExprPtr expr) const;
  ast::DispatchTable const *jump_table(ast::ExprPtr jump_expr,
                                       ast::ExprPtr node) const;

  module::PendingModule *pending_module(ast::Import const *import_node) const;

  std::pair<ConstantBinding, DependentData> *insert_constants(
      ConstantBinding const &constant_binding);

  void set_pending_module(ast::Import const *import_node,
                          module::PendingModule mod);

  void CompleteDeferredBodies();

  template <typename Fn>
  base::move_func<void()> *AddWork(ast::Node const *node, Fn &&fn) {
    auto [iter, success] =
        deferred_work_.lock()->emplace(node, std::forward<Fn>(fn));
    ASSERT(success == true);
    return &iter->second;
  }

#define ICARUS_AST_NODE_X(name)                                                \
  ir::Results Visit(ast::name const *node, EmitValueTag);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

#define ICARUS_AST_NODE_X(name)                                                \
  VerifyResult Visit(ast::name const *node, VerifyTypeTag);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  VerifyResult VerifyConcreteFnLit(ast::FunctionLiteral const *node);

  std::vector<ir::RegOr<ir::Addr>> Visit(ast::Access const *node, EmitRefTag);
  std::vector<ir::RegOr<ir::Addr>> Visit(ast::CommaList const *node,
                                         EmitRefTag);
  std::vector<ir::RegOr<ir::Addr>> Visit(ast::Identifier const *node,
                                         EmitRefTag);
  std::vector<ir::RegOr<ir::Addr>> Visit(ast::Index const *node, EmitRefTag);
  std::vector<ir::RegOr<ir::Addr>> Visit(ast::Unop const *node, EmitRefTag);

  void Visit(type::Array const *t, ir::Results const &val, EmitPrintTag);
  void Visit(type::Enum const *t, ir::Results const &val, EmitPrintTag);
  void Visit(type::Flags const *t, ir::Results const &val, EmitPrintTag);
  void Visit(type::Pointer const *t, ir::Results const &val, EmitPrintTag);
  void Visit(type::Primitive const *t, ir::Results const &val, EmitPrintTag);
  void Visit(type::Tuple const *t, ir::Results const &val, EmitPrintTag);
  void Visit(type::Variant const *t, ir::Results const &val, EmitPrintTag);

  void Visit(type::Struct const *t, ir::Reg reg, EmitDestroyTag);
  void Visit(type::Variant const *t, ir::Reg reg, EmitDestroyTag);
  void Visit(type::Tuple const *t, ir::Reg reg, EmitDestroyTag);
  void Visit(type::Array const *t, ir::Reg reg, EmitDestroyTag);

  void Visit(type::Array const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Results> const &from, EmitCopyAssignTag);
  void Visit(type::Enum const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Results> const &from, EmitCopyAssignTag);
  void Visit(type::Flags const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Results> const &from, EmitCopyAssignTag);
  void Visit(type::Function const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Results> const &from, EmitCopyAssignTag);
  void Visit(type::Pointer const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Results> const &from, EmitCopyAssignTag);
  void Visit(type::Primitive const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Results> const &from, EmitCopyAssignTag);
  void Visit(type::Struct const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Results> const &from, EmitCopyAssignTag);
  void Visit(type::Tuple const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Results> const &from, EmitCopyAssignTag);
  void Visit(type::Variant const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Results> const &from, EmitCopyAssignTag);

  void Visit(type::Array const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Results> const &from, EmitMoveAssignTag);
  void Visit(type::Enum const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Results> const &from, EmitMoveAssignTag);
  void Visit(type::Flags const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Results> const &from, EmitMoveAssignTag);
  void Visit(type::Function const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Results> const &from, EmitMoveAssignTag);
  void Visit(type::Pointer const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Results> const &from, EmitMoveAssignTag);
  void Visit(type::Primitive const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Results> const &from, EmitMoveAssignTag);
  void Visit(type::Struct const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Results> const &from, EmitMoveAssignTag);
  void Visit(type::Tuple const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Results> const &from, EmitMoveAssignTag);
  void Visit(type::Variant const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Results> const &from, EmitMoveAssignTag);

  void Visit(type::Array const *t, ir::Reg reg, EmitDefaultInitTag);
  void Visit(type::Flags const *t, ir::Reg reg, EmitDefaultInitTag);
  void Visit(type::Pointer const *t, ir::Reg reg, EmitDefaultInitTag);
  void Visit(type::Primitive const *t, ir::Reg reg, EmitDefaultInitTag);
  void Visit(type::Struct const *t, ir::Reg reg, EmitDefaultInitTag);
  void Visit(type::Tuple const *t, ir::Reg reg, EmitDefaultInitTag);

  void Visit(ast::Expression const *, type::Typed<ir::Reg> reg,
             EmitMoveInitTag);
  void Visit(ast::ArrayLiteral const *, type::Typed<ir::Reg> reg,
             EmitMoveInitTag);
  void Visit(ast::CommaList const *, type::Typed<ir::Reg> reg, EmitMoveInitTag);
  void Visit(ast::Unop const *, type::Typed<ir::Reg> reg, EmitMoveInitTag);

  void EmitMoveInit(type::Type const *from_type, ir::Results const &from_val,
                    type::Typed<ir::Reg> to_var);

  void Visit(ast::Expression const *, type::Typed<ir::Reg> reg,
             EmitCopyInitTag);
  void Visit(ast::ArrayLiteral const *, type::Typed<ir::Reg> reg,
             EmitCopyInitTag);
  void Visit(ast::CommaList const *, type::Typed<ir::Reg> reg, EmitCopyInitTag);
  void Visit(ast::Unop const *, type::Typed<ir::Reg> reg, EmitCopyInitTag);

  void EmitCopyInit(type::Type const *from_type, ir::Results const &from_val,
                    type::Typed<ir::Reg> to_var);

 private:
  module::BasicModule *mod_;
  ir::Builder &bldr_;

 public:  // TODO make private
  std::pair<ConstantBinding, DependentData> *constants_;
  // We only want to generate at most one node for each set of constants in a
  // function literal, but we can't generate them all at once because, for
  // example:
  //   (val :: T, T :: type) -> () { ... }
  // So we need to be able to build them even when there are dependencies
  // between them. To do this, we bulid them here and then move them into the
  // module constants when they're ready.
  ConstantBinding current_constants_;

  // TODO this looks useful in bindings too. maybe give it a better name and
  // use it more frequently?
  struct YieldResult {
    YieldResult(ast::Expression const *expr, ir::Results val)
        : expr_(expr), val_(std::move(val)) {}

    ast::Expression const *expr_;
    ir::Results val_;
  };
  std::vector<std::vector<YieldResult>> yields_stack_;

  // During validation, when a cyclic dependency is encountered, we write it
  // down here. That way, we can bubble up from the dependency until we see it
  // again, at each step adding the nodes to the error log involved in the
  // dependency. Once complete, we reset this to null
  std::vector<ast::Identifier const *> cyc_deps_;

  // TODO Because you already have arguments, it's perhaps better to just be a
  // pointer into the arguments buffer, to avoid the
  // reallocation/double-storage, but we can deal with this later. Probably
  // requires a deeper refactoring to have things linke ir::ResultView, etc.
  absl::flat_hash_map<ir::Reg, ir::Results> *inline_ = nullptr;

  base::guarded<absl::node_hash_map<ast::Node const *, base::move_func<void()>>>
      deferred_work_;

  std::vector<std::unique_ptr<ir::CompiledFn>> fns_;
  std::vector<std::unique_ptr<ir::ScopeDef>> scope_defs_;
  std::vector<std::unique_ptr<ir::BlockDef>> block_defs_;

  // TODO It's possible to have layers of constant bindings in a tree-like
  // structure. For example,
  //   f :: (a :: int64) => (b :: int64) => (c :: int64) => a + b * c
  // has 3 layers. Essentially the number of layers is the number of nested
  // scopes that have constant parameters (at time of writing only functions and
  // struct literals, though struct literals may not be specified as constants
  // syntactically?). For now you just store them flat in this vector and check
  // them potentially many times. Perhaps a tree-like structure would be more
  // efficient? More cache misses, but you're already paying heavily for the
  // equality call, so maybe it's just a simpler structure.
  //
  // std::list makes sense here because we never traverse them and we need
  // pointer stability. A vector of unique_ptrs would also work, but would
  // unnecessarily reallocate with some frequency..
  std::list<std::pair<ConstantBinding, DependentData>> dep_data_;

  error::Log error_log_;
};
}  // namespace compiler

#endif  // ICARUS_COMPILER_COMPILER_H
