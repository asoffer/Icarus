#ifndef ICARUS_COMPILER_COMPILER_H
#define ICARUS_COMPILER_COMPILER_H

#include "base/tag.h"
#include "compiler/deferred_body.h"
#include "compiler/verify_result.h"
#include "error/log.h"
#include "ir/addr.h"
#include "ir/builder.h"
#include "ir/reg.h"
#include "ir/results.h"
#include "misc/module.h"
#include "type/type_fwd.h"

namespace compiler {
// TODO: Come up with a better name.
//
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

struct Compiler : public DeferredBody<Compiler> {
  Compiler(Module *mod);

  Module *module() { return mod_; }
  ir::Builder &builder() { return bldr_; };
  error::Log *error_log() { return &module()->error_log_; }
  size_t num_errors() { return error_log()->size(); }
  void DumpErrors() { error_log()->Dump(); }

  base::Tagged<ir::Addr, ir::Reg> Alloca(type::Type const *t);
  base::Tagged<ir::Addr, ir::Reg> TmpAlloca(type::Type const *t);

  VerifyResult const *prior_verification_attempt(ast::ExprPtr expr);
  type::Type const *type_of(ast::Expression const *expr) const;
  void set_addr(ast::Declaration const *decl, ir::Reg addr);
  compiler::VerifyResult set_result(ast::ExprPtr expr, compiler::VerifyResult r);

  ir::Reg addr(ast::Declaration const *decl) const;
  void set_dispatch_table(ast::ExprPtr expr, ast::DispatchTable &&table);
  void set_jump_table(ast::ExprPtr jump_expr, ast::ExprPtr node,
                      ast::DispatchTable &&table);

  ast::DispatchTable const *dispatch_table(ast::ExprPtr expr) const;

  core::PendingModule *pending_module(ast::Import const *import_node) const;

  std::pair<ConstantBinding, DependentData> *insert_constants(
      ConstantBinding const &constant_binding);

  void set_pending_module(ast::Import const *import_node,
                          core::PendingModule mod);

  ir::Results EmitValue(ast::Node const *node) { UNREACHABLE(node); };
#define ICARUS_AST_NODE_X(name) ir::Results EmitValue(ast::name const *node);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  VerifyResult VerifyType(ast::Node const *node) { UNREACHABLE(node); };
#define ICARUS_AST_NODE_X(name) VerifyResult VerifyType(ast::name const *node);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  VerifyResult VerifyConcreteFnLit(ast::FunctionLiteral const *node);

  std::vector<ir::RegOr<ir::Addr>> EmitRef(ast::Node const *node) {
    UNREACHABLE(node);
  }
  std::vector<ir::RegOr<ir::Addr>> EmitRef(ast::Access const *node);
  std::vector<ir::RegOr<ir::Addr>> EmitRef(ast::CommaList const *node);
  std::vector<ir::RegOr<ir::Addr>> EmitRef(ast::Identifier const *node);
  std::vector<ir::RegOr<ir::Addr>> EmitRef(ast::Index const *node);
  std::vector<ir::RegOr<ir::Addr>> EmitRef(ast::Unop const *node);

  void EmitPrint(type::Type const *, ir::Results const &) { UNREACHABLE(); }
  void EmitPrint(type::Array const *t, ir::Results const &val);
  void EmitPrint(type::Enum const *t, ir::Results const &val);
  void EmitPrint(type::Flags const *t, ir::Results const &val);
  void EmitPrint(type::Pointer const *t, ir::Results const &val);
  void EmitPrint(type::Primitive const *t, ir::Results const &val);
  void EmitPrint(type::Tuple const *t, ir::Results const &val);
  void EmitPrint(type::Variant const *t, ir::Results const &val);

  void EmitDestroy(type::Type const *, ir::Reg) { UNREACHABLE(); }
  void EmitDestroy(type::Struct const *t, ir::Reg reg);
  void EmitDestroy(type::Variant const *t, ir::Reg reg);
  void EmitDestroy(type::Tuple const *t, ir::Reg reg);
  void EmitDestroy(type::Array const *t, ir::Reg reg);

  void EmitCopyAssign(type::Type const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from) {
    UNREACHABLE();
  }
  void EmitCopyAssign(type::Array const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitCopyAssign(type::Enum const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitCopyAssign(type::Flags const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitCopyAssign(type::Function const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitCopyAssign(type::Pointer const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitCopyAssign(type::Primitive const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitCopyAssign(type::Struct const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitCopyAssign(type::Tuple const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitCopyAssign(type::Variant const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);

  void EmitMoveAssign(type::Type const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from) {
    UNREACHABLE();
  }
  void EmitMoveAssign(type::Array const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitMoveAssign(type::Enum const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitMoveAssign(type::Flags const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitMoveAssign(type::Function const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitMoveAssign(type::Pointer const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitMoveAssign(type::Primitive const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitMoveAssign(type::Struct const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitMoveAssign(type::Tuple const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitMoveAssign(type::Variant const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);

  void EmitDefaultInit(type::Type const *t, ir::Reg) { UNREACHABLE(); }
  void EmitDefaultInit(type::Array const *t, ir::Reg reg);
  void EmitDefaultInit(type::Flags const *t, ir::Reg reg);
  void EmitDefaultInit(type::Pointer const *t, ir::Reg reg);
  void EmitDefaultInit(type::Primitive const *t, ir::Reg reg);
  void EmitDefaultInit(type::Struct const *t, ir::Reg reg);
  void EmitDefaultInit(type::Tuple const *t, ir::Reg reg);

  void EmitMoveInit(ast::Node const *, type::Typed<ir::Reg> reg) {
    UNREACHABLE();
  }
  void EmitMoveInit(ast::Expression const *, type::Typed<ir::Reg> reg);
  void EmitMoveInit(ast::ArrayLiteral const *, type::Typed<ir::Reg> reg);
  void EmitMoveInit(ast::CommaList const *, type::Typed<ir::Reg> reg);
  void EmitMoveInit(ast::Unop const *, type::Typed<ir::Reg> reg);

  void EmitMoveInit(type::Type const *from_type, ir::Results const &from_val,
                    type::Typed<ir::Reg> to_var);

  void EmitCopyInit(ast::Node const *, type::Typed<ir::Reg> reg) {
    UNREACHABLE();
  }
  void EmitCopyInit(ast::Expression const *, type::Typed<ir::Reg> reg);
  void EmitCopyInit(ast::ArrayLiteral const *, type::Typed<ir::Reg> reg);
  void EmitCopyInit(ast::CommaList const *, type::Typed<ir::Reg> reg);
  void EmitCopyInit(ast::Unop const *, type::Typed<ir::Reg> reg);

  void EmitCopyInit(type::Type const *from_type, ir::Results const &from_val,
                    type::Typed<ir::Reg> to_var);

 private:
  Module *mod_;
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
  bool more_stmts_allowed_ = true;

  absl::flat_hash_map<ir::BlockDef const *, ir::BasicBlock *> *block_map;

  // Temporaries need to be destroyed at the end of each statement.
  // This is a pointer to a buffer where temporary allocations can register
  // themselves for deletion.
  std::vector<type::Typed<ir::Reg>> *temporaries_to_destroy_ = nullptr;

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
};
}  // namespace compiler

#endif  // ICARUS_COMPILER_COMPILER_H
