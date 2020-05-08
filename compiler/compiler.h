#ifndef ICARUS_COMPILER_COMPILER_H
#define ICARUS_COMPILER_COMPILER_H

#include <memory>
#include <optional>

#include "absl/container/flat_hash_map.h"
#include "ast/ast_fwd.h"
#include "ast/overload_set.h"
#include "ast/visitor.h"
#include "base/debug.h"
#include "base/move_func.h"
#include "compiler/constant/binding.h"
#include "compiler/data.h"
#include "compiler/module.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/source/source.h"
#include "ir/builder.h"
#include "ir/results.h"
#include "ir/value/addr.h"
#include "ir/value/native_fn.h"
#include "ir/value/reg.h"
#include "module/module.h"
#include "type/qual_type.h"
#include "type/type_fwd.h"
#include "type/visitor.h"

namespace ir {
struct ScopeDef;
struct BlockDef;
}  // namespace ir

namespace compiler {
struct EmitRefTag {};
struct EmitCopyInitTag {};
struct EmitMoveInitTag {};
struct EmitValueTag {};
struct VerifyTypeTag {};
struct EmitDestroyTag {};
struct EmitDefaultInitTag {};
struct EmitCopyAssignTag {};
struct EmitMoveAssignTag {};

struct LibraryModule;  // TODO remove me.

// These are the steps in a traditional compiler of verifying types and emitting
// code. They're tied together because they don't necessarily happen in a
// particular order. Certainly for any given AST node we need to verify its type
// before emitting code for it. However, we may need to emit and execute code
// for some nodes to compute a type at compile-time. For this reason these steps
// require the same contextual data and therefore should be placed into a single
// visitor type.
//
// Note that tying these together has a cost. C++ ties together these steps as
// well as parsing and it has compile-time performance cost as well as language
// semantic restrictions. This design was not chosen lightly. We believe that
// the primary problem with C++ is that parsing is lumped together with
// type-verification and code generation and that the primary benefits that
// surface from separation are from separating parsing from these two stages
// rather than separating all stages. In time we will see if this belief holds
// water.

struct Compiler
    : ast::Visitor<type::QualType(VerifyTypeTag), ir::Results(EmitValueTag),
                   void(type::Typed<ir::Reg>, EmitMoveInitTag),
                   void(type::Typed<ir::Reg>, EmitCopyInitTag),
                   std::vector<ir::RegOr<ir::Addr>>(EmitRefTag)>,
      type::Visitor<void(ir::Reg, EmitDestroyTag),
                    void(ir::Reg, EmitDefaultInitTag),
                    void(ir::RegOr<ir::Addr>, type::Typed<ir::Results> const &,
                         EmitMoveAssignTag),
                    void(ir::RegOr<ir::Addr>, type::Typed<ir::Results> const &,
                         EmitCopyAssignTag)> {
  // Resources and pointers/references to data that are guaranteed to outlive
  // any Compiler construction.
  struct PersistentResources {
    ir::Builder &builder;
    DependentComputedData &data;
    diagnostic::DiagnosticConsumer &diagnostic_consumer;
  };

  // Compiler state that needs to be tracked during the compilation of a single
  // function or jump, but otherwise does not need to be saved.
  struct TransientFunctionState {
    struct ScopeLandingState {
      ir::Label label;
      ir::BasicBlock *block;
      ir::PhiInstruction<int64_t> *phi;
    };
    std::vector<ScopeLandingState> scope_landings;

    struct YieldedArguments {
      core::FnArgs<std::pair<ir::Results, type::QualType>> vals;
      ir::Label label;
    };
    std::vector<YieldedArguments> yields;
  };

  type::QualType Visit(ast::Node const *node, VerifyTypeTag) {
    return ast::SingleVisitor<type::QualType(VerifyTypeTag)>::Visit(
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

  explicit Compiler(PersistentResources const &resources);

  // Returns a new `Compiler` instance which points to the same persistent
  // resources.
  Compiler WithPersistent() const;

  DependentComputedData &data() const { return resources_.data; }
  ir::Builder &builder() { return resources_.builder; };
  diagnostic::DiagnosticConsumer &diag() const {
    return resources_.diagnostic_consumer;
  }

  ir::CompiledFn MakeThunk(ast::Expression const *expr, type::Type const *type);

  std::optional<type::QualType> qual_type_of(ast::Expression const *expr) const;
  type::Type const *type_of(ast::Expression const *expr) const;

  absl::Span<TransientFunctionState::ScopeLandingState const> scope_landings()
      const {
    return state_.scope_landings;
  }
  void add_scope_landing(TransientFunctionState::ScopeLandingState state) {
    state_.scope_landings.push_back(std::move(state));
  }
  void pop_scope_landing() { state_.scope_landings.pop_back(); }

  ir::NativeFn AddFunc(
      type::Function const *fn_type,
      core::Params<type::Typed<ast::Declaration const *>> params);

  module::Pending<LibraryModule> *pending_module(
      ast::Import const *import_node) const;

  void set_pending_module(ast::Import const *import_node,
                          module::Pending<LibraryModule> mod);

  void CompleteDeferredBodies();

#define ICARUS_AST_NODE_X(name)                                                \
  ir::Results Visit(ast::name const *node, EmitValueTag);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

#define ICARUS_AST_NODE_X(name)                                                \
  type::QualType Visit(ast::name const *node, VerifyTypeTag);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  TransientFunctionState::YieldedArguments EmitBlockNode(
      ast::BlockNode const *node);

  type::QualType VerifyConcreteFnLit(ast::FunctionLiteral const *node);

  std::vector<ir::RegOr<ir::Addr>> Visit(ast::Access const *node, EmitRefTag);
  std::vector<ir::RegOr<ir::Addr>> Visit(ast::CommaList const *node,
                                         EmitRefTag);
  std::vector<ir::RegOr<ir::Addr>> Visit(ast::Identifier const *node,
                                         EmitRefTag);
  std::vector<ir::RegOr<ir::Addr>> Visit(ast::Index const *node, EmitRefTag);
  std::vector<ir::RegOr<ir::Addr>> Visit(ast::Unop const *node, EmitRefTag);

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

  PersistentResources resources_;
 private:
  TransientFunctionState state_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_COMPILER_H
