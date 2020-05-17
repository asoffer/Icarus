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
#include "ir/value/addr.h"
#include "ir/value/native_fn.h"
#include "ir/value/reg.h"
#include "ir/value/value.h"
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

std::vector<std::pair<int, core::DependencyNode<ast::Declaration>>>
OrderedDependencyNodes(ast::ParameterizedExpression const *node);

struct Compiler
    : ast::Visitor<EmitMoveInitTag, void(type::Typed<ir::Reg>)>,
      ast::Visitor<EmitCopyInitTag, void(type::Typed<ir::Reg>)>,
      ast::Visitor<EmitRefTag, ir::RegOr<ir::Addr>()>,
      ast::Visitor<EmitValueTag, ir::Value()>,
      ast::Visitor<VerifyTypeTag, type::QualType()>,
      type::Visitor<void(ir::Reg, EmitDestroyTag),
                    void(ir::Reg, EmitDefaultInitTag),
                    void(ir::RegOr<ir::Addr>, type::Typed<ir::Value> const &,
                         EmitMoveAssignTag),
                    void(ir::RegOr<ir::Addr>, type::Typed<ir::Value> const &,
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
      core::FnArgs<std::pair<ir::Value, type::QualType>> vals;
      ir::Label label;
    };
    std::vector<YieldedArguments> yields;
  };

  type::QualType VerifyType(ast::Node const *node) {
    return ast::Visitor<VerifyTypeTag, type::QualType()>::Visit(node);
  }

  ir::Value EmitValue(ast::Node const *node) {
    return ast::Visitor<EmitValueTag, ir::Value()>::Visit(node);
  }

  void EmitCopyInit(ast::Node const *node, type::Typed<ir::Reg> reg) {
    ast::Visitor<EmitCopyInitTag, void(type::Typed<ir::Reg> reg)>::Visit(node,
                                                                         reg);
  }

  void EmitMoveInit(ast::Node const *node, type::Typed<ir::Reg> reg) {
    ast::Visitor<EmitMoveInitTag, void(type::Typed<ir::Reg> reg)>::Visit(node,
                                                                         reg);
  }

  ir::RegOr<ir::Addr> EmitRef(ast::Node const *node) {
    return ast::Visitor<EmitRefTag, ir::RegOr<ir::Addr>()>::Visit(node);
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
             type::Typed<ir::Value> const &from, EmitMoveAssignTag) {
    type::SingleVisitor<void(
        ir::RegOr<ir::Addr>, type::Typed<ir::Value> const &,
        EmitMoveAssignTag)>::Visit(t, to, from, EmitMoveAssignTag{});
  }

  void Visit(type::Type const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Value> const &from, EmitCopyAssignTag) {
    type::SingleVisitor<void(
        ir::RegOr<ir::Addr>, type::Typed<ir::Value> const &,
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

  template <typename T>
  base::expected<T> EvaluateAs(ast::Expression const * expr) {
    ASSIGN_OR(return _.error(), auto val,
                     Evaluate(type::Typed(expr, type::Get<T>())));
    return val.template get<T>();
  }
  base::expected<ir::Value> Evaluate(type::Typed<ast::Expression const *> expr);

  core::Params<type::Type const *> ComputeParamsFromArgs(
      ast::ParameterizedExpression const *node,
      absl::Span<std::pair<int, core::DependencyNode<ast::Declaration>> const>
          ordered_nodes,
      core::FnArgs<type::Typed<ir::Value>> const &args);

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

  void CompleteDeferredBodies();

#define ICARUS_AST_NODE_X(name)                                                \
  type::QualType VerifyType(ast::name const *node);                            \
  type::QualType Visit(VerifyTypeTag, ast::name const *node) override {        \
    return VerifyType(node);                                                   \
  }                                                                            \
                                                                               \
  ir::Value EmitValue(ast::name const *node);                                  \
  ir::Value Visit(EmitValueTag, ast::name const *node) override {              \
    return EmitValue(node);                                                    \
  }
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  TransientFunctionState::YieldedArguments EmitBlockNode(
      ast::BlockNode const *node);

  type::QualType VerifyConcreteFnLit(ast::FunctionLiteral const *node);

  ir::RegOr<ir::Addr> EmitRef(ast::Access const *node);
  ir::RegOr<ir::Addr> Visit(EmitRefTag, ast::Access const *node) override {
    return EmitRef(node);
  }
  ir::RegOr<ir::Addr> EmitRef(ast::Identifier const *node);
  ir::RegOr<ir::Addr> Visit(EmitRefTag, ast::Identifier const *node) override {
    return EmitRef(node);
  }
  ir::RegOr<ir::Addr> EmitRef(ast::Index const *node);
  ir::RegOr<ir::Addr> Visit(EmitRefTag, ast::Index const *node) override {
    return EmitRef(node);
  }
  ir::RegOr<ir::Addr> EmitRef(ast::Unop const *node);
  ir::RegOr<ir::Addr> Visit(EmitRefTag, ast::Unop const *node) override {
    return EmitRef(node);
  }

  void Visit(type::Struct const *t, ir::Reg reg, EmitDestroyTag) override;
  void Visit(type::Variant const *t, ir::Reg reg, EmitDestroyTag) override;
  void Visit(type::Tuple const *t, ir::Reg reg, EmitDestroyTag) override;
  void Visit(type::Array const *t, ir::Reg reg, EmitDestroyTag) override;

  void Visit(type::Array const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Value> const &from, EmitCopyAssignTag) override;
  void Visit(type::Enum const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Value> const &from, EmitCopyAssignTag) override;
  void Visit(type::Flags const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Value> const &from, EmitCopyAssignTag) override;
  void Visit(type::Function const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Value> const &from, EmitCopyAssignTag) override;
  void Visit(type::Pointer const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Value> const &from, EmitCopyAssignTag) override;
  void Visit(type::Primitive const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Value> const &from, EmitCopyAssignTag) override;
  void Visit(type::Struct const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Value> const &from, EmitCopyAssignTag) override;
  void Visit(type::Tuple const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Value> const &from, EmitCopyAssignTag) override;
  void Visit(type::Variant const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Value> const &from, EmitCopyAssignTag) override;

  void Visit(type::Array const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Value> const &from, EmitMoveAssignTag) override;
  void Visit(type::Enum const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Value> const &from, EmitMoveAssignTag) override;
  void Visit(type::Flags const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Value> const &from, EmitMoveAssignTag) override;
  void Visit(type::Function const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Value> const &from, EmitMoveAssignTag) override;
  void Visit(type::Pointer const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Value> const &from, EmitMoveAssignTag) override;
  void Visit(type::Primitive const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Value> const &from, EmitMoveAssignTag) override;
  void Visit(type::Struct const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Value> const &from, EmitMoveAssignTag) override;
  void Visit(type::Tuple const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Value> const &from, EmitMoveAssignTag) override;
  void Visit(type::Variant const *t, ir::RegOr<ir::Addr> to,
             type::Typed<ir::Value> const &from, EmitMoveAssignTag) override;

  void Visit(type::Array const *t, ir::Reg reg, EmitDefaultInitTag)override;
  void Visit(type::Flags const *t, ir::Reg reg, EmitDefaultInitTag)override;
  void Visit(type::Pointer const *t, ir::Reg reg, EmitDefaultInitTag)override;
  void Visit(type::Primitive const *t, ir::Reg reg, EmitDefaultInitTag)override;
  void Visit(type::Struct const *t, ir::Reg reg, EmitDefaultInitTag)override;
  void Visit(type::Tuple const *t, ir::Reg reg, EmitDefaultInitTag)override;

  void EmitMoveInit(ast::Expression const *node, type::Typed<ir::Reg> reg);
  void Visit(EmitMoveInitTag, ast::Expression const *node,
             type::Typed<ir::Reg> reg) {
    return EmitMoveInit(node, reg);
  }
  void EmitMoveInit(ast::ArrayLiteral const *node, type::Typed<ir::Reg> reg);
  void Visit(EmitMoveInitTag, ast::ArrayLiteral const *node,
             type::Typed<ir::Reg> reg) override {
    return EmitMoveInit(node, reg);
  }
  void EmitMoveInit(ast::Unop const *node, type::Typed<ir::Reg> reg);
  void Visit(EmitMoveInitTag, ast::Unop const *node,
             type::Typed<ir::Reg> reg) override {
    return EmitMoveInit(node, reg);
  }

  void EmitMoveInit(type::Type const *from_type, ir::Value from_val,
                    type::Typed<ir::Reg> to_var);

  void EmitCopyInit(ast::Expression const *node, type::Typed<ir::Reg> reg);
  void Visit(EmitCopyInitTag, ast::Expression const *node,
             type::Typed<ir::Reg> reg) {
    return EmitCopyInit(node, reg);
  }
  void EmitCopyInit(ast::ArrayLiteral const *node, type::Typed<ir::Reg> reg);
  void Visit(EmitCopyInitTag, ast::ArrayLiteral const *node,
             type::Typed<ir::Reg> reg) override {
    return EmitCopyInit(node, reg);
  }
  void EmitCopyInit(ast::Unop const *node, type::Typed<ir::Reg> reg);
  void Visit(EmitCopyInitTag, ast::Unop const *node,
             type::Typed<ir::Reg> reg) override {
    return EmitCopyInit(node, reg);
  }

  void EmitCopyInit(type::Type const *from_type, ir::Value from_val,
                    type::Typed<ir::Reg> to_var);

 private:
  PersistentResources resources_;
  TransientFunctionState state_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_COMPILER_H
