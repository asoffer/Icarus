#ifndef ICARUS_COMPILER_COMPILER_H
#define ICARUS_COMPILER_COMPILER_H

#include <memory>
#include <optional>
#include <queue>

#include "absl/container/flat_hash_map.h"
#include "absl/types/span.h"
#include "ast/ast.h"
#include "ast/ast_fwd.h"
#include "ast/overload_set.h"
#include "ast/visitor.h"
#include "base/debug.h"
#include "base/move_func.h"
#include "compiler/cyclic_dependency_tracker.h"
#include "compiler/data.h"
#include "compiler/module.h"
#include "compiler/transient_state.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/source/source.h"
#include "ir/builder.h"
#include "ir/instruction/set.h"
#include "ir/interpretter/evaluate.h"
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

namespace interpretter {
// TODO remove duplication.
// TODO: Include ModInstruction, but only for non-floating-point types.
template <typename... Ts>
using ArithmeticInstructions =
    ir::InstructionSet<ir::RequiredCapabilities(), ir::AddInstruction<Ts>...,
                       ir::SubInstruction<Ts>..., ir::MulInstruction<Ts>...,
                       ir::DivInstruction<Ts>...>;
template <typename... Ts>
using EqualityComparisonInstructions =
    ir::InstructionSet<ir::RequiredCapabilities(), ir::EqInstruction<Ts>...,
                       ir::NeInstruction<Ts>...>;
template <typename... Ts>
using OrderedComparisonInstructions =
    ir::InstructionSet<ir::RequiredCapabilities(), ir::LtInstruction<Ts>...,
                       ir::LeInstruction<Ts>...,
                       EqualityComparisonInstructions<Ts...>>;
using instruction_set_t = ir::InstructionSet<
    ir::RequiredCapabilities(), ir::AddInstruction<uint8_t>,
    ArithmeticInstructions<uint8_t, int8_t, uint16_t, int16_t, uint32_t,
                           int32_t, uint64_t, int64_t, float, double>,
    ir::ModInstruction<uint8_t>, ir::ModInstruction<int8_t>,
    ir::ModInstruction<uint16_t>, ir::ModInstruction<int16_t>,
    ir::ModInstruction<uint32_t>, ir::ModInstruction<int32_t>,
    ir::ModInstruction<uint64_t>, ir::ModInstruction<int64_t>,
    EqualityComparisonInstructions<bool, uint8_t, int8_t, uint16_t, int16_t,
                                   uint32_t, int32_t, uint64_t, int64_t, float,
                                   double, ir::FlagsVal, type::Type const *,
                                   ir::Addr, ir::EnumVal>,
    OrderedComparisonInstructions<uint8_t, int8_t, uint16_t, int16_t, uint32_t,
                                  int32_t, uint64_t, int64_t, float, double,
                                  ir::FlagsVal>,
    ir::NegInstruction<int8_t>, ir::NegInstruction<int16_t>,
    ir::NegInstruction<int32_t>, ir::NegInstruction<int64_t>,
    ir::NegInstruction<float>, ir::NegInstruction<double>,
    ir::RegisterInstruction<uint8_t>, ir::RegisterInstruction<int8_t>,
    ir::RegisterInstruction<uint16_t>, ir::RegisterInstruction<int16_t>,
    ir::RegisterInstruction<uint32_t>, ir::RegisterInstruction<int32_t>,
    ir::RegisterInstruction<uint64_t>, ir::RegisterInstruction<int64_t>,
    ir::RegisterInstruction<float>, ir::RegisterInstruction<double>,
    ir::RegisterInstruction<type::Type const *>,
    ir::RegisterInstruction<ir::Addr>, ir::RegisterInstruction<ir::EnumVal>,
    ir::RegisterInstruction<ir::FlagsVal>, ir::RegisterInstruction<bool>,
    ir::StoreInstruction<uint8_t>, ir::StoreInstruction<int8_t>,
    ir::StoreInstruction<uint16_t>, ir::StoreInstruction<int16_t>,
    ir::StoreInstruction<uint32_t>, ir::StoreInstruction<int32_t>,
    ir::StoreInstruction<uint64_t>, ir::StoreInstruction<int64_t>,
    ir::StoreInstruction<float>, ir::StoreInstruction<double>,
    ir::StoreInstruction<type::Type const *>, ir::StoreInstruction<ir::Addr>,
    ir::StoreInstruction<ir::EnumVal>, ir::StoreInstruction<ir::FlagsVal>,
    ir::StoreInstruction<bool>, ir::StoreInstruction<ir::String>,
    ir::StoreInstruction<ir::Fn>, ir::PhiInstruction<uint8_t>,
    ir::PhiInstruction<int8_t>, ir::PhiInstruction<uint16_t>,
    ir::PhiInstruction<int16_t>, ir::PhiInstruction<uint32_t>,
    ir::PhiInstruction<int32_t>, ir::PhiInstruction<uint64_t>,
    ir::PhiInstruction<int64_t>, ir::PhiInstruction<float>,
    ir::PhiInstruction<double>, ir::PhiInstruction<type::Type const *>,
    ir::PhiInstruction<ir::Addr>, ir::PhiInstruction<ir::EnumVal>,
    ir::PhiInstruction<ir::FlagsVal>, ir::PhiInstruction<bool>,
    ir::PhiInstruction<ir::String>, ir::SetReturnInstruction<uint8_t>,
    ir::SetReturnInstruction<int8_t>, ir::SetReturnInstruction<uint16_t>,
    ir::SetReturnInstruction<int16_t>, ir::SetReturnInstruction<uint32_t>,
    ir::SetReturnInstruction<int32_t>, ir::SetReturnInstruction<uint64_t>,
    ir::SetReturnInstruction<int64_t>, ir::SetReturnInstruction<float>,
    ir::SetReturnInstruction<double>,
    ir::SetReturnInstruction<type::Type const *>,
    ir::SetReturnInstruction<ir::Addr>, ir::SetReturnInstruction<ir::EnumVal>,
    ir::SetReturnInstruction<ir::FlagsVal>, ir::SetReturnInstruction<bool>,
    ir::SetReturnInstruction<ir::String>, ir::SetReturnInstruction<ir::Fn>,
    ir::SetReturnInstruction<core::Bytes>,
    ir::SetReturnInstruction<core::Alignment>,
    ir::SetReturnInstruction<ir::BlockDef const *>,
    ir::SetReturnInstruction<ir::ScopeDef const *>,
    ir::SetReturnInstruction<module::BasicModule const *>,
    ir::SetReturnInstruction<ir::GenericFn>,
    ir::SetReturnInstruction<ir::Jump *>,
    ir::SetReturnInstruction<type::GenericStruct const *>,
    ir::CastInstruction<uint8_t>, ir::CastInstruction<int8_t>,
    ir::CastInstruction<uint16_t>, ir::CastInstruction<int16_t>,
    ir::CastInstruction<uint32_t>, ir::CastInstruction<int32_t>,
    ir::CastInstruction<uint64_t>, ir::CastInstruction<int64_t>,
    ir::CastInstruction<float>, ir::CastInstruction<double>, ir::NotInstruction,
    ir::XorFlagsInstruction, ir::AndFlagsInstruction, ir::OrFlagsInstruction,
    ir::PtrInstruction, ir::BufPtrInstruction, ir::GetReturnInstruction,
    ir::OpaqueTypeInstruction, ir::ArrowInstruction, ir::CallInstruction,
    ir::LoadSymbolInstruction, ir::ArrayInstruction, ir::StructInstruction,
    ir::MakeBlockInstruction, ir::MakeScopeInstruction,
    ir::StructIndexInstruction, ir::TupleIndexInstruction,
    ir::PtrIncrInstruction, ir::TupleInstruction, ir::EnumerationInstruction,
    ir::TypeInfoInstruction, ir::TypeManipulationInstruction,
    ir::ByteViewLengthInstruction, ir::ByteViewDataInstruction,
    ir::DebugIrInstruction>;
}  // namespace interpretter

namespace compiler {
struct EmitRefTag {};
struct EmitCopyInitTag {};
struct EmitMoveInitTag {};
struct EmitValueTag {};
struct VerifyTypeTag {};
struct VerifyBodyTag {};
struct EmitAssignTag {};
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

// TODO: Document.
std::vector<std::pair<int, core::DependencyNode<ast::Declaration>>>
OrderedDependencyNodes(ast::ParameterizedExpression const *node,
                       bool all = false);

struct Compiler
    : ast::Visitor<EmitMoveInitTag,
                   void(absl::Span<type::Typed<ir::RegOr<ir::Addr>> const>)>,
      ast::Visitor<EmitCopyInitTag,
                   void(absl::Span<type::Typed<ir::RegOr<ir::Addr>> const>)>,
      ast::Visitor<EmitAssignTag,
                   void(absl::Span<type::Typed<ir::RegOr<ir::Addr>> const>)>,
      ast::Visitor<EmitRefTag, ir::RegOr<ir::Addr>()>,
      ast::Visitor<EmitValueTag, ir::Value()>,
      ast::Visitor<VerifyTypeTag, type::QualType()>,
      ast::Visitor<VerifyBodyTag, WorkItem::Result()>,
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

  void VerifyAll(base::PtrSpan<ast::Node const> nodes) {
    for (ast::Node const *node : nodes) {
      if (auto const *decl = node->if_as<ast::Declaration>()) {
        if (decl->flags() & ast::Declaration::f_IsConst) { VerifyType(node); }
      }

    }

    for (ast::Node const *node : nodes) {
      if (auto const *decl = node->if_as<ast::Declaration>()) {
        if (decl->flags() & ast::Declaration::f_IsConst) { continue; }
      }

      VerifyType(node);
    }

    CompleteWorkQueue();
  }
  void CompleteWorkQueue() {
    while (not state_.work_queue.empty()) {
      state_.work_queue.ProcessOneItem();
    }
  }

  type::QualType VerifyType(ast::Node const *node) {
    return ast::Visitor<VerifyTypeTag, type::QualType()>::Visit(node);
  }

  WorkItem::Result VerifyBody(ast::Node const *node) {
    return ast::Visitor<VerifyBodyTag, WorkItem::Result()>::Visit(node);
  }

  ir::Value EmitValue(ast::Node const *node) {
    return ast::Visitor<EmitValueTag, ir::Value()>::Visit(node);
  }

  void EmitAssign(ast::Node const *node,
                  absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> regs) {
    ast::Visitor<
        EmitAssignTag,
        void(absl::Span<type::Typed<ir::RegOr<ir::Addr>> const>)>::Visit(node,
                                                                         regs);
  }

  void EmitCopyInit(ast::Node const *node,
                    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> regs) {
    ast::Visitor<
        EmitCopyInitTag,
        void(absl::Span<type::Typed<ir::RegOr<ir::Addr>> const>)>::Visit(node,
                                                                         regs);
  }

  void EmitMoveInit(ast::Node const *node,
                    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> regs) {
    ast::Visitor<
        EmitMoveInitTag,
        void(absl::Span<type::Typed<ir::RegOr<ir::Addr>> const>)>::Visit(node,
                                                                         regs);
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
  base::expected<T, interpretter::EvaluationFailure> EvaluateAs(
      ast::Expression const *expr) {
    ASSIGN_OR(return _.error(), auto val,
                     Evaluate(type::Typed(expr, type::Get<T>())));
    return val.template get<T>();
  }
  base::expected<ir::Value, interpretter::EvaluationFailure> Evaluate(
      type::Typed<ast::Expression const *> expr, bool must_complete = true);

  core::Params<std::pair<ir::Value, type::QualType>> ComputeParamsFromArgs(
      ast::ParameterizedExpression const *node,
      absl::Span<std::pair<int, core::DependencyNode<ast::Declaration>> const>
          ordered_nodes,
      core::FnArgs<type::Typed<ir::Value>> const &args);

  std::optional<type::QualType> qual_type_of(ast::Expression const *expr) const;
  type::Type const *type_of(ast::Expression const *expr) const;

  absl::Span<TransientState::ScopeLandingState const> scope_landings()
      const {
    return state_.scope_landings;
  }
  void add_scope_landing(TransientState::ScopeLandingState state) {
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
  WorkItem::Result Visit(VerifyBodyTag, ast::name const *node) override {      \
    return VerifyBody(node);                                                   \
  }                                                                            \
                                                                               \
  ir::Value EmitValue(ast::name const *node);                                  \
  ir::Value Visit(EmitValueTag, ast::name const *node) override {              \
    return EmitValue(node);                                                    \
  }
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  TransientState::YieldedArguments EmitBlockNode(ast::BlockNode const *node);

  // The reason to separate out type/body verification is if the body might
  // transitively have identifiers referring to a declaration that is assigned
  // directly to this node.
  WorkItem::Result VerifyBody(ast::Expression const *node) {
    return WorkItem::Result::Success;
  }
  WorkItem::Result VerifyBody(ast::BlockLiteral const *node);
  WorkItem::Result VerifyBody(ast::EnumLiteral const *node);
  WorkItem::Result VerifyBody(ast::FunctionLiteral const *node);
  WorkItem::Result VerifyBody(ast::Jump const *node);
  WorkItem::Result VerifyBody(ast::ParameterizedStructLiteral const *node);
  WorkItem::Result VerifyBody(ast::StructLiteral const *node);

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
  ir::RegOr<ir::Addr> EmitRef(ast::UnaryOperator const *node);
  ir::RegOr<ir::Addr> Visit(EmitRefTag,
                            ast::UnaryOperator const *node) override {
    return EmitRef(node);
  }

  void Visit(type::Struct const *t, ir::Reg reg, EmitDestroyTag) override;
  void Visit(type::Tuple const *t, ir::Reg reg, EmitDestroyTag) override;
  void Visit(type::Array const *t, ir::Reg reg, EmitDestroyTag) override;
  void Visit(type::Primitive const *t, ir::Reg reg, EmitDestroyTag) override;
  void Visit(type::Pointer const *t, ir::Reg reg, EmitDestroyTag) override;

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

  void Visit(type::Array const *t, ir::Reg reg, EmitDefaultInitTag) override;
  void Visit(type::Flags const *t, ir::Reg reg, EmitDefaultInitTag) override;
  void Visit(type::Pointer const *t, ir::Reg reg, EmitDefaultInitTag) override;
  void Visit(type::Primitive const *t, ir::Reg reg,
             EmitDefaultInitTag) override;
  void Visit(type::Struct const *t, ir::Reg reg, EmitDefaultInitTag) override;
  void Visit(type::Tuple const *t, ir::Reg reg, EmitDefaultInitTag) override;

  void EmitAssign(ast::Access const *node,
                  absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> regs);
  void Visit(EmitAssignTag, ast::Access const *node,
             absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> regs) override {
    return EmitAssign(node, regs);
  }

  void EmitAssign(ast::BinaryOperator const *node,
                  absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> regs);
  void Visit(EmitAssignTag, ast::BinaryOperator const *node,
             absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> regs) override {
    return EmitAssign(node, regs);
  }

  void EmitAssign(ast::Call const *node,
                  absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> regs);
  void Visit(EmitAssignTag, ast::Call const *node,
             absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> regs) override {
    return EmitAssign(node, regs);
  }

  void EmitAssign(ast::Identifier const *node,
                  absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> regs);
  void Visit(EmitAssignTag, ast::Identifier const *node,
             absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> regs) override {
    return EmitAssign(node, regs);
  }

  void EmitAssign(ast::Index const *node,
                  absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> regs);
  void Visit(EmitAssignTag, ast::Index const *node,
             absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> regs) override {
    return EmitAssign(node, regs);
  }

  void EmitAssign(ast::Terminal const *node,
                  absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> regs);
  void Visit(EmitAssignTag, ast::Terminal const *node,
             absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> regs) override {
    return EmitAssign(node, regs);
  }

  void EmitAssign(ast::UnaryOperator const *node,
                  absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> regs);
  void Visit(EmitAssignTag, ast::UnaryOperator const *node,
             absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> regs) override {
    return EmitAssign(node, regs);
  }

#define DEFINE_EMIT_INIT(node_type)                                            \
  void EmitCopyInit(node_type const *node,                                     \
                    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> regs);  \
  void Visit(EmitCopyInitTag, node_type const *node,                           \
             absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> regs)          \
      override {                                                               \
    return EmitCopyInit(node, regs);                                           \
  }                                                                            \
  void EmitMoveInit(node_type const *node,                                     \
                    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> regs);  \
  void Visit(EmitMoveInitTag, node_type const *node,                           \
             absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> regs)          \
      override {                                                               \
    return EmitMoveInit(node, regs);                                           \
  }
  DEFINE_EMIT_INIT(ast::Access)
  DEFINE_EMIT_INIT(ast::ArrayLiteral)
  DEFINE_EMIT_INIT(ast::BinaryOperator)
  DEFINE_EMIT_INIT(ast::Call)
  DEFINE_EMIT_INIT(ast::Cast)
  DEFINE_EMIT_INIT(ast::DesignatedInitializer)
  DEFINE_EMIT_INIT(ast::Identifier)
  DEFINE_EMIT_INIT(ast::Index)
  DEFINE_EMIT_INIT(ast::Terminal)
  DEFINE_EMIT_INIT(ast::UnaryOperator)
#undef DEFINE_EMIT_INIT

  void EmitMoveInit(type::Typed<ir::Value> from_val,
                    type::Typed<ir::Reg> to_var);

  void EmitCopyInit(type::Typed<ir::Value> from_val,
                    type::Typed<ir::Reg> to_var);

  type::QualType VerifyBinaryOverload(std::string_view symbol,
                                      ast::Expression const *node,
                                      type::Typed<ir::Value> const &lhs,
                                      type::Typed<ir::Value> const &rhs);

  // TODO: Figure out where to stick these.
  struct CallError {
    struct TooManyArguments {
      size_t num_provided;
      size_t max_num_accepted;
    };

    struct MissingNonDefaultableArguments {
      absl::flat_hash_set<std::string> names;
    };

    struct TypeMismatch {
      std::variant<std::string, size_t> parameter;
      type::Type const *argument_type;
    };

    struct NoParameterNamed {
      std::string name;
    };

    struct PositionalArgumentNamed {
      size_t index;
      std::string name;
    };

    using ErrorReason =
        std::variant<TooManyArguments, MissingNonDefaultableArguments,
                     TypeMismatch, NoParameterNamed, PositionalArgumentNamed,
                     CallError>;

    // TODO: It might be better to track back to the definition, but for now all
    // we have is type information.
    absl::flat_hash_map<type::Callable const *, ErrorReason> reasons;
  };

  CompiledModule const *EvaluateModuleWithCache(ast::Expression const *expr) {
    // TODO: Implement caching behavior.
    auto maybe_mod = EvaluateAs<module::BasicModule *>(expr);
    if (not maybe_mod) {
      diag().Consume(diagnostic::EvaluationFailure{.failure = maybe_mod.error(),
                                                   .range   = expr->range()});
      return nullptr;
    }

    // TODO: Rather than evaluating as a BasicModule and then down-casting to
    // CompiledModule, we should make this cast unnecessary.
    return &(*maybe_mod)->as<CompiledModule>();
  }

 private:
  friend struct WorkItem;

  WorkItem::Result CompleteStruct(ast::StructLiteral const *node);

  std::optional<core::FnArgs<type::Typed<ir::Value>>> VerifyFnArgs(
      core::FnArgs<ast::Expression const *> const &args);

  type::QualType VerifyUnaryOverload(char const *symbol,
                                     ast::Expression const *node,
                                     type::Typed<ir::Value> const &operand);

  base::expected<type::QualType, CallError> VerifyCall(
      ast::Call const *call_expr,
      absl::flat_hash_map<ast::Expression const *, type::Callable const *> const
          &overload_map,
      core::FnArgs<type::Typed<ir::Value>> const &args);

  std::pair<type::QualType, absl::flat_hash_map<ast::Expression const *,
                                                type::Callable const *>>
  VerifyCallee(ast::Expression const *callee,
               core::FnArgs<type::Typed<ir::Value>> const &args);

  PersistentResources resources_;
  TransientState state_;

  // TODO: Should be persistent, but also needs on some local context
  // (DependentComputedData).
  CyclicDependencyTracker cylcic_dependency_tracker_;
};

inline WorkItem::Result WorkItem::Process() const {
  Compiler c({
      .builder             = ir::GetBuilder(),
      .data                = context,
      .diagnostic_consumer = consumer,
  });
  switch (kind) {
    case Kind::VerifyBlockBody:
      return c.VerifyBody(&node->as<ast::BlockLiteral>());
    case Kind::VerifyEnumBody:
      return c.VerifyBody(&node->as<ast::EnumLiteral>());
    case Kind::VerifyFunctionBody:
      return c.VerifyBody(&node->as<ast::FunctionLiteral>());
    case Kind::VerifyJumpBody: return c.VerifyBody(&node->as<ast::Jump>());
    case Kind::VerifyScopeBody: NOT_YET();
    case Kind::VerifyStructBody:
      return c.VerifyBody(&node->as<ast::StructLiteral>());
    case Kind::CompleteStructMembers:
      return c.CompleteStruct(&node->as<ast::StructLiteral>());
  }
}

inline void WorkQueue::ProcessOneItem() {
  ASSERT(items_.empty() == false);
  WorkItem item = std::move(items_.front());
  items_.pop();
  WorkItem::Result result = item.Process();
  bool deferred = (result == WorkItem::Result::Deferred);
  if (deferred) { items_.push(std::move(item)); }
#if defined(ICARUS_DEBUG)
  if (deferred) {
    DEBUG_LOG()("Deferring ", item.node->DebugString());
    cycle_breaker_count_ = deferred ? cycle_breaker_count_ + 1 : 0;
  }
  ASSERT(cycle_breaker_count_ <= items_.size());
#endif
}

}  // namespace compiler

#endif  // ICARUS_COMPILER_COMPILER_H
