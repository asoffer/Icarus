#ifndef ICARUS_COMPILER_COMPILER_H
#define ICARUS_COMPILER_COMPILER_H

#include <memory>
#include <optional>
#include <queue>

#include "absl/container/flat_hash_map.h"
#include "absl/types/span.h"
#include "ast/ast.h"
#include "ast/overload_set.h"
#include "ast/visitor.h"
#include "base/debug.h"
#include "base/log.h"
#include "compiler/context.h"
#include "compiler/cyclic_dependency_tracker.h"
#include "compiler/module.h"
#include "compiler/resources.h"
#include "compiler/transient_state.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/source/source.h"
#include "ir/builder.h"
#include "ir/instruction/set.h"
#include "ir/interpreter/evaluate.h"
#include "ir/value/addr.h"
#include "ir/value/module_id.h"
#include "ir/value/native_fn.h"
#include "ir/value/reg.h"
#include "ir/value/value.h"
#include "module/importer.h"
#include "module/module.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/generic_function.h"
#include "type/generic_struct.h"
#include "type/jump.h"
#include "type/opaque.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/qual_type.h"
#include "type/slice.h"
#include "type/struct.h"
#include "type/type.h"
#include "type/visitor.h"

namespace compiler {
struct PatternMatchingContext {
  type::Type type;
  base::untyped_buffer value;
  union {
    size_t array_type_index = 0;
  };
};

struct EmitRefTag {};
struct EmitCopyInitTag {};
struct EmitMoveInitTag {};
struct EmitValueTag {};
struct VerifyTypeTag {};
struct VerifyBodyTag {};
struct EmitDestroyTag {};
struct EmitDefaultInitTag {};
struct EmitCopyAssignTag {};
struct EmitMoveAssignTag {};
struct PatternTypeTag {};
struct PatternMatchTag {};

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
    : ast::Visitor<EmitMoveInitTag,
                   void(absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const>)>,
      ast::Visitor<EmitCopyInitTag,
                   void(absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const>)>,
      ast::Visitor<EmitMoveAssignTag,
                   void(absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const>)>,
      ast::Visitor<EmitCopyAssignTag,
                   void(absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const>)>,
      ast::Visitor<EmitRefTag, ir::Reg()>,
      ast::Visitor<EmitValueTag, ir::Value()>,
      ast::Visitor<VerifyTypeTag, absl::Span<type::QualType const>()>,
      ast::Visitor<VerifyBodyTag, WorkItem::Result()>,
      ast::Visitor<
          PatternMatchTag,
          bool(PatternMatchingContext *,
               absl::flat_hash_map<ast::Declaration::Id const *, ir::Value> *)>,
      ast::Visitor<PatternTypeTag, bool(type::Type)>,
      type::Visitor<EmitDestroyTag, void(ir::Reg)>,
      type::Visitor<EmitMoveInitTag,
                    void(ir::Reg, type::Typed<ir::Value> const &)>,
      type::Visitor<EmitCopyInitTag,
                    void(ir::Reg, type::Typed<ir::Value> const &)>,
      type::Visitor<EmitDefaultInitTag, void(ir::Reg)>,
      type::Visitor<EmitMoveAssignTag, void(ir::RegOr<ir::addr_t>,
                                            type::Typed<ir::Value> const &)>,
      type::Visitor<EmitCopyAssignTag, void(ir::RegOr<ir::addr_t>,
                                            type::Typed<ir::Value> const &)> {
  PersistentResources &resources() { return resources_; }

  template <typename... Args>
  Compiler MakeChild(Args &&... args) {
    Compiler c(std::forward<Args>(args)...);
    c.builder().CurrentGroup() = builder().CurrentGroup();
    c.builder().CurrentBlock() = builder().CurrentBlock();
    return c;
  }

  void ProcessExecutableBody(base::PtrSpan<ast::Node const> nodes,
                             ir::CompiledFn *main_fn);

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

  void Enqueue(WorkItem work_item) {
    state_.work_queue.Enqueue(std::move(work_item));
  }

  absl::Span<type::QualType const> VerifyType(ast::Node const *node) {
    return ast::Visitor<VerifyTypeTag,
                        absl::Span<type::QualType const>()>::Visit(node);
  }

  WorkItem::Result VerifyBody(ast::Node const *node) {
    return ast::Visitor<VerifyBodyTag, WorkItem::Result()>::Visit(node);
  }

  bool PatternMatch(
      ast::Node const *node, PatternMatchingContext &context,
      absl::flat_hash_map<ast::Declaration::Id const *, ir::Value> &bindings) {
    return ast::Visitor<
        PatternMatchTag,
        bool(PatternMatchingContext *,
             absl::flat_hash_map<ast::Declaration::Id const *, ir::Value>
                 *)>::Visit(node, &context, &bindings);
  }

  void EnqueuePatternMatch(ast::Node const *node,
                           PatternMatchingContext context) {
    pattern_match_queues_.back().emplace(node, std::move(context));
  }

  void EnqueueVerifyPatternMatchType(ast::Node const *node,
                                     type::Type match_type) {
    verify_pattern_type_queues_.back().emplace(node, match_type);
  }

  bool VerifyPatternType(ast::Node const *node, type::Type t) {
    return ast::Visitor<PatternTypeTag, bool(type::Type)>::Visit(node, t);
  }


  ir::Value EmitValue(ast::Node const *node) {
    return ast::Visitor<EmitValueTag, ir::Value()>::Visit(node);
  }

  void EmitMoveAssign(ast::Node const *node,
                      absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs) {
    ast::Visitor<
        EmitMoveAssignTag,
        void(absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const>)>::Visit(node,
                                                                         regs);
  }

  void EmitCopyAssign(ast::Node const *node,
                      absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs) {
    ast::Visitor<
        EmitCopyAssignTag,
        void(absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const>)>::Visit(node,
                                                                         regs);
  }

  void EmitCopyInit(ast::Node const *node,
                    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs) {
    ast::Visitor<
        EmitCopyInitTag,
        void(absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const>)>::Visit(node,
                                                                         regs);
  }

  void EmitMoveInit(ast::Node const *node,
                    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs) {
    ast::Visitor<
        EmitMoveInitTag,
        void(absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const>)>::Visit(node,
                                                                         regs);
  }

  ir::Reg EmitRef(ast::Node const *node) {
    return ast::Visitor<EmitRefTag, ir::Reg()>::Visit(node);
  }

  void EmitDestroy(type::Typed<ir::Reg> r) {
    type::Visitor<EmitDestroyTag, void(ir::Reg)>::Visit(r.type().get(),
                                                        r.get());
  }

  void EmitDefaultInit(type::Typed<ir::Reg> r) {
    type::Visitor<EmitDefaultInitTag, void(ir::Reg)>::Visit(r.type().get(),
                                                            r.get());
  }

  void EmitMoveInit(type::Typed<ir::Reg> to,
                    type::Typed<ir::Value> const &from) {
    type::Visitor<EmitMoveInitTag, void(ir::Reg, type::Typed<ir::Value> const
                                                     &)>::Visit(to.type().get(),
                                                                *to, from);
  }

  void EmitCopyInit(type::Typed<ir::Reg> to,
                    type::Typed<ir::Value> const &from) {
    type::Visitor<EmitCopyInitTag, void(ir::Reg, type::Typed<ir::Value> const
                                                     &)>::Visit(to.type().get(),
                                                                *to, from);
  }

  void EmitMoveAssign(type::Typed<ir::RegOr<ir::addr_t>> const &to,
                      type::Typed<ir::Value> const &from) {
    type::Visitor<EmitMoveAssignTag,
                  void(ir::RegOr<ir::addr_t>,
                       type::Typed<ir::Value> const &)>::Visit(to.type().get(),
                                                               to.get(), from);
  }

  void EmitCopyAssign(type::Typed<ir::RegOr<ir::addr_t>> const &to,
                      type::Typed<ir::Value> const &from) {
    type::Visitor<EmitCopyAssignTag,
                  void(ir::RegOr<ir::addr_t>,
                       type::Typed<ir::Value> const &)>::Visit(to.type().get(),
                                                               to.get(), from);
  }

  explicit Compiler(PersistentResources const &resources);
  Compiler(Compiler const &) = delete;
  Compiler(Compiler &&)      = default;

  Context &context() const { return resources_.data; }
  ir::Builder &builder() { return builder_; };
  diagnostic::DiagnosticConsumer &diag() const {
    return resources_.diagnostic_consumer;
  }
  ir::BasicBlock *current_block() { return builder().CurrentBlock(); }

  module::Importer &importer() const { return resources_.importer; }

  // Evaluates `expr` in the current context as a value of type `T`. If
  // evaluation succeeds, returns the vaule, otherwise adds a diagnostic for the
  // failure and returns `nullopt`. If the expresison is no tof type `T`, the
  // behavior is undefined.
  template <typename T>
  std::optional<T> EvaluateOrDiagnoseAs(ast::Expression const *expr,
                                        bool must_complete = true) {
    type::Type t = [] {
      constexpr auto type = base::meta<T>;
      if constexpr (type == base::meta<type::Type>) { return type::Type_; }
      if constexpr (type == base::meta<ir::Scope>) { return type::Scope; }
      if constexpr (type == base::meta<ir::ModuleId>) { return type::Module; }
      if constexpr (type == base::meta<interface::Interface>) {
        return type::Interface;
      }
    }();
    auto maybe_value =
        Evaluate(type::Typed<ast::Expression const *>(expr, t), must_complete);
    if (not maybe_value) {
      diag().Consume(maybe_value.error());
      return std::nullopt;
    }
    return maybe_value->template get<T>();
  }

  ir::Value EvaluateOrDiagnose(type::Typed<ast::Expression const *> expr) {
    if (auto maybe_value = Evaluate(expr)) {
      return *maybe_value;
    } else {
      diag().Consume(maybe_value.error());
      return ir::Value();
    }
  }
  base::untyped_buffer EvaluateToBufferOrDiagnose(
      type::Typed<ast::Expression const *> expr);

  interpreter::EvaluationResult Evaluate(
      type::Typed<ast::Expression const *> expr, bool must_complete = true);

  core::Params<std::pair<ir::Value, type::QualType>> ComputeParamsFromArgs(
      ast::ParameterizedExpression const *node,
      core::Arguments<type::Typed<ir::Value>> const &args);

  // Attemnts to instantiate `node` with `args`, possibly creating a new
  // instantiation as a subcontext of `this->context()` if needed.
  Context::InsertSubcontextResult Instantiate(
      ast::ParameterizedExpression const *node,
      core::Arguments<type::Typed<ir::Value>> const &args);
  // Finds an already existing instantiation of `node` with `args` as a
  // subcontext of `this->context()`. Behavior is undefined if none exists.
  Context::FindSubcontextResult FindInstantiation(
      ast::ParameterizedExpression const *node,
      core::Arguments<type::Typed<ir::Value>> const &args);

  TransientState &state() { return state_; }

  void CompleteDeferredBodies();

#define ICARUS_AST_NODE_X(name)                                                \
  absl::Span<type::QualType const> VerifyType(ast::name const *node);          \
  absl::Span<type::QualType const> Visit(VerifyTypeTag, ast::name const *node) \
      override {                                                               \
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

#define DEFINE_PATTERN_MATCH(name)                                             \
  bool PatternMatch(                                                           \
      name const *node, PatternMatchingContext &context,                       \
      absl::flat_hash_map<ast::Declaration::Id const *, ir::Value> &bindings); \
  bool Visit(                                                                  \
      PatternMatchTag, name const *node, PatternMatchingContext *context,      \
      absl::flat_hash_map<ast::Declaration::Id const *, ir::Value> *bindings)  \
      override {                                                               \
    return PatternMatch(node, *context, *bindings);                            \
  }                                                                            \
                                                                               \
  bool VerifyPatternType(name const *node, type::Type t);                      \
  bool Visit(PatternTypeTag, name const *node, type::Type t) override {        \
    return VerifyPatternType(node, t);                                         \
  }
  DEFINE_PATTERN_MATCH(ast::Access)
  DEFINE_PATTERN_MATCH(ast::ArrayType)
  DEFINE_PATTERN_MATCH(ast::BinaryOperator)
  DEFINE_PATTERN_MATCH(ast::BindingDeclaration)
  DEFINE_PATTERN_MATCH(ast::Declaration)
  DEFINE_PATTERN_MATCH(ast::Terminal)
  DEFINE_PATTERN_MATCH(ast::UnaryOperator)
#undef DEFINE_PATTERN_MATCH

  // The reason to separate out type/body verification is if the body might
  // transitively have identifiers referring to a declaration that is assigned
  // directly to this node.
  WorkItem::Result VerifyBody(ast::Expression const *node) {
    return WorkItem::Result::Success;
  }
  WorkItem::Result VerifyBody(ast::EnumLiteral const *node);
  WorkItem::Result VerifyBody(ast::FunctionLiteral const *node);
  WorkItem::Result VerifyBody(ast::ParameterizedStructLiteral const *node);
  WorkItem::Result VerifyBody(ast::StructLiteral const *node);

  ir::Reg EmitRef(ast::Access const *node);
  ir::Reg Visit(EmitRefTag, ast::Access const *node) override {
    return EmitRef(node);
  }
  ir::Reg EmitRef(ast::Identifier const *node);
  ir::Reg Visit(EmitRefTag, ast::Identifier const *node) override {
    return EmitRef(node);
  }
  ir::Reg EmitRef(ast::Index const *node);
  ir::Reg Visit(EmitRefTag, ast::Index const *node) override {
    return EmitRef(node);
  }
  ir::Reg EmitRef(ast::UnaryOperator const *node);
  ir::Reg Visit(EmitRefTag, ast::UnaryOperator const *node) override {
    return EmitRef(node);
  }

#define DEFINE_EMIT_ASSIGN(T)                                                  \
  void Visit(EmitCopyAssignTag, T const *ty, ir::RegOr<ir::addr_t> r,            \
             type::Typed<ir::Value> const &tv) override {                      \
    EmitCopyAssign(type::Typed<ir::RegOr<ir::addr_t>, T>(r, ty), tv);            \
  }                                                                            \
  void EmitCopyAssign(type::Typed<ir::RegOr<ir::addr_t>, T> const &,             \
                      type::Typed<ir::Value> const &);                         \
                                                                               \
  void Visit(EmitMoveAssignTag, T const *ty, ir::RegOr<ir::addr_t> r,            \
             type::Typed<ir::Value> const &tv) override {                      \
    EmitMoveAssign(type::Typed<ir::RegOr<ir::addr_t>, T>(r, ty), tv);            \
  }                                                                            \
  void EmitMoveAssign(type::Typed<ir::RegOr<ir::addr_t>, T> const &r,            \
                      type::Typed<ir::Value> const &);

  DEFINE_EMIT_ASSIGN(type::Array);
  DEFINE_EMIT_ASSIGN(type::Enum);
  DEFINE_EMIT_ASSIGN(type::Flags);
  DEFINE_EMIT_ASSIGN(type::Function);
  DEFINE_EMIT_ASSIGN(type::Pointer);
  DEFINE_EMIT_ASSIGN(type::BufferPointer);
  DEFINE_EMIT_ASSIGN(type::Primitive);
  DEFINE_EMIT_ASSIGN(type::Slice);
  DEFINE_EMIT_ASSIGN(type::Struct);

#undef DEFINE_EMIT_ASSIGN

#define DEFINE_EMIT_DEFAULT_INIT(T)                                            \
  void Visit(EmitDefaultInitTag, T const *ty, ir::Reg r) override {            \
    EmitDefaultInit(type::Typed<ir::Reg, T>(r, ty));                           \
  }                                                                            \
  void EmitDefaultInit(type::Typed<ir::Reg, T> const &r)

  DEFINE_EMIT_DEFAULT_INIT(type::Array);
  DEFINE_EMIT_DEFAULT_INIT(type::Flags);
  DEFINE_EMIT_DEFAULT_INIT(type::Pointer);
  DEFINE_EMIT_DEFAULT_INIT(type::BufferPointer);
  DEFINE_EMIT_DEFAULT_INIT(type::Primitive);
  DEFINE_EMIT_DEFAULT_INIT(type::Struct);

#undef DEFINE_EMIT_DEFAULT_INIT

#define DEFINE_EMIT_INIT(T)                                                    \
  void Visit(EmitMoveInitTag, T const *ty, ir::Reg r,                          \
             type::Typed<ir::Value> const &from) override {                    \
    EmitMoveInit(type::Typed<ir::Reg, T>(r, ty), from);                        \
  }                                                                            \
  void EmitMoveInit(type::Typed<ir::Reg, T> to,                                \
                    type::Typed<ir::Value> const &from);                       \
                                                                               \
  void Visit(EmitCopyInitTag, T const *ty, ir::Reg r,                          \
             type::Typed<ir::Value> const &from) override {                    \
    EmitCopyInit(type::Typed<ir::Reg, T>(r, ty), from);                        \
  }                                                                            \
  void EmitCopyInit(type::Typed<ir::Reg, T> to,                                \
                    type::Typed<ir::Value> const &from)

  DEFINE_EMIT_INIT(type::Array);
  DEFINE_EMIT_INIT(type::Enum);
  DEFINE_EMIT_INIT(type::Flags);
  DEFINE_EMIT_INIT(type::Function);
  DEFINE_EMIT_INIT(type::Pointer);
  DEFINE_EMIT_INIT(type::BufferPointer);
  DEFINE_EMIT_INIT(type::Primitive);
  DEFINE_EMIT_INIT(type::Slice);
  DEFINE_EMIT_INIT(type::Struct);

#undef DEFINE_EMIT_INIT

#define DEFINE_EMIT_DESTROY(T)                                                 \
  void Visit(EmitDestroyTag, T const *ty, ir::Reg r) override {                \
    EmitDestroy(type::Typed<ir::Reg, T>(r, ty));                               \
  }                                                                            \
  void EmitDestroy(type::Typed<ir::Reg, T> const &r)

  DEFINE_EMIT_DESTROY(type::Array);
  DEFINE_EMIT_DESTROY(type::Enum) {}
  DEFINE_EMIT_DESTROY(type::Flags) {}
  DEFINE_EMIT_DESTROY(type::Pointer) {}
  DEFINE_EMIT_DESTROY(type::BufferPointer) {}
  DEFINE_EMIT_DESTROY(type::Primitive) {}
  DEFINE_EMIT_DESTROY(type::Slice) {}
  DEFINE_EMIT_DESTROY(type::Struct);

#undef DEFINE_EMIT_DESTROY

#define DEFINE_EMIT(node_type)                                                 \
  void EmitCopyInit(node_type const *node,                                     \
                    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs);  \
  void Visit(EmitCopyInitTag, node_type const *node,                           \
             absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs)          \
      override {                                                               \
    return EmitCopyInit(node, regs);                                           \
  }                                                                            \
  void EmitMoveInit(node_type const *node,                                     \
                    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs);  \
  void Visit(EmitMoveInitTag, node_type const *node,                           \
             absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs)          \
      override {                                                               \
    return EmitMoveInit(node, regs);                                           \
  }                                                                            \
  void EmitMoveAssign(                                                         \
      node_type const *node,                                                   \
      absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs);                \
  void Visit(EmitMoveAssignTag, node_type const *node,                         \
             absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs)          \
      override {                                                               \
    return EmitMoveAssign(node, regs);                                         \
  }                                                                            \
  void EmitCopyAssign(                                                         \
      node_type const *node,                                                   \
      absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs);                \
  void Visit(EmitCopyAssignTag, node_type const *node,                         \
             absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs)          \
      override {                                                               \
    return EmitCopyAssign(node, regs);                                         \
  }
  DEFINE_EMIT(ast::Access)
  DEFINE_EMIT(ast::ArrayLiteral)
  DEFINE_EMIT(ast::ArrayType)
  DEFINE_EMIT(ast::BinaryOperator)
  DEFINE_EMIT(ast::Call)
  DEFINE_EMIT(ast::Cast)
  DEFINE_EMIT(ast::FunctionLiteral)
  DEFINE_EMIT(ast::DesignatedInitializer)
  DEFINE_EMIT(ast::Identifier)
  DEFINE_EMIT(ast::Index)
  DEFINE_EMIT(ast::ScopeNode)
  DEFINE_EMIT(ast::ShortFunctionLiteral)
  DEFINE_EMIT(ast::SliceType)
  DEFINE_EMIT(ast::Terminal)
  DEFINE_EMIT(ast::UnaryOperator)
#undef DEFINE_EMIT

  type::QualType VerifyBinaryOverload(std::string_view symbol,
                                      ast::Expression const *node,
                                      type::Typed<ir::Value> const &lhs,
                                      type::Typed<ir::Value> const &rhs);

  ir::ModuleId EvaluateModuleWithCache(ast::Expression const *expr);

  std::optional<core::Params<type::QualType>> VerifyParams(
      core::Params<std::unique_ptr<ast::Declaration>> const &params);

  friend struct WorkItem;

  // TODO: The implementation here has some overlap with CompleteStruct.
  WorkItem::Result EnsureDataCompleteness(type::Struct *s);

  WorkItem::Result CompleteStruct(ast::StructLiteral const *node);
  WorkItem::Result CompleteStruct(ast::ParameterizedStructLiteral const *node);
  WorkItem::Result EmitJumpBody(ast::Jump const *node);
  WorkItem::Result EmitFunctionBody(ast::FunctionLiteral const *node);
  WorkItem::Result EmitShortFunctionBody(ast::ShortFunctionLiteral const *node);

 private:
  std::optional<core::Arguments<type::Typed<ir::Value>>> VerifyArguments(
      core::Arguments<ast::Expression const *> const &args);
  std::optional<core::Arguments<type::Typed<ir::Value>>> VerifyArguments(
      absl::Span<ast::Call::Argument const> args);

  type::QualType VerifyUnaryOverload(char const *symbol,
                                     ast::Expression const *node,
                                     type::Typed<ir::Value> const &operand);

  std::variant<
      std::vector<type::QualType>,
      absl::flat_hash_map<type::Callable const *, core::CallabilityResult>>
  VerifyCall(ast::Call const *call_expr,
             absl::flat_hash_map<ast::Expression const *,
                                 type::Callable const *> const &overload_map,
             core::Arguments<type::Typed<ir::Value>> const &args);

  std::pair<type::QualType, absl::flat_hash_map<ast::Expression const *,
                                                type::Callable const *>>
  VerifyCallee(
      ast::Expression const *callee,
      core::Arguments<type::Typed<ir::Value>> const &args,
      absl::flat_hash_set<type::Type> const &argument_dependent_lookup_types);

  PersistentResources resources_;
  TransientState state_;
  ir::Builder builder_;

  // TODO: Should move this into TransientState?
  std::vector<std::queue<std::pair<ast::Node const *, PatternMatchingContext>>>
      pattern_match_queues_;
  std::vector<std::queue<std::pair<ast::Node const *, type::Type>>>
      verify_pattern_type_queues_;

  // TODO: Should be persistent, but also needs on some local context.
  CyclicDependencyTracker cylcic_dependency_tracker_;
};

inline void WorkQueue::ProcessOneItem() {
  ASSERT(items_.empty() == false);
  WorkItem item = std::move(items_.front());
  items_.pop();
  WorkItem::Result result = item.Process();
  bool deferred           = (result == WorkItem::Result::Deferred);
  if (deferred) { items_.push(std::move(item)); }
#if defined(ICARUS_DEBUG)
  if (deferred) {
    LOG("", "Deferring %s", item.node->DebugString());
    cycle_breaker_count_ = deferred ? cycle_breaker_count_ + 1 : 0;
  }
  ASSERT(cycle_breaker_count_ <= items_.size());
#endif
}

}  // namespace compiler

#endif  // ICARUS_COMPILER_COMPILER_H
