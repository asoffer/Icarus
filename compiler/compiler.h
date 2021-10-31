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
#include "compiler/bound_parameters.h"
#include "compiler/context.h"
#include "compiler/cyclic_dependency_tracker.h"
#include "compiler/instructions.h"
#include "compiler/resources.h"
#include "compiler/transient_state.h"
#include "compiler/work_item.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/source/source.h"
#include "ir/builder.h"
#include "ir/instruction/set.h"
#include "ir/interpreter/evaluate.h"
#include "ir/value/addr.h"
#include "ir/value/module_id.h"
#include "ir/value/native_fn.h"
#include "ir/value/reg.h"
#include "ir/value/result_buffer.h"
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

struct NotAType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "not-a-type";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Expression was expected to be a type, but instead "
                         "was a value of type `%s`.",
                         type),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
  type::Type type;
};

struct PatternMatchingContext {
  type::Type type;
  ir::CompleteResultBuffer value;
  union {
    size_t array_type_index = 0;
  };
};

struct EmitRefTag {};
struct EmitCopyInitTag {};
struct EmitMoveInitTag {};
struct EmitToBufferTag {};
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
      ast::Visitor<EmitToBufferTag, void(ir::PartialResultBuffer *)>,
      ast::Visitor<VerifyTypeTag, absl::Span<type::QualType const>()>,
      ast::Visitor<VerifyBodyTag, bool()>,
      ast::Visitor<PatternMatchTag,
                   bool(PatternMatchingContext *,
                        absl::flat_hash_map<ast::Declaration::Id const *,
                                            ir::CompleteResultBuffer> *)>,
      ast::Visitor<PatternTypeTag, bool(type::Type)>,
      type::Visitor<EmitDestroyTag, void(ir::Reg)>,
      type::Visitor<EmitMoveInitTag,
                    void(ir::Reg, ir::PartialResultBuffer const *)>,
      type::Visitor<EmitCopyInitTag,
                    void(ir::Reg, ir::PartialResultBuffer const *)>,
      type::Visitor<EmitDefaultInitTag, void(ir::Reg)>,
      type::Visitor<EmitMoveAssignTag,
                    void(ir::RegOr<ir::addr_t>,
                         type::Typed<ir::PartialResultRef> const &)>,
      type::Visitor<EmitCopyAssignTag,
                    void(ir::RegOr<ir::addr_t>,
                         type::Typed<ir::PartialResultRef> const &)> {
  PersistentResources &resources() { return resources_; }

  template <typename... Args>
  Compiler MakeChild(Args &&... args) {
    Compiler c(std::forward<Args>(args)...);
    c.builder().CurrentGroup() = builder().CurrentGroup();
    c.builder().CurrentBlock() = builder().CurrentBlock();
    return c;
  }

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
  }

  void Enqueue(WorkItem const &w,
               absl::flat_hash_set<WorkItem> prerequisites = {}) {
    resources_.enqueue(w, std::move(prerequisites));
  }
  void EnsureComplete(WorkItem const &w) { resources_.complete(w); }

  absl::Span<type::QualType const> VerifyType(ast::Node const *node) {
    return ast::Visitor<VerifyTypeTag,
                        absl::Span<type::QualType const>()>::Visit(node);
  }

  bool VerifyBody(ast::Node const *node) {
    return ast::Visitor<VerifyBodyTag, bool()>::Visit(node);
  }

  bool PatternMatch(ast::Node const *node, PatternMatchingContext &context,
                    absl::flat_hash_map<ast::Declaration::Id const *,
                                        ir::CompleteResultBuffer> &bindings) {
    return ast::Visitor<PatternMatchTag,
                        bool(PatternMatchingContext *,
                             absl::flat_hash_map<ast::Declaration::Id const *,
                                                 ir::CompleteResultBuffer>
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

  template <typename T>
  ir::RegOr<T> EmitWithCastTo(type::Type t, ast::Node const *node,
                              ir::PartialResultBuffer &buffer) {
    EmitToBuffer(node, buffer);
    auto result = builder().CastTo<T>(t, buffer[0]);
    buffer.clear();
    return result;
  }

  template <typename T>
  ir::RegOr<T> EmitWithCastTo(type::Type t, ast::Node const *node) {
    ir::PartialResultBuffer buffer;
    return EmitWithCastTo<T>(t, node, buffer);
  }

  template <typename T>
  ir::RegOr<T> EmitAs(ast::Node const *node, ir::PartialResultBuffer &buffer) {
    EmitToBuffer(node, buffer);
    auto result = buffer.get<T>(0);
    buffer.clear();
    return result;
  }

  void EmitVoid(ast::Node const *node) {
    ir::PartialResultBuffer buffer;
    EmitToBuffer(node, buffer);
  }

  template <typename T>
  ir::RegOr<T> EmitAs(ast::Node const *node) {
    ir::PartialResultBuffer buffer;
    return EmitAs<T>(node, buffer);
  }

  void EmitToBuffer(ast::Node const *node, ir::PartialResultBuffer &buffer) {
    ast::Visitor<EmitToBufferTag, void(ir::PartialResultBuffer *)>::Visit(
        node, &buffer);
  }

  void EmitMoveAssign(
      ast::Node const *node,
      absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs) {
    ast::Visitor<EmitMoveAssignTag,
                 void(absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const>)>::
        Visit(node, regs);
  }

  void EmitCopyAssign(
      ast::Node const *node,
      absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs) {
    ast::Visitor<EmitCopyAssignTag,
                 void(absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const>)>::
        Visit(node, regs);
  }

  void EmitCopyInit(ast::Node const *node,
                    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs) {
    ast::Visitor<EmitCopyInitTag,
                 void(absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const>)>::
        Visit(node, regs);
  }

  void EmitMoveInit(ast::Node const *node,
                    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs) {
    ast::Visitor<EmitMoveInitTag,
                 void(absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const>)>::
        Visit(node, regs);
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
                    ir::PartialResultBuffer const &from) {
    type::Visitor<EmitMoveInitTag, void(ir::Reg, ir::PartialResultBuffer const
                                                     *)>::Visit(to.type().get(),
                                                                *to, &from);
  }

  void EmitCopyInit(type::Typed<ir::Reg> to,
                    ir::PartialResultBuffer const &from) {
    type::Visitor<EmitCopyInitTag, void(ir::Reg, ir::PartialResultBuffer const
                                                     *)>::Visit(to.type().get(),
                                                                *to, &from);
  }

  void EmitMoveAssign(type::Typed<ir::RegOr<ir::addr_t>> const &to,
                      type::Typed<ir::PartialResultRef> const &from) {
    using V = type::Visitor<EmitMoveAssignTag,
                            void(ir::RegOr<ir::addr_t>,
                                 type::Typed<ir::PartialResultRef> const &)>;
    V::Visit(to.type().get(), to.get(), from);
  }

  void EmitCopyAssign(type::Typed<ir::RegOr<ir::addr_t>> const &to,
                      type::Typed<ir::PartialResultRef> const &from) {
    using V = type::Visitor<EmitCopyAssignTag,
                            void(ir::RegOr<ir::addr_t>,
                                 type::Typed<ir::PartialResultRef> const &)>;
    V::Visit(to.type().get(), to.get(), from);
  }

  explicit Compiler(PersistentResources const &resources);
  explicit Compiler(PersistentResources const &resources, TransientState state);
  Compiler(Compiler const &) = delete;
  Compiler(Compiler &&)      = default;

  Context &context() const { return *resources_.context; }
  ir::Builder &builder() { return builder_; };
  diagnostic::DiagnosticConsumer &diag() const {
    return *resources_.diagnostic_consumer;
  }
  ir::BasicBlock *current_block() { return builder().CurrentBlock(); }

  module::Importer &importer() const { return *resources_.importer; }

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
      if constexpr (type == base::meta<uint64_t>) { return type::U64; }
      if constexpr (type == base::meta<ir::Integer>) { return type::Integer; }
      if constexpr (type == base::meta<interface::Interface>) {
        return type::Interface;
      }
    }();
    auto result = EvaluateToBufferOrDiagnose(
        type::Typed<ast::Expression const *>(expr, t), must_complete);
    if (not result) return std::nullopt;
    return result->get<T>(0);
  }

  std::variant<ir::CompleteResultBuffer,
               std::vector<diagnostic::ConsumedMessage>>
  EvaluateToBuffer(type::Typed<ast::Expression const *> expr,
                   bool must_complete = true) {
    return resources().evaluate(expr, must_complete);
  }

  std::optional<ir::CompleteResultBuffer> EvaluateToBufferOrDiagnose(
      type::Typed<ast::Expression const *> expr, bool must_complete = true);

  interpreter::EvaluationResult Evaluate(
      type::Typed<ast::Expression const *> expr, bool must_complete = true);

  TransientState &state() { return state_; }

#define ICARUS_AST_NODE_X(name)                                                \
  absl::Span<type::QualType const> VerifyType(ast::name const *node);          \
  absl::Span<type::QualType const> Visit(VerifyTypeTag, ast::name const *node) \
      override {                                                               \
    return VerifyType(node);                                                   \
  }                                                                            \
                                                                               \
  bool Visit(VerifyBodyTag, ast::name const *node) override {                  \
    return VerifyBody(node);                                                   \
  }                                                                            \
                                                                               \
  void Visit(EmitToBufferTag, ast::name const *node,                           \
             ir::PartialResultBuffer *buffer) override {                       \
    EmitToBuffer(node, *buffer);                                               \
  }

#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

#define DEFINE_EMIT(name)                                                      \
  void EmitToBuffer(name const *node, ir::PartialResultBuffer &buffer);

  DEFINE_EMIT(ast::Access)
  DEFINE_EMIT(ast::ArgumentType)
  DEFINE_EMIT(ast::ArrayLiteral)
  DEFINE_EMIT(ast::ArrayType)
  DEFINE_EMIT(ast::Assignment)
  DEFINE_EMIT(ast::BinaryOperator)
  DEFINE_EMIT(ast::BindingDeclaration)
  DEFINE_EMIT(ast::BlockLiteral)
  DEFINE_EMIT(ast::BlockNode)
  DEFINE_EMIT(ast::BuiltinFn)
  DEFINE_EMIT(ast::Call)
  DEFINE_EMIT(ast::Cast)
  DEFINE_EMIT(ast::ComparisonOperator)
  DEFINE_EMIT(ast::ConditionalGoto)
  DEFINE_EMIT(ast::Declaration)
  DEFINE_EMIT(ast::Declaration_Id)
  DEFINE_EMIT(ast::DesignatedInitializer)
  DEFINE_EMIT(ast::EnumLiteral)
  DEFINE_EMIT(ast::FunctionLiteral)
  DEFINE_EMIT(ast::FunctionType)
  DEFINE_EMIT(ast::Identifier)
  DEFINE_EMIT(ast::Import)
  DEFINE_EMIT(ast::Index)
  DEFINE_EMIT(ast::InterfaceLiteral)
  DEFINE_EMIT(ast::Label)
  DEFINE_EMIT(ast::Jump)
  DEFINE_EMIT(ast::ParameterizedStructLiteral)
  DEFINE_EMIT(ast::PatternMatch)
  DEFINE_EMIT(ast::ReturnStmt)
  DEFINE_EMIT(ast::ScopeLiteral)
  DEFINE_EMIT(ast::ScopeNode)
  DEFINE_EMIT(ast::SliceType)
  DEFINE_EMIT(ast::ShortFunctionLiteral)
  DEFINE_EMIT(ast::StructLiteral)
  DEFINE_EMIT(ast::Terminal)
  DEFINE_EMIT(ast::UnaryOperator)
  DEFINE_EMIT(ast::UnconditionalGoto)
  DEFINE_EMIT(ast::YieldStmt)

#undef DEFINE_EMIT

#define DEFINE_PATTERN_MATCH(name)                                             \
  bool PatternMatch(name const *node, PatternMatchingContext &context,         \
                    absl::flat_hash_map<ast::Declaration::Id const *,          \
                                        ir::CompleteResultBuffer> &bindings);  \
  bool Visit(                                                                  \
      PatternMatchTag, name const *node, PatternMatchingContext *context,      \
      absl::flat_hash_map<ast::Declaration::Id const *,                        \
                          ir::CompleteResultBuffer> *bindings) override {      \
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
  DEFINE_PATTERN_MATCH(ast::Call)
  DEFINE_PATTERN_MATCH(ast::Declaration)
  DEFINE_PATTERN_MATCH(ast::SliceType)
  DEFINE_PATTERN_MATCH(ast::Terminal)
  DEFINE_PATTERN_MATCH(ast::UnaryOperator)
#undef DEFINE_PATTERN_MATCH

  // The reason to separate out type/body verification is if the body might
  // transitively have identifiers referring to a declaration that is assigned
  // directly to this node.
  bool VerifyBody(ast::Expression const *node) { return true; }
  bool VerifyBody(ast::EnumLiteral const *node);
  bool VerifyBody(ast::FunctionLiteral const *node);
  bool VerifyBody(ast::ParameterizedStructLiteral const *node);
  bool VerifyBody(ast::StructLiteral const *node);

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
  void Visit(EmitCopyAssignTag, T const *ty, ir::RegOr<ir::addr_t> r,          \
             type::Typed<ir::PartialResultRef> const &v) override {            \
    EmitCopyAssign(type::Typed<ir::RegOr<ir::addr_t>, T>(r, ty), v);           \
  }                                                                            \
  void EmitCopyAssign(type::Typed<ir::RegOr<ir::addr_t>, T> const &,           \
                      type::Typed<ir::PartialResultRef> const &);              \
                                                                               \
  void Visit(EmitMoveAssignTag, T const *ty, ir::RegOr<ir::addr_t> r,          \
             type::Typed<ir::PartialResultRef> const &v) override {            \
    EmitMoveAssign(type::Typed<ir::RegOr<ir::addr_t>, T>(r, ty), v);           \
  }                                                                            \
  void EmitMoveAssign(type::Typed<ir::RegOr<ir::addr_t>, T> const &r,          \
                      type::Typed<ir::PartialResultRef> const &);

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
             ir::PartialResultBuffer const *from) override {                   \
    EmitMoveInit(type::Typed<ir::Reg, T>(r, ty), *from);                       \
  }                                                                            \
  void EmitMoveInit(type::Typed<ir::Reg, T> to,                                \
                    ir::PartialResultBuffer const &from);                      \
                                                                               \
  void Visit(EmitCopyInitTag, T const *ty, ir::Reg r,                          \
             ir::PartialResultBuffer const *from) override {                   \
    EmitCopyInit(type::Typed<ir::Reg, T>(r, ty), *from);                       \
  }                                                                            \
  void EmitCopyInit(type::Typed<ir::Reg, T> to,                                \
                    ir::PartialResultBuffer const &from);

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
  void EmitCopyInit(                                                           \
      node_type const *node,                                                   \
      absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs);              \
  void Visit(EmitCopyInitTag, node_type const *node,                           \
             absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs)        \
      override {                                                               \
    return EmitCopyInit(node, regs);                                           \
  }                                                                            \
  void EmitMoveInit(                                                           \
      node_type const *node,                                                   \
      absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs);              \
  void Visit(EmitMoveInitTag, node_type const *node,                           \
             absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs)        \
      override {                                                               \
    return EmitMoveInit(node, regs);                                           \
  }                                                                            \
  void EmitMoveAssign(                                                         \
      node_type const *node,                                                   \
      absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs);              \
  void Visit(EmitMoveAssignTag, node_type const *node,                         \
             absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs)        \
      override {                                                               \
    return EmitMoveAssign(node, regs);                                         \
  }                                                                            \
  void EmitCopyAssign(                                                         \
      node_type const *node,                                                   \
      absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs);              \
  void Visit(EmitCopyAssignTag, node_type const *node,                         \
             absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs)        \
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

  type::QualType VerifyBinaryOverload(
      std::string_view symbol, ast::Expression const *node,
      type::Typed<ir::CompleteResultRef> const &lhs,
      type::Typed<ir::CompleteResultRef> const &rhs);

  ir::ModuleId EvaluateModuleWithCache(ast::Expression const *expr);

  std::optional<core::Params<type::QualType>> VerifyParams(
      core::Params<std::unique_ptr<ast::Declaration>> const &params);

  // TODO: The implementation here has some overlap with CompleteStruct.
  bool EnsureDataCompleteness(type::Struct *s);

  bool CompleteStruct(ast::StructLiteral const *node);
  bool CompleteStruct(ast::ParameterizedStructLiteral const *node);
  bool EmitJumpBody(ast::Jump const *node);
  bool EmitFunctionBody(ast::FunctionLiteral const *node);
  bool EmitShortFunctionBody(ast::ShortFunctionLiteral const *node);

 private:
  std::optional<core::Arguments<type::Typed<ir::CompleteResultRef>>>
  VerifyArguments(absl::Span<ast::Call::Argument const> args,
                  ir::CompleteResultBuffer &out);

  type::QualType VerifyUnaryOverload(
      char const *symbol, ast::Expression const *node,
      type::Typed<ir::CompleteResultRef> const &operand);

  std::variant<
      std::vector<type::QualType>,
      absl::flat_hash_map<type::Callable const *, core::CallabilityResult>>
  VerifyCall(ast::Call const *call_expr,
             absl::flat_hash_map<ast::Expression const *,
                                 type::Callable const *> const &overload_map,
             core::Arguments<type::Typed<ir::CompleteResultRef>> const &args);

  std::pair<type::QualType, absl::flat_hash_map<ast::Expression const *,
                                                type::Callable const *>>
  VerifyCallee(
      ast::Expression const *callee,
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

}  // namespace compiler

#endif  // ICARUS_COMPILER_COMPILER_H
