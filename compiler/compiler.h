#ifndef ICARUS_COMPILER_COMPILER_H
#define ICARUS_COMPILER_COMPILER_H

#include <functional>
#include <memory>
#include <optional>
#include <queue>

#include "absl/container/flat_hash_map.h"
#include "absl/types/span.h"
#include "ast/ast.h"
#include "base/any_invocable.h"
#include "base/debug.h"
#include "base/log.h"
#include "compiler/bound_parameters.h"
#include "compiler/compilation_data.h"
#include "compiler/context.h"
#include "compiler/instructions.h"
#include "compiler/resources.h"
#include "compiler/transient_state.h"
#include "compiler/work_item.h"
#include "core/call.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/source/buffer.h"
#include "frontend/source/view.h"
#include "ir/instruction/instructions.h"
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
#include "type/block.h"
#include "type/cast.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/opaque.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/qual_type.h"
#include "type/slice.h"
#include "type/struct.h"
#include "type/type.h"

namespace compiler {

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

template <typename C>
struct MoveInitEmitter {
  using signature = void(absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const>);

  template <typename NodeType>
  void operator()(NodeType const *node,
                  absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs) {
    return static_cast<C *>(this)->EmitMoveInit(node, regs);
  }
};

template <typename C>
struct CopyInitEmitter {
  using signature = void(absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const>);

  template <typename NodeType>
  void operator()(NodeType const *node,
                  absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs) {
    return static_cast<C *>(this)->EmitCopyInit(node, regs);
  }
};

template <typename C>
struct MoveAssignEmitter {
  using signature = void(absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const>);

  template <typename NodeType>
  void operator()(NodeType const *node,
                  absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs) {
    return static_cast<C *>(this)->EmitMoveAssign(node, regs);
  }
};

template <typename C>
struct CopyAssignEmitter {
  using signature = void(absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const>);

  template <typename NodeType>
  void operator()(NodeType const *node,
                  absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs) {
    return static_cast<C *>(this)->EmitCopyAssign(node, regs);
  }
};

template <typename C>
struct RefEmitter {
  using signature = ir::Reg();

  template <typename NodeType>
  ir::Reg operator()(NodeType const *node) {
    return static_cast<C *>(this)->EmitRef(node);
  }
};

template <typename C>
struct IrEmitter {
  using signature = void(ir::PartialResultBuffer &buffer);

  template <typename NodeType>
  void operator()(NodeType const *node, ir::PartialResultBuffer &buffer) {
    return static_cast<C *>(this)->EmitToBuffer(node, buffer);
  }
};

template <typename C>
struct PatternMatcher {
  using signature = bool(PatternMatchingContext &,
                         absl::flat_hash_map<ast::Declaration::Id const *,
                                             ir::CompleteResultBuffer> &);

  template <typename NodeType>
  bool operator()(NodeType const *node, PatternMatchingContext &context,
                  absl::flat_hash_map<ast::Declaration::Id const *,
                                      ir::CompleteResultBuffer> &bindings) {
    return static_cast<C *>(this)->PatternMatch(node, context, bindings);
  }
};

struct Compiler : CompilationDataReference,
                  MoveInitEmitter<Compiler>,
                  CopyInitEmitter<Compiler>,
                  MoveAssignEmitter<Compiler>,
                  CopyAssignEmitter<Compiler>,
                  RefEmitter<Compiler>,
                  IrEmitter<Compiler>,
                  PatternMatcher<Compiler> {
  explicit Compiler(CompilationData *data) : CompilationDataReference(data){};
  explicit Compiler(CompilationDataReference ref)
      : CompilationDataReference(ref){};
  Compiler(Compiler const &) = delete;
  Compiler(Compiler &&)      = delete;

  void EmitMoveInit(ast::Node const *node,
                    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs) {
    return node->visit<MoveInitEmitter<Compiler>>(*this, regs);
  }

  void EmitCopyInit(ast::Node const *node,
                    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs) {
    return node->visit<CopyInitEmitter<Compiler>>(*this, regs);
  }

  void EmitMoveAssign(
      ast::Node const *node,
      absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs) {
    return node->visit<MoveAssignEmitter<Compiler>>(*this, regs);
  }

  void EmitCopyAssign(
      ast::Node const *node,
      absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs) {
    return node->visit<CopyAssignEmitter<Compiler>>(*this, regs);
  }

  ir::Reg EmitRef(ast::Node const *node) {
    return node->visit<RefEmitter<Compiler>>(*this);
  }

  void EmitToBuffer(ast::Node const *node, ir::PartialResultBuffer &buffer) {
    node->visit<IrEmitter<Compiler>>(*this, buffer);
  }

  bool PatternMatch(ast::Node const *node, PatternMatchingContext &context,
                    absl::flat_hash_map<ast::Declaration::Id const *,
                                        ir::CompleteResultBuffer> &bindings) {
    return node->visit<PatternMatcher<Compiler>>(*this, context, bindings);
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

  // Evaluates `expr` in the current context as a value of type `T`. If
  // evaluation succeeds, returns the vaule, otherwise adds a diagnostic for the
  // failure and returns `nullopt`. If the expresison is no tof type `T`, the
  // behavior is undefined.
  template <typename T>
  std::optional<T> EvaluateOrDiagnoseAs(ast::Expression const *expr) {
    type::Type t = [] {
      constexpr auto type = base::meta<T>;
      if constexpr (type == base::meta<type::Type>) { return type::Type_; }
      if constexpr (type == base::meta<bool>) { return type::Bool; }
      if constexpr (type == base::meta<ir::Scope>) { return type::Scp({}); }
      if constexpr (type == base::meta<ir::Block>) { return type::Blk({}); }
      if constexpr (type == base::meta<ir::ModuleId>) { return type::Module; }
      if constexpr (type == base::meta<uint64_t>) { return type::U64; }
      if constexpr (type == base::meta<ir::Integer>) { return type::Integer; }
      if constexpr (type == base::meta<ir::UnboundScope>) {
        return type::UnboundScope;
      }
      if constexpr (type == base::meta<ir::ScopeContext>) {
        return type::ScopeContext;
      }
      if constexpr (type == base::meta<interface::Interface>) {
        return type::Interface;
      }
    }();
    auto result = EvaluateToBufferOrDiagnose(
        type::Typed<ast::Expression const *>(expr, t));
    if (not result) return std::nullopt;
    return result->get<T>(0);
  }

#define ICARUS_AST_NODE_X(name)                                                \
  void EmitToBuffer(ast::name const *node, ir::PartialResultBuffer &buffer);

#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

#define DEFINE_PATTERN_MATCH(name)                                             \
  bool PatternMatch(name const *node, PatternMatchingContext &context,         \
                    absl::flat_hash_map<ast::Declaration::Id const *,          \
                                        ir::CompleteResultBuffer> &bindings);

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

  ir::Reg EmitRef(ast::Access const *node);
  ir::Reg EmitRef(ast::Identifier const *node);
  ir::Reg EmitRef(ast::Index const *node);
  ir::Reg EmitRef(ast::UnaryOperator const *node);

#define DEFINE_EMIT(node_type)                                                 \
  void EmitCopyInit(                                                           \
      node_type const *node,                                                   \
      absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs);              \
  void EmitMoveInit(                                                           \
      node_type const *node,                                                   \
      absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs);              \
  void EmitMoveAssign(                                                         \
      node_type const *node,                                                   \
      absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs);              \
  void EmitCopyAssign(                                                         \
      node_type const *node,                                                   \
      absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> regs);

  DEFINE_EMIT(ast::Access)
  DEFINE_EMIT(ast::ArrayLiteral)
  DEFINE_EMIT(ast::ArrayType)
  DEFINE_EMIT(ast::BinaryOperator)
  DEFINE_EMIT(ast::Call)
  DEFINE_EMIT(ast::Cast)
  DEFINE_EMIT(ast::ComparisonOperator)
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

  bool CompleteStruct(ast::StructLiteral const *node);
  bool CompleteStruct(ast::ParameterizedStructLiteral const *node);
  bool CompleteEnum(ast::EnumLiteral const *node);
  bool EmitScopeBody(ast::ScopeLiteral const *node);
  bool EmitFunctionBody(ast::FunctionLiteral const *node);
  bool EmitShortFunctionBody(ast::ShortFunctionLiteral const *node);
  bool EmitParameterizedStructFunctionBody(
      ast::ParameterizedStructLiteral const *node);

  void DestroyTemporaries() {
    for (auto iter = state().temporaries_to_destroy.rbegin();
         iter != state().temporaries_to_destroy.rend(); ++iter) {
      // TODO:EmitDestroy(*iter);
    }
    state().temporaries_to_destroy.clear();
  }

  ir::OutParams OutParams(
      absl::Span<type::Type const> types,
      absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to = {}) {
    std::vector<ir::Reg> regs;
    regs.reserve(types.size());
    for (size_t i = 0; i < types.size(); ++i) {
      regs.push_back(
          types[i].get()->is_big()
              ? (to.empty() ? state().TmpAlloca(types[i]) : to[i]->reg())
              : current().subroutine->Reserve());
    }
    return ir::OutParams(std::move(regs));
  }
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_COMPILER_H
