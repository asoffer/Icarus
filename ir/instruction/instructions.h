#ifndef ICARUS_IR_INSTRUCTION_INSTRUCTIONS_H
#define ICARUS_IR_INSTRUCTION_INSTRUCTIONS_H

#include <memory>

#include "absl/algorithm/container.h"
#include "absl/container/flat_hash_map.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "ast/scope.h"
#include "base/extend.h"
#include "base/extend/traverse.h"
#include "base/meta.h"
#include "ir/instruction/base.h"
#include "ir/instruction/debug.h"
#include "ir/interpreter/architecture.h"
#include "ir/interpreter/execution_context.h"
#include "ir/interpreter/foreign.h"
#include "ir/interpreter/stack_frame.h"
#include "ir/out_params.h"
#include "ir/subroutine.h"
#include "ir/value/fn.h"
#include "ir/value/reg_or.h"
#include "ir/value/scope.h"
#include "type/array.h"
#include "type/pointer.h"
#include "type/struct.h"
#include "type/type.h"

// This file defines the interface required for IR instructions as well as all
// the common instructions available in the core IR.
namespace ir {

template <typename>
struct CastInstruction;

template <interpreter::FitsInRegister ToType, typename FromType>
struct CastInstruction<ToType(FromType)>
    : base::Extend<CastInstruction<ToType(FromType)>>::template With<
          base::BaseTraverseExtension, base::BaseSerializeExtension> {
  using from_type = FromType;
  using to_type   = ToType;
  using from_operand_type =
      std::conditional_t<interpreter::FitsInRegister<from_type>, from_type,
                         addr_t>;

  std::string to_string() const {
    return absl::StrFormat("%s = cast %s (%s -> %s)",
                           base::UniversalPrintToString(result),
                           base::UniversalPrintToString(value),
                           typeid(from_type).name(), typeid(to_type).name());
  }

  void Apply(interpreter::ExecutionContext& ctx) const
      requires(not interpreter::FitsInRegister<from_type>) {
    ctx.current_frame().set(
        result, reinterpret_cast<from_type const*>(ctx.resolve(value))
                    ->template as_type<to_type>());
  }

  to_type Resolve() const requires(interpreter::FitsInRegister<from_type>) {
    if constexpr (base::meta<to_type> == base::meta<Char>) {
      static_assert(sizeof(from_type) == 1, "Invalid cast to Char");
      return static_cast<uint8_t>(value.value());
    }
    from_type from = value.value();
    if constexpr (std::is_integral_v<from_type> or
                  std::is_floating_point_v<from_type>) {
      return from;
    } else {
      return from.template as_type<to_type>();
    }
  }

  RegOr<from_operand_type> value;
  Reg result;
};

template <interpreter::NotFitsInRegister ToType, typename FromType>
struct CastInstruction<ToType(FromType)>
    : base::Extend<CastInstruction<ToType(FromType)>>::template With<
          base::BaseTraverseExtension, base::BaseSerializeExtension> {
  using from_type = FromType;
  using to_type   = ToType;
  using from_operand_type =
      std::conditional_t<interpreter::FitsInRegister<from_type>, from_type,
                         addr_t>;

  std::string to_string() const {
    return absl::StrFormat("%s = cast %s (%s -> %s)",
                           base::UniversalPrintToString(into),
                           base::UniversalPrintToString(value),
                           typeid(from_type).name(), typeid(to_type).name());
  }

  void Apply(interpreter::ExecutionContext& ctx) const {
    if constexpr (interpreter::FitsInRegister<from_type>) {
      // Casting is a construction operation, but for to-types that don't fit in
      // registers that requires allocation first.
      new (ctx.resolve(into)) to_type(ctx.resolve(value));
    } else {
      new (ctx.resolve(into))
          to_type(*reinterpret_cast<from_type const*>(ctx.resolve(value)));
    }
  }

  RegOr<from_operand_type> value;
  RegOr<addr_t> into;
};

template <typename ToType, typename FromType>
struct CastInstruction<ToType(FromType)>
    : base::Extend<CastInstruction<ToType(FromType)>>::template With<
          base::BaseTraverseExtension, base::BaseSerializeExtension> {
  using from_type = FromType;
  using to_type   = ToType;
  using from_operand_type =
      std::conditional_t<interpreter::FitsInRegister<from_type>, from_type,
                         addr_t>;

  std::string to_string() const {
    return absl::StrFormat("%s = cast %s (%s -> %s)",
                           base::UniversalPrintToString(result),
                           base::UniversalPrintToString(value),
                           typeid(from_type).name(), typeid(to_type).name());
  }

  void Apply(interpreter::ExecutionContext& ctx) const
      requires(not interpreter::FitsInRegister<from_type> or
               not interpreter::FitsInRegister<to_type>) {
    if constexpr (interpreter::FitsInRegister<from_type>) {
      // Casting is a construction operation, but for to-types that don't fit in
      // registers that requires allocation first.
      new (ctx.resolve<addr_t>(result)) to_type(ctx.resolve(value));
    } else if constexpr (interpreter::FitsInRegister<to_type>) {
      ctx.current_frame().set(
          result, reinterpret_cast<from_type const*>(ctx.resolve(value))
                      ->template as_type<to_type>());
    } else {
      new (ctx.resolve<addr_t>(result))
          to_type(*reinterpret_cast<from_type const*>(ctx.resolve(value)));
    }
  }

  to_type Resolve() const requires(interpreter::FitsInRegister<from_type>and
                                       interpreter::FitsInRegister<to_type>) {
    if constexpr (base::meta<to_type> == base::meta<Char>) {
      static_assert(sizeof(from_type) == 1, "Invalid cast to Char");
      return static_cast<uint8_t>(value.value());
    }
    from_type from = value.value();
    if constexpr (std::is_integral_v<from_type> or
                  std::is_floating_point_v<from_type>) {
      return from;
    } else {
      return from.template as_type<to_type>();
    }
  }

  RegOr<from_operand_type> value;
  Reg result;
};

struct PtrDiffInstruction
    : base::Extend<PtrDiffInstruction>::With<base::BaseSerializeExtension,
                                             DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = ptrdiff %1$s %2$s";

  void Apply(interpreter::ExecutionContext& ctx) const {
    ctx.current_frame().set(
        result, (ctx.resolve(lhs) - ctx.resolve(rhs)) /
                    pointee_type.bytes(interpreter::kArchitecture).value());
  }

  friend void BaseTraverse(Inliner& inl, PtrDiffInstruction& inst) {
    base::Traverse(inl, inst.lhs, inst.rhs, inst.result);
  }

  RegOr<addr_t> lhs;
  RegOr<addr_t> rhs;
  type::Type pointee_type;
  Reg result;
};

struct NotInstruction
    : base::Extend<NotInstruction>::With<base::BaseTraverseExtension,
                                         base::BaseSerializeExtension,
                                         DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = not %1$s";

  bool Resolve() const { return Apply(operand.value()); }
  static bool Apply(bool operand) { return not operand; }

  RegOr<bool> operand;
  Reg result;
};

// TODO Morph this into interpreter break-point instructions.
struct DebugIrInstruction
    : base::Extend<DebugIrInstruction>::With<base::BaseTraverseExtension,
                                             base::BaseSerializeExtension,
                                             DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "debug-ir(%s)";

  void Apply(interpreter::ExecutionContext&) const { std::cerr << *fn; }
  Subroutine const* fn;
};

struct InitInstruction
    : base::Extend<InitInstruction>::With<base::BaseSerializeExtension,
                                          DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "init %2$s";

  interpreter::StackFrame Apply(interpreter::ExecutionContext& ctx) const {
    if (auto* s = type.if_as<type::Struct>()) {
      Fn f = *s->init_;
      interpreter::StackFrame frame(&f.native().byte_code(), ctx.stack());
      frame.set(Reg::Parameter(0), ctx.resolve<addr_t>(reg));
      return frame;

    } else if (auto* a = type.if_as<type::Array>()) {
      Fn f = a->Initializer();
      interpreter::StackFrame frame(&f.native().byte_code(), ctx.stack());
      frame.set(Reg::Parameter(0), ctx.resolve<addr_t>(reg));
      return frame;

    } else {
      NOT_YET();
    }
  }

  friend void BaseTraverse(Inliner& inl, InitInstruction& inst) {
    inl(inst.reg);
  }

  type::Type type;
  Reg reg;
};

struct DestroyInstruction
    : base::Extend<DestroyInstruction>::With<base::BaseSerializeExtension,
                                             DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "destroy %2$s";

  interpreter::StackFrame Apply(interpreter::ExecutionContext& ctx) const {
    // TODO: Might not be native.
    interpreter::StackFrame frame(&function().native().byte_code(),
                                  ctx.stack());
    frame.set(Reg::Parameter(0), ctx.resolve(addr));
    return frame;
  }

  friend void BaseTraverse(Inliner& inl, DestroyInstruction& inst) {
    inl(inst.addr);
  }

  type::Type type;
  RegOr<addr_t> addr;

 private:
  Fn function() const {
    if (auto* s = type.if_as<type::Struct>()) {
      ASSERT(s->dtor_.has_value() == true);
      return *s->dtor_;
    } else if (auto* a = type.if_as<type::Array>()) {
      return a->Destructor();
    } else {
      NOT_YET();
    }
  }
};

struct CopyInstruction
    : base::Extend<CopyInstruction>::With<base::BaseSerializeExtension,
                                          DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "copy %2$s to %3$s";

  interpreter::StackFrame Apply(interpreter::ExecutionContext& ctx) const {
    // TODO: Might not be native.
    interpreter::StackFrame frame(&function().native().byte_code(),
                                  ctx.stack());
    frame.set(Reg::Parameter(0), ctx.resolve<addr_t>(to));
    frame.set(Reg::Parameter(1), ctx.resolve<addr_t>(from));
    return frame;
  }

  friend void BaseTraverse(Inliner& inl, CopyInstruction& inst) {
    base::Traverse(inl, inst.from, inst.to);
  }

  type::Type type;
  RegOr<addr_t> from;
  RegOr<addr_t> to;

 private:
  Fn function() const {
    if (auto* s = type.if_as<type::Struct>()) {
      ASSERT(s->completeness() == type::Completeness::Complete);
      return *ASSERT_NOT_NULL(s->CopyAssignment(s));
    } else if (auto* a = type.if_as<type::Array>()) {
      return a->CopyAssign();
    } else {
      NOT_YET();
    }
  }
};

struct CopyInitInstruction
    : base::Extend<CopyInitInstruction>::With<base::BaseSerializeExtension,
                                              DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "copy-init %2$s to %3$s";

  interpreter::StackFrame Apply(interpreter::ExecutionContext& ctx) const {
    // TODO: Might not be native.
    interpreter::StackFrame frame(&function().native().byte_code(),
                                  ctx.stack());
    frame.set(Reg::Parameter(0), ctx.resolve<addr_t>(from));
    frame.set(Reg::Output(0), ctx.resolve<addr_t>(to));
    return frame;
  }

  friend void BaseTraverse(Inliner& inl, CopyInitInstruction& inst) {
    base::Traverse(inl, inst.from, inst.to);
  }

  type::Type type;
  RegOr<addr_t> from;
  RegOr<addr_t> to;

 private:
  Fn function() const {
    if (auto* s = type.if_as<type::Struct>()) {
      ASSERT(s->completeness() == type::Completeness::Complete);
      return *ASSERT_NOT_NULL(s->CopyInit(s));
    } else if (auto* a = type.if_as<type::Array>()) {
      return a->CopyInit();
    } else {
      NOT_YET();
    }
  }
};

enum class Action { CopyInit, MoveInit, CopyAssign, MoveAssign, Destroy };
template <Action A, typename T>
struct CompileTime : base::Extend<CompileTime<A, T>>::template With<
                         base::BaseSerializeExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = [] {
    if constexpr (A == Action::CopyInit) { return "copy-init %1$s %2$s"; }
    if constexpr (A == Action::MoveInit) { return "move-init %1$s %2$s"; }
    if constexpr (A == Action::CopyAssign) { return "copy-assign %1$s %2$s"; }
    if constexpr (A == Action::MoveAssign) { return "move-assign %1$s %2$s"; }
  }();

  void Apply(interpreter::ExecutionContext& ctx) const {
    auto* to_ptr   = reinterpret_cast<T*>(ctx.resolve(to));
    auto* from_ptr = reinterpret_cast<T*>(ctx.resolve(from));
    if constexpr (A == Action::CopyInit) {
      new (to_ptr) T(*from_ptr);
    } else if constexpr (A == Action::MoveInit) {
      new (to_ptr) T(std::move(*from_ptr));
    } else if constexpr (A == Action::CopyAssign) {
      *to_ptr = *from_ptr;
    } else if constexpr (A == Action::MoveAssign) {
      *to_ptr = std::move(*from_ptr);
    }
  }

  friend void BaseTraverse(Inliner& inl, CompileTime& inst) {
    base::Traverse(inl, inst.from, inst.to);
  }

  RegOr<addr_t> from;
  RegOr<addr_t> to;
};

template <typename T>
struct CompileTime<Action::Destroy, T>
    : base::Extend<CompileTime<Action::Destroy, T>>::template With<
          base::BaseSerializeExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "destroy %1$s";

  void Apply(interpreter::ExecutionContext& ctx) const {
    reinterpret_cast<T*>(ctx.resolve(addr)).~T();
  }

  friend void BaseTraverse(Inliner& inl, CompileTime& inst) {
    base::Traverse(inl, inst.addr);
  }

  RegOr<addr_t> addr;
};

struct MoveInstruction
    : base::Extend<MoveInstruction>::With<base::BaseSerializeExtension,
                                          DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "move %2$s to %3$s";

  interpreter::StackFrame Apply(interpreter::ExecutionContext& ctx) const {
    // TODO: Might not be native.
    interpreter::StackFrame frame(&function().native().byte_code(),
                                  ctx.stack());
    frame.set(Reg::Parameter(0), ctx.resolve<addr_t>(to));
    frame.set(Reg::Parameter(1), ctx.resolve<addr_t>(from));
    return frame;
  }

  friend void BaseTraverse(Inliner& inl, MoveInstruction& inst) {
    base::Traverse(inl, inst.from, inst.to);
  }

  type::Type type;
  RegOr<addr_t> from;
  RegOr<addr_t> to;

 private:
  Fn function() const {
    if (auto* s = type.if_as<type::Struct>()) {
      ASSERT(s->completeness() == type::Completeness::Complete);
      return *ASSERT_NOT_NULL(s->MoveAssignment(s));
    } else if (auto* a = type.if_as<type::Array>()) {
      return a->MoveAssign();
    } else {
      NOT_YET();
    }
  }
};

struct MoveInitInstruction
    : base::Extend<MoveInitInstruction>::With<base::BaseSerializeExtension,
                                              DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "move-init %2$s to %3$s";

  interpreter::StackFrame Apply(interpreter::ExecutionContext& ctx) const {
    // TODO: Might not be native.
    interpreter::StackFrame frame(&function().native().byte_code(),
                                  ctx.stack());
    frame.set(Reg::Parameter(0), ctx.resolve<addr_t>(from));
    frame.set(Reg::Output(0), ctx.resolve<addr_t>(to));
    return frame;
  }

  friend void BaseTraverse(Inliner& inl, MoveInitInstruction& inst) {
    base::Traverse(inl, inst.from, inst.to);
  }

  type::Type type;
  RegOr<addr_t> from;
  RegOr<addr_t> to;

 private:
  Fn function() const {
    if (auto* s = type.if_as<type::Struct>()) {
      ASSERT(s->completeness() == type::Completeness::Complete);
      return *ASSERT_NOT_NULL(s->MoveInit(s));
    } else if (auto* a = type.if_as<type::Array>()) {
      return a->MoveInit();
    } else {
      NOT_YET();
    }
  }
};

[[noreturn]] inline void FatalInterpreterError(std::string_view err_msg) {
  // TODO: Add a diagnostic explaining the failure.
  absl::FPrintF(stderr,
                R"(
  ----------------------------------------
  Fatal interpreter failure:
    %s
  ----------------------------------------)"
                "\n",
                err_msg);
  std::terminate();
}

struct LoadDataSymbolInstruction
    : base::Extend<LoadDataSymbolInstruction>::With<
          base::BaseSerializeExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = load-symbol %1$s";

  void Apply(interpreter::ExecutionContext& ctx) const {
    absl::StatusOr<void*> sym = interpreter::LoadDataSymbol(name);
    if (not sym.ok()) { FatalInterpreterError(sym.status().message()); }
    ctx.current_frame().set(result, Addr(*sym));
  }

  friend void BaseTraverse(Inliner& inl, LoadDataSymbolInstruction& inst) {
    inl(inst.result);
  }

  std::string name;
  Reg result;
};

struct StructIndexInstruction
    : base::Extend<StructIndexInstruction>::With<base::BaseTraverseExtension,
                                                 base::BaseSerializeExtension,
                                                 DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat =
      "%4$s = index %2$s of %1$s (struct %3$s)";

  addr_t Resolve() const {
    return addr.value() +
           struct_type->offset(index.value(), interpreter::kArchitecture)
               .value();
  }

  RegOr<addr_t> addr;
  RegOr<int64_t> index;
  ::type::Struct const* struct_type;
  Reg result;
};

struct PtrIncrInstruction
    : base::Extend<PtrIncrInstruction>::With<base::BaseTraverseExtension,
                                             base::BaseSerializeExtension,
                                             DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat =
      "%4$s = index %2$s of %1$s (pointer %3$s)";

  addr_t Resolve() const {
    return addr.value() +
           (core::FwdAlign(
                ptr->pointee().bytes(interpreter::kArchitecture),
                ptr->pointee().alignment(interpreter::kArchitecture)) *
            index.value())
               .value();
  }

  RegOr<addr_t> addr;
  RegOr<int64_t> index;
  ::type::Pointer const* ptr;
  Reg result;
};

struct AndInstruction
    : base::Extend<AndInstruction>::With<base::BaseTraverseExtension,
                                         base::BaseSerializeExtension,
                                         DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = and %1$s %2$s";

  bool Resolve() const { return Apply(lhs.value(), rhs.value()); }
  static bool Apply(bool lhs, bool rhs) { return lhs and rhs; }

  RegOr<bool> lhs;
  RegOr<bool> rhs;
  Reg result;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_INSTRUCTIONS_H
