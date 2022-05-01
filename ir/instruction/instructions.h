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
#include "ir/instruction/foreign.h"
#include "ir/interpreter/interpreter.h"
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

  friend bool InterpretInstruction(interpreter::Interpreter& interpreter,
                                   CastInstruction const& inst) {
    if constexpr (interpreter::FitsInRegister<from_type>) {
      if constexpr (base::meta<to_type> == base::meta<Char>) {
        static_assert(sizeof(from_type) == 1, "Invalid cast to Char");
        interpreter.frame().set(inst.result,
                                ir::Char(static_cast<uint8_t>(
                                    interpreter.frame().resolve(inst.value))));
      }
      from_type from = interpreter.frame().resolve(inst.value);
      if constexpr (std::is_integral_v<from_type> or
                    std::is_floating_point_v<from_type>) {
        interpreter.frame().set(inst.result, static_cast<to_type>(from));
      } else {
        interpreter.frame().set(inst.result, from.template as_type<to_type>());
      }
    } else {
      interpreter.frame().set(inst.result,
                              reinterpret_cast<from_type const*>(
                                  interpreter.frame().resolve(inst.value))
                                  ->template as_type<to_type>());
    }
    return true;
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

  friend bool InterpretInstruction(interpreter::Interpreter& interpreter,
                                   CastInstruction const& inst) {
    if constexpr (interpreter::FitsInRegister<from_type>) {
      // Casting is a construction operation, but for to-types that don't fit in
      // registers that requires allocation first.
      new (interpreter.frame().resolve(inst.into))
          to_type(interpreter.frame().resolve(inst.value));
    } else {
      new (interpreter.frame().resolve(inst.into))
          to_type(*reinterpret_cast<from_type const*>(
              interpreter.frame().resolve(inst.value)));
    }
    return true;
  }

  RegOr<from_operand_type> value;
  RegOr<addr_t> into;
};

struct PtrDiffInstruction
    : base::Extend<PtrDiffInstruction>::With<base::BaseSerializeExtension,
                                             DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%4$s = ptrdiff %1$s %2$s";

  friend bool InterpretInstruction(ir::interpreter::Interpreter& interpreter,
                                   PtrDiffInstruction const& inst) {
    interpreter.frame().set(inst.result,
                            (interpreter.frame().resolve(inst.lhs) -
                             interpreter.frame().resolve(inst.rhs)) /
                                inst.pointee_type.bytes(core::Host).value());
    return true;
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

  friend bool InterpretInstruction(interpreter::Interpreter& interpreter,
                                   NotInstruction const& inst) {
    interpreter.frame().set(inst.result,
                            not interpreter.frame().resolve(inst.operand));
    return true;
  }

  RegOr<bool> operand;
  Reg result;
};

// TODO Morph this into interpreter break-point instructions.
struct DebugIrInstruction
    : base::Extend<DebugIrInstruction>::With<base::BaseTraverseExtension,
                                             base::BaseSerializeExtension> {
  std::string to_string() const { return "debug-ir"; }
  friend std::ostream& operator<<(std::ostream& os,
                                  DebugIrInstruction const& inst) {
    return os << inst.to_string();
  }

  friend bool InterpretInstruction(interpreter::Interpreter& interpreter,
                                   DebugIrInstruction const& inst) {
    std::cerr << *ASSERT_NOT_NULL(inst.fn);
    return true;
  }

  Subroutine const* fn;  // TODO: Fix this.
};

struct InitInstruction
    : base::Extend<InitInstruction>::With<base::BaseSerializeExtension,
                                          DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "init %2$s";

  friend bool InterpretInstruction(interpreter::Interpreter& interpreter,
                                   InitInstruction const& inst) {
    if (auto* s = inst.type.if_as<type::Struct>()) {
      return interpreter.push_frame(
          *s->init_,
          CompleteResultBuffer(interpreter.frame().resolve<addr_t>(inst.reg)),
          {});

    } else if (auto* a = inst.type.if_as<type::Array>()) {
      return interpreter.push_frame(
          a->Initializer(),
          CompleteResultBuffer(interpreter.frame().resolve<addr_t>(inst.reg)),
          {});

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

  friend bool InterpretInstruction(interpreter::Interpreter& interpreter,
                                   DestroyInstruction const& inst) {
    return interpreter.push_frame(
        inst.function(),
        CompleteResultBuffer(interpreter.frame().resolve(inst.addr)), {});
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

  friend bool InterpretInstruction(interpreter::Interpreter& interpreter,
                                   CopyInstruction const& inst) {
    return interpreter.push_frame(
        inst.function(),
        CompleteResultBuffer(interpreter.frame().resolve(inst.to),
                             interpreter.frame().resolve(inst.from)),
        {});
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

  friend bool InterpretInstruction(interpreter::Interpreter& interpreter,
                                   CopyInitInstruction const& inst) {
    return interpreter.push_frame(
        inst.function(),
        CompleteResultBuffer(interpreter.frame().resolve(inst.to),
                             interpreter.frame().resolve(inst.from)),
        {});
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

  friend bool InterpretInstruction(interpreter::Interpreter& interpreter,
                                   CompileTime const& inst) {
    auto* to_ptr = reinterpret_cast<T*>(interpreter.frame().resolve(inst.to));
    auto* from_ptr =
        reinterpret_cast<T*>(interpreter.frame().resolve(inst.from));
    if constexpr (A == Action::CopyInit) {
      new (to_ptr) T(*from_ptr);
    } else if constexpr (A == Action::MoveInit) {
      new (to_ptr) T(std::move(*from_ptr));
    } else if constexpr (A == Action::CopyAssign) {
      *to_ptr = *from_ptr;
    } else if constexpr (A == Action::MoveAssign) {
      *to_ptr = std::move(*from_ptr);
    }
    return true;
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

  friend bool InterpretInstruction(interpreter::Interpreter& interpreter,
                                   CompileTime const& inst) {
    reinterpret_cast<T*>(interpreter.frame().resolve(inst.addr))->~T();
    return true;
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

  friend bool InterpretInstruction(interpreter::Interpreter& interpreter,
                                   MoveInstruction const& inst) {
    return interpreter.push_frame(
        inst.function(),
        CompleteResultBuffer(interpreter.frame().resolve(inst.to),
                             interpreter.frame().resolve(inst.from)),
        {});
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

  friend bool InterpretInstruction(interpreter::Interpreter& interpreter,
                                   MoveInitInstruction const& inst) {
    return interpreter.push_frame(
        inst.function(),
        CompleteResultBuffer(interpreter.frame().resolve(inst.to),
                             interpreter.frame().resolve(inst.from)),
        {});
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

struct LoadDataSymbolInstruction
    : base::Extend<LoadDataSymbolInstruction>::With<
          base::BaseSerializeExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = load-symbol %1$s";

  friend bool InterpretInstruction(ir::interpreter::Interpreter& interpreter,
                                   LoadDataSymbolInstruction const& inst) {
    absl::StatusOr<void*> sym = LoadDataSymbol(inst.name);
    if (not sym.ok()) {
      interpreter.FatalError(sym.status().message());
      return false;
    }
    interpreter.frame().set(inst.result, Addr(*sym));
    return true;
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

  friend bool InterpretInstruction(ir::interpreter::Interpreter& interpreter,
                                   StructIndexInstruction const& inst) {
    interpreter.frame().set(
        inst.result,
        interpreter.frame().resolve(inst.addr) +
            inst.struct_type
                ->offset(interpreter.frame().resolve(inst.index), core::Host)
                .value());
    return true;
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

  friend bool InterpretInstruction(interpreter::Interpreter& interpreter,
                                   PtrIncrInstruction const& inst) {
    interpreter.frame().set(
        inst.result,
        interpreter.frame().resolve(inst.addr) +
            core::FwdAlign(inst.ptr->pointee().bytes(core::Host),
                           inst.ptr->pointee().alignment(core::Host))
                    .value() *
                interpreter.frame().resolve(inst.index));
    return true;
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

  friend bool InterpretInstruction(interpreter::Interpreter& interpreter,
                                   AndInstruction const& inst) {
    interpreter.frame().set(inst.result,
                            (interpreter.frame().resolve(inst.lhs) and
                             interpreter.frame().resolve(inst.rhs)));
    return true;
  }

  RegOr<bool> lhs;
  RegOr<bool> rhs;
  Reg result;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_INSTRUCTIONS_H
