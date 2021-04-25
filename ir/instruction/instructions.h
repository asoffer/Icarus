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
#include "base/meta.h"
#include "ir/compiled_block.h"
#include "ir/compiled_scope.h"
#include "ir/instruction/base.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/inliner.h"
#include "ir/interpreter/architecture.h"
#include "ir/interpreter/execution_context.h"
#include "ir/interpreter/foreign.h"
#include "ir/interpreter/stack_frame.h"
#include "ir/out_params.h"
#include "ir/value/block.h"
#include "ir/value/fn.h"
#include "ir/value/generic_fn.h"
#include "ir/value/reg_or.h"
#include "ir/value/scope.h"
#include "ir/value/string.h"
#include "type/array.h"
#include "type/generic_function.h"
#include "type/pointer.h"
#include "type/struct.h"
#include "type/type.h"

// This file defines the interface required for IR instructions as well as all
// the common instructions available in the core IR.
namespace ir {

template <typename>
struct CastInstruction;

template <typename ToType, typename FromType>
struct CastInstruction<ToType(FromType)>
    : base::Extend<CastInstruction<ToType(FromType)>>::template With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  using from_type                                = FromType;
  using to_type                                  = ToType;
  static constexpr std::string_view kDebugFormat = "%2$s = cast %1$s";

  ToType Resolve() const {
    if constexpr (base::meta<ToType> == base::meta<ir::Char>) {
      if constexpr (sizeof(FromType) == 1) {
        return static_cast<uint8_t>(value.value());
      } else {
        UNREACHABLE();
      }
    } else {
      return value.value();
    }
  }

  RegOr<FromType> value;
  Reg result;
};

struct PtrDiffInstruction
    : base::Extend<PtrDiffInstruction>::With<ByteCodeExtension, InlineExtension,
                                             DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = ptrdiff %1$s %2$s";

  void Apply(interpreter::ExecutionContext& ctx) const {
    ctx.current_frame().regs_.set(
        result, (ctx.resolve(lhs) - ctx.resolve(rhs)) /
                    pointee_type.bytes(interpreter::kArchitecture));
  }

  RegOr<Addr> lhs;
  RegOr<Addr> rhs;
  type::Type pointee_type;
  Reg result;
};

struct NotInstruction
    : base::Extend<NotInstruction>::With<ByteCodeExtension, InlineExtension,
                                         DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = not %1$s";

  bool Resolve() const { return Apply(operand.value()); }
  static bool Apply(bool operand) { return not operand; }

  RegOr<bool> operand;
  Reg result;
};

// TODO Morph this into interpreter break-point instructions.
struct DebugIrInstruction
    : base::Extend<DebugIrInstruction>::With<ByteCodeExtension, InlineExtension,
                                             DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "debug-ir";

  void Apply(interpreter::ExecutionContext& ctx) const {
    std::cerr << *ctx.current_frame().fn().native();
  }
};

struct AbortInstruction
    : base::Extend<AbortInstruction>::With<ByteCodeExtension, InlineExtension,
                                           DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "abort";

  void Apply(interpreter::ExecutionContext&) const { std::abort(); }
};


struct InitInstruction
    : base::Extend<InitInstruction>::With<ByteCodeExtension, InlineExtension,
                                          DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "init %2$s";

  interpreter::StackFrame Apply(interpreter::ExecutionContext& ctx) const {
    if (auto* s = type.if_as<type::Struct>()) {
      ir::Fn f   = *s->init_;
      interpreter::StackFrame frame(f.native(), ctx.stack());
      frame.regs_.set(ir::Reg::Arg(0), ctx.resolve<ir::Addr>(reg));
      return frame;

    } else if (auto* a = type.if_as<type::Array>()) {
      ir::Fn f   = a->Initializer();
      interpreter::StackFrame frame(f.native(), ctx.stack());
      frame.regs_.set(ir::Reg::Arg(0), ctx.resolve<ir::Addr>(reg));
      return frame;

    } else {
      NOT_YET();
    }
  }

  type::Type type;
  Reg reg;
};

struct DestroyInstruction
    : base::Extend<DestroyInstruction>::With<ByteCodeExtension, InlineExtension,
                                             DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "destroy %2$s";

  interpreter::StackFrame Apply(interpreter::ExecutionContext& ctx) const {
    interpreter::StackFrame frame(function(), ctx.stack());
    frame.regs_.set(ir::Reg::Arg(0), ctx.resolve<ir::Addr>(reg));
    return frame;
  }

  type::Type type;
  Reg reg;

 private:
  ir::Fn function() const {
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
    : base::Extend<CopyInstruction>::With<ByteCodeExtension, InlineExtension,
                                          DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "copy %2$s to %3$s";

  interpreter::StackFrame Apply(interpreter::ExecutionContext& ctx) const {
    interpreter::StackFrame frame(function(), ctx.stack());
    frame.regs_.set(ir::Reg::Arg(0), ctx.resolve<ir::Addr>(to));
    frame.regs_.set(ir::Reg::Arg(1), ctx.resolve<ir::Addr>(from));
    return frame;
  }

  type::Type type;
  ir::RegOr<ir::Addr> from;
  ir::RegOr<ir::Addr> to;

 private:
  ir::Fn function() const {
    if (auto* s = type.if_as<type::Struct>()) {
      return *ASSERT_NOT_NULL(s->CopyAssignment(s));
    } else if (auto* a = type.if_as<type::Array>()) {
      return a->CopyAssign();
    } else {
      NOT_YET();
    }
  }
};

struct CopyInitInstruction
    : base::Extend<CopyInitInstruction>::With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "copy-init %2$s to %3$s";

  interpreter::StackFrame Apply(interpreter::ExecutionContext& ctx) const {
    interpreter::StackFrame frame(function(), ctx.stack());
    frame.regs_.set(ir::Reg::Arg(0), ctx.resolve<ir::Addr>(from));
    frame.regs_.set(ir::Reg::Out(0), ctx.resolve<ir::Addr>(to));
    return frame;
  }

  type::Type type;
  ir::RegOr<ir::Addr> from;
  ir::RegOr<ir::Addr> to;

 private:
  ir::Fn function() const {
    if (auto* s = type.if_as<type::Struct>()) {
      return *ASSERT_NOT_NULL(s->CopyInit(s));
    } else if (auto* a = type.if_as<type::Array>()) {
      return a->CopyInit();
    } else {
      NOT_YET();
    }
  }
};

struct MoveInstruction
    : base::Extend<MoveInstruction>::With<ByteCodeExtension, InlineExtension,
                                          DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "move %2$s to %3$s";

  interpreter::StackFrame Apply(interpreter::ExecutionContext& ctx) const {
    interpreter::StackFrame frame(function(), ctx.stack());
    frame.regs_.set(ir::Reg::Arg(0), ctx.resolve<ir::Addr>(to));
    frame.regs_.set(ir::Reg::Arg(1), ctx.resolve<ir::Addr>(from));
    return frame;
  }

  type::Type type;
  ir::RegOr<ir::Addr> from;
  ir::RegOr<ir::Addr> to;

 private:
  ir::Fn function() const {
    if (auto* s = type.if_as<type::Struct>()) {
      return *ASSERT_NOT_NULL(s->MoveAssignment(s));
    } else if (auto* a = type.if_as<type::Array>()) {
      return a->MoveAssign();
    } else {
      NOT_YET();
    }
  }
};

struct MoveInitInstruction
    : base::Extend<MoveInitInstruction>::With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "move-init %2$s to %3$s";

  interpreter::StackFrame Apply(interpreter::ExecutionContext& ctx) const {
    interpreter::StackFrame frame(function(), ctx.stack());
    frame.regs_.set(ir::Reg::Arg(0), ctx.resolve<ir::Addr>(from));
    frame.regs_.set(ir::Reg::Out(0), ctx.resolve<ir::Addr>(to));
    return frame;
  }

  type::Type type;
  ir::RegOr<ir::Addr> from;
  ir::RegOr<ir::Addr> to;

 private:
  ir::Fn function() const {
    if (auto* s = type.if_as<type::Struct>()) {
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

struct LoadSymbolInstruction
    : base::Extend<LoadSymbolInstruction>::With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat =
      "%3$s = load-symbol %1$s: %2$s";

  void Apply(interpreter::ExecutionContext& ctx) const {
    // TODO: We could probably extract this into two separate instructions (one
    // for functions and one for pointers) so that we can surface errors during
    // code-gen without the UNREACHABLE.
    if (auto* fn_type = type.if_as<type::Function>()) {
      auto sym = interpreter::LoadFunctionSymbol(name);
      if (not sym.ok()) { FatalInterpreterError(sym.status().message()); }
      ctx.current_frame().regs_.set(result,
                                    ir::Fn(ir::ForeignFn(*sym, fn_type)));
    } else if (type.is<type::Pointer>()) {
      absl::StatusOr<void*> sym = interpreter::LoadDataSymbol(name);
      if (not sym.ok()) { FatalInterpreterError(sym.status().message()); }
      ctx.current_frame().regs_.set(result, ir::Addr::Heap(*sym));
    } else {
      UNREACHABLE(type.to_string());
    }
  }

  std::string name;
  type::Type type;
  Reg result;
};

struct TypeInfoInstruction
    : base::Extend<TypeInfoInstruction>::With<ByteCodeExtension,
                                              InlineExtension> {
  enum class Kind : uint8_t { Alignment = 0, Bytes = 2 };

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(
        stringify(result),
        kind == Kind::Alignment ? " = alignment " : " = bytes ",
        type.is_reg() ? stringify(type.reg()) : type.value().to_string());
  }

  uint64_t Resolve() const {
    switch (kind) {
      case Kind::Alignment:
        return type.value().alignment(interpreter::kArchitecture).value();
      case Kind::Bytes:
        return type.value().bytes(interpreter::kArchitecture).value();
    }
  }

  Kind kind;
  RegOr<type::Type> type;
  Reg result;
};

struct MakeBlockInstruction
    : base::Extend<MakeBlockInstruction>::With<ByteCodeExtension,
                                               InlineExtension> {
  std::string to_string() const { return "make-block"; }  // TODO

  Block Resolve() const {
    std::vector<ir::Fn> resolved_befores;
    resolved_befores.reserve(befores.size());
    for (auto const& fn : befores) { resolved_befores.push_back(fn.value()); }

    absl::flat_hash_set<ir::Jump> resolved_afters;
    resolved_afters.reserve(afters.size());
    for (auto const& jmp : afters) { resolved_afters.insert(jmp.value()); }

    *CompiledBlock::From(block) = CompiledBlock(
        OverloadSet(std::move(resolved_befores)), std::move(resolved_afters));

    return block;
  }

  Block block;
  std::vector<RegOr<Fn>> befores;
  std::vector<RegOr<Jump>> afters;
  Reg result;
};

struct MakeScopeInstruction
    : base::Extend<MakeScopeInstruction>::With<ByteCodeExtension,
                                               InlineExtension> {
  std::string to_string() const { return "make-scope"; }  // TODO

  Scope Resolve() const {
    absl::flat_hash_set<ir::Jump> resolved_inits;
    resolved_inits.reserve(inits.size());
    for (auto const& init : inits) { resolved_inits.insert(init.value()); }

    std::vector<ir::Fn> resolved_dones;
    resolved_dones.reserve(dones.size());
    for (auto const& fn : dones) { resolved_dones.push_back(fn.value()); }

    CompiledScope::From(scope)->Initialize(
        std::move(resolved_inits), OverloadSet(std::move(resolved_dones)),
        blocks);
    return scope;
  }

  Scope scope;
  std::vector<RegOr<Jump>> inits;
  std::vector<RegOr<Fn>> dones;
  absl::flat_hash_map<std::string_view, Block> blocks;
  Reg result;
};

struct StructIndexInstruction
    : base::Extend<StructIndexInstruction>::With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat =
      "%4$s = index %2$s of %1$s (struct %3$s)";

  Addr Resolve() const {
    return addr.value() +
           struct_type->offset(index.value(), interpreter::kArchitecture);
  }

  RegOr<Addr> addr;
  RegOr<int64_t> index;
  ::type::Struct const* struct_type;
  Reg result;
};

struct PtrIncrInstruction
    : base::Extend<PtrIncrInstruction>::With<ByteCodeExtension, InlineExtension,
                                             DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat =
      "%4$s = index %2$s of %1$s (pointer %3$s)";

  Addr Resolve() const {
    return addr.value() +
           core::FwdAlign(
               ptr->pointee().bytes(interpreter::kArchitecture),
               ptr->pointee().alignment(interpreter::kArchitecture)) *
               index.value();
  }

  RegOr<Addr> addr;
  RegOr<int64_t> index;
  ::type::Pointer const* ptr;
  Reg result;
};

struct AndInstruction
    : base::Extend<AndInstruction>::With<ByteCodeExtension, InlineExtension,
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
