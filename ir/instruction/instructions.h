#ifndef ICARUS_IR_INSTRUCTION_INSTRUCTIONS_H
#define ICARUS_IR_INSTRUCTION_INSTRUCTIONS_H

#include <memory>

#include "absl/algorithm/container.h"
#include "absl/container/flat_hash_map.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "ast/scope/scope.h"
#include "base/extend.h"
#include "base/meta.h"
#include "ir/block_def.h"
#include "ir/instruction/arithmetic.h"
#include "ir/instruction/base.h"
#include "ir/instruction/byte_view.h"
#include "ir/instruction/compare.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/flags.h"
#include "ir/instruction/inliner.h"
#include "ir/instruction/op_codes.h"
#include "ir/instruction/type.h"
#include "ir/interpretter/architecture.h"
#include "ir/interpretter/execution_context.h"
#include "ir/interpretter/foreign.h"
#include "ir/interpretter/stack_frame.h"
#include "ir/out_params.h"
#include "ir/scope_def.h"
#include "ir/value/enum_and_flags.h"
#include "ir/value/fn.h"
#include "ir/value/generic_fn.h"
#include "ir/value/reg_or.h"
#include "ir/value/string.h"
#include "type/generic_function.h"
#include "type/util.h"

// This file defines the interface required for IR instructions as well as all
// the common instructions available in the core IR.
namespace ir {

template <typename FromType, typename ToType>
struct CastInstruction
    : base::Extend<CastInstruction<FromType, ToType>>::template With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  using from_type                                = FromType;
  using to_type                                  = ToType;
  static constexpr std::string_view kDebugFormat = "%2$s = cast %1$s";

  void Apply(interpretter::ExecutionContext& ctx) const {
    ctx.current_frame().regs_.set(result,
                                  static_cast<ToType>(ctx.resolve(value)));
  }

  RegOr<FromType> value;
  Reg result;
};

struct NotInstruction
    : base::Extend<NotInstruction>::With<ByteCodeExtension, InlineExtension,
                                         DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = not %1$s";

  void Apply(interpretter::ExecutionContext& ctx) const {
    ctx.current_frame().regs_.set(result, not ctx.resolve(operand));
  }
  static bool Apply(bool operand) { return not operand; }

  RegOr<bool> operand;
  Reg result;
};

// TODO Morph this into interpretter break-point instructions.
struct DebugIrInstruction
    : base::Extend<DebugIrInstruction>::With<ByteCodeExtension, InlineExtension,
                                             DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "debug-ir";

  void Apply(interpretter::ExecutionContext& ctx) const {
    std::cerr << *ctx.current_frame().fn_.get();
  }
};

struct InitInstruction
    : base::Extend<InitInstruction>::With<ByteCodeExtension, InlineExtension,
                                          DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "init %2$s";

  interpretter::StackFrame Apply(interpretter::ExecutionContext& ctx) const {
    if (auto* s = type->if_as<type::Struct>()) {
      ir::Fn f = s->init_func_.get();
      auto frame = ctx.MakeStackFrame(f.native());
      frame.regs_.set(ir::Reg::Arg(0), ctx.resolve<ir::Addr>(reg));
      return frame;

    } else if (auto* tup = type->if_as<type::Tuple>()) {
      ir::Fn f = tup->init_func_.get();
      auto frame = ctx.MakeStackFrame(f.native());
      frame.regs_.set(ir::Reg::Arg(0), ctx.resolve<ir::Addr>(reg));
      return frame;

    } else if (auto* a = type->if_as<type::Array>()) {
      NOT_YET();  // f = a->init_func_.get();
    } else {
      NOT_YET();
    }
  }

  type::Type const* type;
  Reg reg;
};

struct DestroyInstruction
    : base::Extend<DestroyInstruction>::With<ByteCodeExtension, InlineExtension,
                                             DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "destroy %2$s";

  interpretter::StackFrame Apply(interpretter::ExecutionContext& ctx) const {
    if (auto* s = type->if_as<type::Struct>()) {
      ir::Fn f = s->destroy_func_.get();
      auto frame = ctx.MakeStackFrame(f.native());
      frame.regs_.set(ir::Reg::Arg(0), ctx.resolve<ir::Addr>(reg));
      return frame;

    } else if (auto* tup = type->if_as<type::Tuple>()) {
      ir::Fn f = tup->destroy_func_.get();
      auto frame = ctx.MakeStackFrame(f.native());
      frame.regs_.set(ir::Reg::Arg(0), ctx.resolve<ir::Addr>(reg));
      return frame;

    } else if (auto* a = type->if_as<type::Array>()) {
      NOT_YET();  // f = a->destroy_func_.get();
    } else {
      NOT_YET();
    }
  }

  type::Type const* type;
  Reg reg;
};

struct CopyInstruction
    : base::Extend<CopyInstruction>::With<ByteCodeExtension, InlineExtension,
                                          DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "copy %2$s -> %3$s";

  interpretter::StackFrame Apply(interpretter::ExecutionContext& ctx) const {
    NOT_YET();
  }

  type::Type const* type;
  ir::RegOr<ir::Addr> from;
  ir::RegOr<ir::Addr> to;
};

struct MoveInstruction
    : base::Extend<MoveInstruction>::With<ByteCodeExtension, InlineExtension,
                                          DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "move %2$s -> %3$s";

  interpretter::StackFrame Apply(interpretter::ExecutionContext& ctx) const {
    NOT_YET();
  }

  type::Type const* type;
  ir::RegOr<ir::Addr> from;
  ir::RegOr<ir::Addr> to;
};

[[noreturn]] inline void FatalInterpretterError(std::string_view err_msg) {
  // TODO: Add a diagnostic explaining the failure.
  absl::FPrintF(stderr,
                R"(
  ----------------------------------------
  Fatal interpretter failure:
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

  void Apply(interpretter::ExecutionContext& ctx) const {
    // TODO: We could probably extract this into two separate instructions (one
    // for functions and one for pointers) so that we can surface errors during
    // code-gen without the UNREACHABLE.
    if (auto* fn_type = type->if_as<type::Function>()) {
      ASSIGN_OR(FatalInterpretterError(_.error().to_string()),  //
                void (*sym)(), interpretter::LoadFunctionSymbol(name.get()));
      ctx.current_frame().regs_.set(result,
                                    ir::Fn(ir::ForeignFn(sym, fn_type)));
    } else if (type->is<type::Pointer>()) {
      ASSIGN_OR(FatalInterpretterError(_.error().to_string()),  //
                void* sym, interpretter::LoadDataSymbol(name.get()));
      ctx.current_frame().regs_.set(result, ir::Addr::Heap(sym));
    } else {
      UNREACHABLE(type->to_string());
    }
  }

  String name;
  type::Type const* type;
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
        type.is_reg() ? stringify(type.reg()) : type.value()->to_string());
  }

  void Apply(interpretter::ExecutionContext& ctx) const {
    switch (kind) {
      case Kind::Alignment:
        ctx.current_frame().regs_.set(
            result, ctx.resolve(type)->alignment(interpretter::kArchitecture));
        break;
      case Kind::Bytes:
        ctx.current_frame().regs_.set(
            result, ctx.resolve(type)->bytes(interpretter::kArchitecture));
        break;
    }
  }

  Kind kind;
  RegOr<type::Type const*> type;
  Reg result;
};

struct MakeBlockInstruction
    : base::Extend<MakeBlockInstruction>::With<ByteCodeExtension,
                                               InlineExtension> {
  std::string to_string() const { return "make-scope"; }  // TODO

  void Apply(interpretter::ExecutionContext& ctx) const {
    std::vector<ir::Fn> resolved_befores;
    resolved_befores.reserve(befores.size());
    for (auto const& fn : befores) {
      resolved_befores.push_back(ctx.resolve(fn));
    }

    absl::flat_hash_set<ir::Jump> resolved_afters;
    resolved_afters.reserve(afters.size());
    for (auto const& jmp : afters) { resolved_afters.insert(ctx.resolve(jmp)); }

    *block_def         = ir::BlockDef(std::move(resolved_afters));
    block_def->before_ = ir::OverloadSet(std::move(resolved_befores));

    ctx.current_frame().regs_.set(result, block_def);
  }

  BlockDef* block_def;
  std::vector<RegOr<Fn>> befores;
  std::vector<RegOr<Jump>> afters;
  Reg result;
};

struct MakeScopeInstruction
    : base::Extend<MakeScopeInstruction>::With<ByteCodeExtension,
                                               InlineExtension> {
  std::string to_string() const { return "make-scope"; }  // TODO

  void Apply(interpretter::ExecutionContext& ctx) const {
    absl::flat_hash_set<ir::Jump> resolved_inits;
    resolved_inits.reserve(inits.size());
    for (auto const& init : inits) { resolved_inits.insert(ctx.resolve(init)); }
    *scope_def->start_ = ir::BlockDef(std::move(resolved_inits));

    std::vector<ir::Fn> resolved_dones;
    resolved_dones.reserve(dones.size());
    for (auto const& fn : dones) { resolved_dones.push_back(ctx.resolve(fn)); }
    scope_def->exit_->before_ = ir::OverloadSet(std::move(resolved_dones));

    for (auto [name, block] : blocks) {
      scope_def->blocks_.emplace(name, block);
    }

    ctx.current_frame().regs_.set(result, scope_def);
  }

  ScopeDef* scope_def;
  std::vector<RegOr<Jump>> inits;
  std::vector<RegOr<Fn>> dones;
  absl::flat_hash_map<std::string_view, BlockDef*> blocks;
  Reg result;
};

struct StructIndexInstruction
    : base::Extend<StructIndexInstruction>::With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat =
      "%4$s = index %2$s of %1$s (struct %3$s)";

  void Apply(interpretter::ExecutionContext& ctx) const {
    ctx.current_frame().regs_.set(
        result,
        ctx.resolve(addr) + struct_type->offset(ctx.resolve(index),
                                                interpretter::kArchitecture));
  }

  RegOr<Addr> addr;
  RegOr<int64_t> index;
  ::type::Struct const* struct_type;
  Reg result;
};

struct TupleIndexInstruction
    : base::Extend<TupleIndexInstruction>::With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat =
      "%4$s = index %2$s of %1$s (tuple %3$s)";

  void Apply(interpretter::ExecutionContext& ctx) const {
    ctx.current_frame().regs_.set(
        result, ctx.resolve(addr) + tuple->offset(ctx.resolve(index),
                                                  interpretter::kArchitecture));
  }

  RegOr<Addr> addr;
  RegOr<int64_t> index;
  ::type::Tuple const* tuple;
  Reg result;
};

struct PtrIncrInstruction
    : base::Extend<PtrIncrInstruction>::With<ByteCodeExtension, InlineExtension,
                                             DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat =
      "%4$s = index %2$s of %1$s (pointer %3$s)";

  void Apply(interpretter::ExecutionContext& ctx) const {
    ctx.current_frame().regs_.set(
        result,
        ctx.resolve(addr) +
            core::FwdAlign(
                ptr->pointee()->bytes(interpretter::kArchitecture),
                ptr->pointee()->alignment(interpretter::kArchitecture)) *
                ctx.resolve(index));
  }

  RegOr<Addr> addr;
  RegOr<int64_t> index;
  ::type::Pointer const* ptr;
  Reg result;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_INSTRUCTIONS_H
