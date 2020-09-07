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
#include "ir/instruction/phi.h"
#include "ir/instruction/type.h"
#include "ir/instruction/util.h"
#include "ir/interpretter/execute.h"
#include "ir/interpretter/foreign.h"
#include "ir/out_params.h"
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

struct LoadInstruction
    : base::Extend<LoadInstruction>::With<
          WriteByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr cmd_index_t kIndex = internal::kLoadInstructionNumber;
  static constexpr std::string_view kDebugFormat = "%3$s = load %2$s";

  uint16_t num_bytes;
  RegOr<Addr> addr;
  Reg result;
};

template <typename T>
struct StoreInstruction
    : base::Extend<StoreInstruction<T>>::template With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "store %1$s -> [%2$s]";
  using type = T;

  void Apply(interpretter::ExecutionContext& ctx) {
    ir::Addr addr = ctx.resolve(location);
    type val      = ctx.resolve(value);
    switch (addr.kind()) {
      case ir::Addr::Kind::Stack: ctx.stack_.set(addr.stack(), val); break;
      case ir::Addr::Kind::ReadOnly:
        NOT_YET(
            "Storing into read-only data seems suspect. Is it just for "
            "initialization?");
        break;
      case ir::Addr::Kind::Heap:
        *ASSERT_NOT_NULL(static_cast<type*>(addr.heap())) = val;
    }
  }

  RegOr<T> value;
  RegOr<Addr> location;
};

// This instruction is a bit strange sets a register to either another registor,
// or an immediate value. By the very nature of Single-Static-Assignment, every
// use of this instruction is an optimization opportunity. If a register is
// initialized with an immediate value, we can do constant propagation. If it is
// initialized with another register, the two registers can be folded into a
// single register.
//
// The benefit of such an instruction is that it enables us to inline code
// without worrying about rewriting register names immediately. This instruction
// should never be visible in the final code.
template <typename T>
struct RegisterInstruction
    : base::Extend<RegisterInstruction<T>>::template With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = %1$s";

  void Apply(interpretter::ExecutionContext& ctx) const {
    ctx.current_frame()->regs_.set(result, Apply(ctx.resolve(operand)));
  }
  static T Apply(T val) { return val; }

  RegOr<T> operand;
  Reg result;
};

template <typename T>
struct SetReturnInstruction
    : base::Extend<SetReturnInstruction<T>>::template With<
          WriteByteCodeExtension, InlineExtension, DebugFormatExtension> {
  using type = T;
  static constexpr cmd_index_t kIndex =
      internal::kSetReturnInstructionRange.start +
      internal::PrimitiveIndex<T>();
  static constexpr std::string_view kDebugFormat = "set-ret %1$s = %2$s";

  uint16_t index;
  RegOr<T> value;
};

template <typename FromType, typename ToType>
struct CastInstruction
    : base::Extend<CastInstruction<FromType, ToType>>::template With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  using from_type                                = FromType;
  using to_type                                  = ToType;
  static constexpr std::string_view kDebugFormat = "%2$s = cast %1$s";

  void Apply(interpretter::ExecutionContext& ctx) const {
    ctx.current_frame()->regs_.set(result,
                                   static_cast<ToType>(ctx.resolve(value)));
  }

  RegOr<FromType> value;
  Reg result;
};

struct GetReturnInstruction
    : base::Extend<GetReturnInstruction>::With<
          WriteByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr cmd_index_t kIndex = internal::kGetReturnInstructionIndex;
  static constexpr std::string_view kDebugFormat = "%2$s = get-ret %1$s";

  uint16_t index;
  Reg result;
};

struct NotInstruction
    : base::Extend<NotInstruction>::With<ByteCodeExtension, InlineExtension,
                                         DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = not %1$s";


  void Apply(interpretter::ExecutionContext& ctx) const {
    ctx.current_frame()->regs_.set(result, not ctx.resolve(operand));
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
    std::cerr << *ctx.current_frame()->fn_.get();
  }
};

// Oddly named to be sure, this instruction is used to do initializations,
// copies, moves, or destructions of the given type.
struct TypeManipulationInstruction {
  constexpr static cmd_index_t kIndex =
      internal::kTypeManipulationInstructionNumber;

  enum class Kind : uint8_t { Init, Destroy, Move, Copy };
  TypeManipulationInstruction(Kind k, type::Type const* type, Reg from,
                              RegOr<Addr> to = RegOr<Addr>(Reg(0)))
      : kind(k), type(type), r(from), to(to) {}
  ~TypeManipulationInstruction() {}

  std::string to_string() const {
    char const* name;
    switch (kind) {
      case Kind::Init:
        return absl::StrCat("init ", type->to_string(), " ", stringify(r));
      case Kind::Destroy:
        return absl::StrCat("destroy ", type->to_string(), " ", stringify(r));
      case Kind::Copy:
        return absl::StrCat("copy ", type->to_string(), " ", stringify(r), " ",
                            stringify(to));
      case Kind::Move:
        return absl::StrCat("move ", type->to_string(), " ", stringify(r), " ",
                            stringify(to));
      default: UNREACHABLE();
    }
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write(kind);
    writer->Write(type);
    writer->Write(r);
    if (kind == Kind::Copy or kind == Kind::Move) {
      writer->Write(to.is_reg());
      to.apply([&](auto v) { writer->Write(v); });
    }
  }

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(r);
    if (kind == Kind::Copy or kind == Kind::Move) { inliner.Inline(to); }
  }

  Kind kind;
  type::Type const* type;
  Reg r;
  RegOr<Addr> to;  // Only meaningful for copy and move
};

struct CallInstruction {
  static constexpr cmd_index_t kIndex = internal::kCallInstructionNumber;

  CallInstruction(type::Function const* fn_type, RegOr<Fn> const& fn,
                  std::vector<Value> args, OutParams outs)
      : fn_type_(fn_type),
        fn_(fn),
        args_(std::move(args)),
        outs_(std::move(outs)) {
    ASSERT(this->outs_.size() == fn_type_->output().size());
    ASSERT(args_.size() == fn_type_->params().size());
  }

  ~CallInstruction() {}

  std::string to_string() const {
    using base::stringify;
    std::string result = absl::StrCat("call ", stringify(fn_));
    for (auto const& arg : args_) {
      absl::StrAppend(&result, "\n      -> ", stringify(arg));
    }
    for (size_t i = 0; i < fn_type_->output().size(); ++i) {
      absl::StrAppend(&result, "\n      <- ", stringify(outs_[i]));
    }

    return result;
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write(fn_.is_reg());
    fn_.apply([&](auto v) { writer->Write(v); });
    size_t bytes_written_slot = writer->buf_->reserve<core::Bytes>();

    size_t arg_index = 0;
    for (Value const& arg : args_) {
      Reg const* r = arg.get_if<Reg>();
      writer->Write(static_cast<bool>(r));
      if (r) {
        writer->Write(*r);
      } else {
        type::Apply(fn_type_->params()[arg_index].value.type(), [&](auto tag) {
          using T = typename decltype(tag)::type;
          writer->Write(arg.get<T>());
        });
      }
      ++arg_index;
    }

    outs_.WriteByteCode(writer);

    writer->buf_->set(bytes_written_slot,
                      core::Bytes{writer->buf_->size() - bytes_written_slot -
                                  sizeof(core::Bytes)});
  }

  RegOr<Fn> func() const { return fn_; }

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(fn_);
    for (auto& arg : args_) { inliner.Inline(arg); }
    for (auto& reg : outs_.regs()) { inliner.Inline(reg); }
  }

 private:
  type::Function const* fn_type_;
  RegOr<Fn> fn_;
  std::vector<Value> args_;
  OutParams outs_;
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
      ctx.current_frame()->regs_.set(result, ir::Fn(ir::ForeignFn(sym, fn_type)));
    } else if (type->is<type::Pointer>()) {
      ASSIGN_OR(FatalInterpretterError(_.error().to_string()),  //
                void* sym, interpretter::LoadDataSymbol(name.get()));
      ctx.current_frame()->regs_.set(result, ir::Addr::Heap(sym));
    } else {
      UNREACHABLE(type->to_string());
    }
  }

  String name;
  type::Type const* type;
  Reg result;
};

struct TypeInfoInstruction {
  static constexpr cmd_index_t kIndex = internal::kTypeInfoInstructionNumber;
  enum class Kind : uint8_t { Alignment = 0, Bytes = 2 };
  TypeInfoInstruction(Kind kind, RegOr<type::Type const*> type)
      : kind(kind), type(type) {}
  ~TypeInfoInstruction() {}

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(
        stringify(result),
        kind == Kind::Alignment ? " = alignment " : " = bytes ",
        type.is_reg() ? stringify(type.reg()) : type.value()->to_string());
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write<uint8_t>(static_cast<uint8_t>(kind) |
                           static_cast<uint8_t>(type.is_reg()));
    type.apply([&](auto v) { writer->Write(v); });
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) { inliner.Inline(type); }

  Kind kind;
  RegOr<type::Type const*> type;
  Reg result;
};

struct MakeBlockInstruction {
  static constexpr cmd_index_t kIndex = internal::kMakeBlockInstructionNumber;

  MakeBlockInstruction(BlockDef* block_def, std::vector<RegOr<Fn>> befores,
                       std::vector<RegOr<Jump*>> afters)
      : block_def(block_def),
        befores(std::move(befores)),
        afters(std::move(afters)) {}
  ~MakeBlockInstruction() {}

  // TODO
  std::string to_string() const { return "make-block "; }

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write(block_def);
    internal::WriteBits<uint16_t, RegOr<Fn>>(
        writer, befores, [](RegOr<Fn> const& r) { return r.is_reg(); });
    absl::c_for_each(befores, [&](RegOr<Fn> x) {
      x.apply([&](auto v) { writer->Write(v); });
    });
    internal::WriteBits<uint16_t, RegOr<Jump*>>(
        writer, afters, [](RegOr<Jump*> const& r) { return r.is_reg(); });
    absl::c_for_each(afters, [&](RegOr<Jump*> x) {
      x.apply([&](auto v) { writer->Write(v); });
    });
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(befores);
    inliner.Inline(afters);
    inliner.Inline(result);
  }

  BlockDef* block_def;
  std::vector<RegOr<Fn>> befores;
  std::vector<RegOr<Jump*>> afters;
  Reg result;
};

struct MakeScopeInstruction {
  static constexpr cmd_index_t kIndex = internal::kMakeScopeInstructionNumber;

  MakeScopeInstruction(ScopeDef* scope_def, std::vector<RegOr<Jump*>> inits,
                       std::vector<RegOr<Fn>> dones,
                       absl::flat_hash_map<std::string_view, BlockDef*> blocks)
      : scope_def(scope_def),
        inits(std::move(inits)),
        dones(std::move(dones)),
        blocks(std::move(blocks)) {}
  ~MakeScopeInstruction() {}

  // TODO
  std::string to_string() const { return "make-scope"; }

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write(scope_def);

    internal::WriteBits<uint16_t, RegOr<Jump*>>(
        writer, inits, [](RegOr<Jump*> const& r) { return r.is_reg(); });
    absl::c_for_each(inits, [&](RegOr<Jump*> x) {
      x.apply([&](auto v) { writer->Write(v); });
    });
    internal::WriteBits<uint16_t, RegOr<Fn>>(
        writer, dones, [](RegOr<Fn> const& r) { return r.is_reg(); });
    absl::c_for_each(dones, [&](RegOr<Fn> x) {
      x.apply([&](auto v) { writer->Write(v); });
    });

    writer->Write<uint16_t>(blocks.size());
    for (auto [name, block] : blocks) {
      writer->Write(name);
      writer->Write(block);
    }
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(inits);
    inliner.Inline(dones);
    inliner.Inline(result);
  }

  ScopeDef* scope_def;
  std::vector<RegOr<Jump*>> inits;
  std::vector<RegOr<Fn>> dones;
  absl::flat_hash_map<std::string_view, BlockDef*> blocks;
  Reg result;
};

struct StructIndexInstruction
    : base::Extend<StructIndexInstruction>::With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat =
      "%4$s = index %2$s of %1$s (struct %3$s)";

  void Apply(interpretter::ExecutionContext& ctx) {
    ctx.current_frame()->regs_.set(
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

  void Apply(interpretter::ExecutionContext& ctx) {
    ctx.current_frame()->regs_.set(
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

  void Apply(interpretter::ExecutionContext& ctx) {
    ctx.current_frame()->regs_.set(
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
