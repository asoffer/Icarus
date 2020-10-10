#ifndef ICARUS_IR_INSTRUCTION_CORE_H
#define ICARUS_IR_INSTRUCTION_CORE_H

#include <string>
#include <utility>
#include <vector>

#include "absl/strings/str_cat.h"
#include "base/extend.h"
#include "ir/blocks/basic.h"
#include "ir/byte_code_writer.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/inliner.h"
#include "ir/instruction/op_codes.h"
#include "ir/interpretter/execution_context.h"
#include "ir/out_params.h"
#include "ir/value/generic_fn.h"
#include "ir/value/jump.h"
#include "ir/value/reg_or.h"
#include "type/util.h"

namespace ir {
// These instructions are required to appear in every instruction set. They
// consist of any instruction having to do with control-flow or reading/writing
// memory.

struct GetReturnInstruction
    : base::Extend<GetReturnInstruction>::With<
          WriteByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr cmd_index_t kIndex = internal::kGetReturnInstruction;
  static constexpr std::string_view kDebugFormat = "%2$s = get-ret %1$s";

  uint16_t index;
  Reg result;
};

struct LoadInstruction
    : base::Extend<LoadInstruction>::With<
          WriteByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr cmd_index_t kIndex = internal::kLoadInstructionNumber;
  static constexpr std::string_view kDebugFormat = "%3$s = load %2$s";

  uint16_t num_bytes;
  RegOr<Addr> addr;
  Reg result;
};

namespace internal_core {
template <typename SizeType, typename T, typename Fn>
void WriteBits(ByteCodeWriter* writer, absl::Span<T const> span,
               Fn&& predicate) {
  ASSERT(span.size() < std::numeric_limits<SizeType>::max());
  writer->Write<SizeType>(span.size());

  uint8_t reg_mask = 0;
  for (size_t i = 0; i < span.size(); ++i) {
    if (predicate(span[i])) { reg_mask |= (1 << (7 - (i % 8))); }
    if (i % 8 == 7) {
      writer->Write(reg_mask);
      reg_mask = 0;
    }
  }
  if (span.size() % 8 != 0) { writer->Write(reg_mask); }
}
}  // namespace internal_core

// TODO consider changing these to something like 'basic block arguments'
template <typename T>
struct PhiInstruction {
  using type = T;

  PhiInstruction() = default;
  PhiInstruction(std::vector<BasicBlock const*> blocks,
                 std::vector<RegOr<T>> values)
      : blocks(std::move(blocks)), values(std::move(values)) {}

  void add(BasicBlock const* block, RegOr<T> value) {
    blocks.push_back(block);
    values.push_back(value);
  }

  std::string to_string() const {
    using base::stringify;
    std::string s = absl::StrCat(stringify(result), " = phi ");
    for (size_t i = 0; i < blocks.size(); ++i) {
      absl::StrAppend(&s, "\n      ", stringify(blocks[i]), ": ",
                      stringify(values[i]));
    }
    return s;
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write<uint16_t>(values.size());
    for (auto block : blocks) { writer->Write(block); }
    internal_core::WriteBits<uint16_t, RegOr<T>>(
        writer, values, [](RegOr<T> const& r) { return r.is_reg(); });

    absl::c_for_each(values, [&](RegOr<T> const& x) {
      x.apply([&](auto v) { writer->Write(v); });
    });

    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(values);
    inliner.Inline(result);
  }

  std::vector<BasicBlock const*> blocks;
  std::vector<RegOr<T>> values;
  Reg result;
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
    ctx.current_frame().regs_.set(result, Apply(ctx.resolve(operand)));
  }
  static T Apply(T val) { return val; }

  RegOr<T> operand;
  Reg result;
};

template <typename T>
struct SetReturnInstruction
    : base::Extend<SetReturnInstruction<T>>::template With<
          WriteByteCodeExtension, InlineExtension, DebugFormatExtension> {
  using type                                     = T;
  static constexpr std::string_view kDebugFormat = "set-ret %1$s = %2$s";

  uint16_t index;
  RegOr<T> value;
};

template <typename T>
struct StoreInstruction
    : base::Extend<StoreInstruction<T>>::template With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "store %1$s into [%2$s]";
  using type                                     = T;

  void Apply(interpretter::ExecutionContext& ctx) {
    ctx.Store(ctx.resolve(location), ctx.resolve(value));
  }

  RegOr<T> value;
  RegOr<Addr> location;
};

struct CallInstruction {
  CallInstruction(type::Function const* fn_type, RegOr<Fn> const& fn,
                  std::vector<Value> args, OutParams outs)
      : fn_type_(fn_type),
        fn_(fn),
        args_(std::move(args)),
        outs_(std::move(outs)) {
    ASSERT(this->outs_.size() == fn_type_->output().size());
    ASSERT(args_.size() == fn_type_->params().size());
  }

  std::string to_string() const {
    using base::stringify;
    std::string result = absl::StrCat("call ", stringify(fn_));
    for (auto const& arg : args_) {
      absl::StrAppend(&result, "\n      I ", stringify(arg));
    }
    for (size_t i = 0; i < fn_type_->output().size(); ++i) {
      absl::StrAppend(&result, "\n      O ", stringify(outs_[i]));
    }

    return result;
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write(fn_);
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

struct CommentInstruction
    : base::Extend<CommentInstruction>::With<InlineExtension,
                                             DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "comment: %1$s";

  static CommentInstruction ReadFromByteCode(
      base::untyped_buffer::const_iterator*) {
    return {};
  }
  void WriteByteCode(ByteCodeWriter*) const {}
  void Apply(interpretter::ExecutionContext& ctx) const {}

  std::string comment;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_CORE_H
