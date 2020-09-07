#ifndef ICARUS_IR_INSTRUCTION_TYPE_H
#define ICARUS_IR_INSTRUCTION_TYPE_H

#include <string_view>

#include "base/extend.h"
#include "ir/byte_code_writer.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/inliner.h"
#include "ir/instruction/op_codes.h"
#include "ir/instruction/util.h"
#include "ir/struct_field.h"
#include "type/array.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/qual_type.h"
#include "type/type.h"

namespace ir {

struct TupleInstruction
    : base::Extend<TupleInstruction>::With<ByteCodeExtension, InlineExtension> {
  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(
        "type ", stringify(this->result), " = tup ",
        absl::StrJoin(this->values, " ",
                      [](std::string* out, RegOr<type::Type const*> const& r) {
                        out->append(stringify(r));
                      }));
  }

  void Apply(interpretter::ExecutionContext& ctx) const {
    // TODO: It'd be better to deserialize and resolve simultaneously to avoid
    // this extra allocation.
    std::vector<type::Type const*> types;
    types.reserve(values.size());
    for (auto const& value : values) { types.push_back(ctx.resolve(value)); }
    ctx.current_frame()->regs_.set(result, type::Tup(std::move(types)));
  }

  std::vector<RegOr<type::Type const*>> values;
  Reg result;
};

struct EnumerationInstruction {
  enum class Kind { Enum, Flags };
  EnumerationInstruction(
      Kind k, module::BasicModule* mod, std::vector<std::string_view> names,
      absl::flat_hash_map<uint64_t, RegOr<uint64_t>> specified_values)
      : kind_(k),
        mod_(mod),
        names_(std::move(names)),
        specified_values_(std::move(specified_values)) {}
  ~EnumerationInstruction() {}

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write(kind_ == Kind::Enum);
    writer->Write<uint16_t>(names_.size());
    writer->Write<uint16_t>(specified_values_.size());
    writer->Write(mod_);
    for (auto name : names_) { writer->Write(name); }

    for (auto const& [index, val] : specified_values_) {
      // TODO these could be packed much more efficiently.
      writer->Write(index);
      writer->Write<bool>(val.is_reg());
      val.apply([&](auto v) { writer->Write(v); });
    }

    writer->Write(result);
  };

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(stringify(result),
                        kind_ == Kind::Enum ? " = enum (" : " = flags (",
                        absl::StrJoin(names_, ", "), ")");
  }

  void Inline(InstructionInliner const& inliner) { inliner.Inline(result); }

  Kind kind_;
  module::BasicModule* mod_;
  std::vector<std::string_view> names_;
  absl::flat_hash_map<uint64_t, RegOr<uint64_t>> specified_values_;
  Reg result;
};

struct OpaqueTypeInstruction
    : base::Extend<OpaqueTypeInstruction>::With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = opaque %1$s";

  void Apply(interpretter::ExecutionContext& ctx) const {
    ctx.current_frame()->regs_.set(result, type::Allocate<type::Opaque>(mod));
  }

  module::BasicModule const* mod;
  Reg result;
};

struct ArrowInstruction
    : base::Extend<ArrowInstruction>::With<ByteCodeExtension, InlineExtension> {
  void Apply(interpretter::ExecutionContext& ctx) const {
    core::Params<type::QualType> params;
    params.reserve(lhs.size());
    for (const auto& t : lhs) {
      // TODO: push qualtype into `Apply` parameters
      params.append(
          core::AnonymousParam(type::QualType::NonConstant(ctx.resolve(t))));
    }

    std::vector<type::Type const*> rhs_types;
    rhs_types.reserve(rhs.size());
    for (auto const& t : rhs) { rhs_types.push_back(ctx.resolve(t)); }

    ctx.current_frame()->regs_.set(
        result, type::Func(std::move(params), std::move(rhs_types)));
  }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(
        stringify(result), " = (",
        absl::StrJoin(lhs, ", ",
                      [](std::string* out, RegOr<type::Type const*> const& r) {
                        out->append(stringify(r));
                      }),
        ") -> (",
        absl::StrJoin(rhs, ", ",
                      [](std::string* out, RegOr<type::Type const*> const& r) {
                        out->append(stringify(r));
                      }),
        ")");
  }

  std::vector<RegOr<type::Type const*>> lhs, rhs;
  Reg result;
};

struct PtrInstruction
    : base::Extend<PtrInstruction>::With<ByteCodeExtension, InlineExtension,
                                         DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = ptr %1$s";

  void Apply(interpretter::ExecutionContext& ctx) const {
    ctx.current_frame()->regs_.set(result, Apply(ctx.resolve(operand)));
  }
  static type::Pointer const* Apply(type::Type const* operand) {
    return type::Ptr(operand);
  }

  RegOr<type::Type const*> operand;
  Reg result;
};

struct BufPtrInstruction
    : base::Extend<BufPtrInstruction>::With<ByteCodeExtension, InlineExtension,
                                            DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = buf-ptr %1$s";

  void Apply(interpretter::ExecutionContext& ctx) const {
    ctx.current_frame()->regs_.set(result, Apply(ctx.resolve(operand)));
  }
  static type::BufferPointer const* Apply(type::Type const* operand) {
    return type::BufPtr(operand);
  }

  RegOr<type::Type const*> operand;
  Reg result;
};

struct StructInstruction {
  StructInstruction(module::BasicModule const* mod, type::Struct* s,
                    std::vector<StructField> fields,
                    std::optional<ir::Fn> move_assign, std::optional<ir::Fn> dtor)
      : mod(mod),
        struct_(s),
        fields(std::move(fields)),
        move_assign(move_assign),
        dtor(dtor) {}
  ~StructInstruction() {}

  std::string to_string() const {
    // TODO
    return absl::StrCat(stringify(result), " = struct TODO");
  }

  struct control_bits {
    uint8_t length_is_reg : 1;
    uint8_t type_is_reg : 1;
  };

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write<uint16_t>(fields.size());
    writer->Write(mod);
    writer->Write(struct_);

    // TODO shuffling fields order?
    for (auto const& field : fields) {
      writer->Write(field.name());
      if (auto* v = field.initial_value()) {
        writer->Write(true);
        writer->Write(field.type().value());
        writer->Write(*v);
      } else {
        writer->Write(false);
        writer->Write(field.type());
      }
    }

    writer->Write(move_assign.has_value());
    if (move_assign.has_value()) { writer->Write(*move_assign); }

    writer->Write(dtor.has_value());
    if (dtor.has_value()) { writer->Write(*dtor); }

    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) {
    for (auto& field : fields) {
      if (auto* r = field.type_reg()) { inliner.Inline(*r); }
      if (auto* v = field.initial_value()) {
        if (auto* r = v->get_if<Reg>()) { inliner.Inline(*r); }
      }
    }
  }

  module::BasicModule const* mod;
  type::Struct* struct_;
  std::vector<StructField> fields;
  std::optional<ir::Fn> move_assign;
  std::optional<ir::Fn> dtor;
  Reg result;
};

struct ArrayInstruction
    : base::Extend<ArrayInstruction>::With<ByteCodeExtension, InlineExtension> {
  using length_t = int64_t;

  std::string to_string() const {
    return absl::StrCat(stringify(result), " = array ", stringify(length),
                        stringify(data_type));
  }

  void Apply(interpretter::ExecutionContext& ctx) const {
    ctx.current_frame()->regs_.set(
        result, type::Arr(ctx.resolve(length), ctx.resolve(data_type)));
  }

  RegOr<length_t> length;
  RegOr<type::Type const*> data_type;
  Reg result;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_TYPE_H
