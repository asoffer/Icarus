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
#include "type/function.h"
#include "type/pointer.h"
#include "type/qual_type.h"
#include "type/type.h"
#include "type/variant.h"

namespace ir {

struct TupleInstruction
    : base::Extend<TupleInstruction>::With<InlineExtension> {
  using variadic                      = type::Type const*;
  static constexpr cmd_index_t kIndex = internal::kTupleInstructionNumber;

  static type::Type const* Apply(std::vector<type::Type const*> entries) {
    return type::Tup(std::move(entries));
  }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(
        "type ", stringify(this->result), " = tup ",
        absl::StrJoin(this->values, " ",
                      [](std::string* out, RegOr<type::Type const*> const& r) {
                        out->append(stringify(r));
                      }));
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write(kIndex);
    internal::WriteBits<uint16_t, RegOr<type::Type const*>>(
        writer, values,
        [](RegOr<type::Type const*> const& r) { return r.is_reg(); });

    for (auto const& x : values) {
      x.apply([&](auto v) { writer->Write(v); });
    }

    writer->Write(result);
  }

  std::vector<RegOr<type::Type const*>> values;
  Reg result;
};

struct VariantInstruction
    : base::Extend<TupleInstruction>::With<InlineExtension> {
  using variadic                      = type::Type const*;
  static constexpr cmd_index_t kIndex = internal::kVariantInstructionNumber;

  static type::Type const* Apply(std::vector<type::Type const*> entries) {
    return type::Var(std::move(entries));
  }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(
        "type ", stringify(this->result), " = var ",
        absl::StrJoin(this->values, " ",
                      [](std::string* out, RegOr<type::Type const*> const& r) {
                        out->append(stringify(r));
                      }));
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write(kIndex);
    internal::WriteBits<uint16_t, RegOr<type::Type const*>>(
        writer, values,
        [](RegOr<type::Type const*> const& r) { return r.is_reg(); });

    for (auto const& x : values) {
      x.apply([&](auto v) { writer->Write(v); });
    }

    writer->Write(result);
  }

  std::vector<RegOr<type::Type const*>> values;
  Reg result;
};

struct EnumerationInstruction {
  static constexpr cmd_index_t kIndex = internal::kEnumerationInstructionNumber;

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
    writer->Write(kIndex);
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
          WriteByteCodeExtension, InlineExtension, DebugFormatExtension> {
  constexpr static cmd_index_t kIndex = internal::kOpaqueTypeInstructionNumber;
  static constexpr std::string_view kDebugFormat = "%2$s = opaque %1$s";

  module::BasicModule const* mod;
  Reg result;
};

struct ArrowInstruction {
  constexpr static cmd_index_t kIndex = internal::kArrowInstructionNumber;

  ArrowInstruction(std::vector<RegOr<type::Type const*>> lhs,
                   std::vector<RegOr<type::Type const*>> rhs)
      : lhs(std::move(lhs)), rhs(std::move(rhs)) {}
  ~ArrowInstruction() {}

  // TODO take parameters, or a span if you don't want construction of names?
  static type::Type const* Apply(std::vector<type::Type const*> const& lhs,
                                 std::vector<type::Type const*> rhs) {
    // TODO named arguments
    core::Params<type::QualType> params;
    params.reserve(lhs.size());
    for (auto* t : lhs) {
      // TODO push qualtype into `Apply` parameters
      params.append(core::AnonymousParam(type::QualType::NonConstant(t)));
    }
    return type::Func(std::move(params), std::move(rhs));
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

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write(kIndex);

    internal::WriteBits<uint16_t, RegOr<type::Type const*>>(
        writer, lhs,
        [](RegOr<type::Type const*> const& r) { return r.is_reg(); });
    absl::c_for_each(lhs, [&](RegOr<type::Type const*> const& x) {
      x.apply([&](auto v) { writer->Write(v); });
    });

    internal::WriteBits<uint16_t, RegOr<type::Type const*>>(
        writer, rhs,
        [](RegOr<type::Type const*> const& r) { return r.is_reg(); });
    absl::c_for_each(rhs, [&](RegOr<type::Type const*> const& x) {
      x.apply([&](auto v) { writer->Write(v); });
    });

    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(lhs);
    inliner.Inline(rhs);
    inliner.Inline(result);
  }

  std::vector<RegOr<type::Type const*>> lhs, rhs;
  Reg result;
};

struct PtrInstruction
    : base::Extend<PtrInstruction>::With<
          WriteByteCodeExtension, InlineExtension, DebugFormatExtension> {
  using unary                         = type::Type const*;
  static constexpr cmd_index_t kIndex = internal::kPtrInstructionNumber;
  static constexpr std::string_view kDebugFormat = "%2$s = ptr %1$s";

  static type::Pointer const* Apply(type::Type const* operand) {
    return type::Ptr(operand);
  }

  RegOr<type::Type const*> operand;
  Reg result;
};

struct BufPtrInstruction
    : base::Extend<BufPtrInstruction>::With<
          WriteByteCodeExtension, InlineExtension, DebugFormatExtension> {
  using unary                         = type::Type const*;
  static constexpr cmd_index_t kIndex = internal::kBufPtrInstructionNumber;
  static constexpr std::string_view kDebugFormat = "%2$s = buf-ptr %1$s";

  static type::BufferPointer const* Apply(type::Type const* operand) {
    return type::BufPtr(operand);
  }

  RegOr<type::Type const*> operand;
  Reg result;
};

struct StructInstruction {
  static constexpr cmd_index_t kIndex = internal::kStructInstructionNumber;
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
    writer->Write(kIndex);
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

struct ArrayInstruction {
  static constexpr cmd_index_t kIndex = internal::kArrayInstructionNumber;
  using length_t                      = int64_t;

  ArrayInstruction(RegOr<length_t> length, RegOr<type::Type const*> data_type)
      : length(length), data_type(data_type) {}
  ~ArrayInstruction() {}

  std::string to_string() const {
    return absl::StrCat(stringify(result), " = array ", stringify(length),
                        stringify(data_type));
  }

  struct control_bits {
    uint8_t length_is_reg : 1;
    uint8_t type_is_reg : 1;
  };

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write(kIndex);
    writer->Write(control_bits{.length_is_reg = length.is_reg(),
                               .type_is_reg   = data_type.is_reg()});

    length.apply([&](auto v) { writer->Write(v); });
    data_type.apply([&](auto v) { writer->Write(v); });
    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(length);
    inliner.Inline(data_type);
    inliner.Inline(result);
  }

  RegOr<length_t> length;
  RegOr<type::Type const*> data_type;
  Reg result;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_TYPE_H
