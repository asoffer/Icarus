#ifndef ICARUS_IR_INSTRUCTION_TYPE_H
#define ICARUS_IR_INSTRUCTION_TYPE_H

#include <string_view>

#include "absl/container/flat_hash_set.h"
#include "absl/random/distributions.h"
#include "absl/random/random.h"
#include "base/extend.h"
#include "ir/byte_code_writer.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/inliner.h"
#include "ir/instruction/op_codes.h"
#include "ir/struct_field.h"
#include "ir/value/enum_and_flags.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/qual_type.h"
#include "type/struct.h"
#include "type/tuple.h"
#include "type/type.h"

namespace ir {

struct TupleInstruction
    : base::Extend<TupleInstruction>::With<ByteCodeExtension, InlineExtension> {
  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(
        "type ", stringify(this->result), " = tup ",
        absl::StrJoin(this->values, " ",
                      [](std::string* out, RegOr<type::Type> const& r) {
                        out->append(stringify(r));
                      }));
  }

  void Apply(interpretter::ExecutionContext& ctx) const {
    // TODO: It'd be better to deserialize and resolve simultaneously to avoid
    // this extra allocation.
    std::vector<type::Type> types;
    types.reserve(values.size());
    for (auto const& value : values) { types.push_back(ctx.resolve(value)); }
    ctx.current_frame().regs_.set(result, type::Tup(std::move(types)));
  }

  std::vector<RegOr<type::Type>> values;
  Reg result;
};

struct EnumInstruction
    : base::Extend<EnumInstruction>::With<ByteCodeExtension, InlineExtension> {
  void Apply(interpretter::ExecutionContext& ctx) const {
    using enum_t = ir::EnumVal::underlying_type;

    absl::flat_hash_set<enum_t> used_vals;

    for (auto const& [index, reg_or_value] : specified_values_) {
      used_vals.insert(ctx.resolve(reg_or_value));
    }

    absl::BitGen gen;

    absl::flat_hash_map<std::string, ir::EnumVal> mapping;

    for (size_t i = 0; i < names_.size(); ++i) {
      auto iter = specified_values_.find(i);
      if (iter != specified_values_.end()) {
        mapping.emplace(names_[i], ctx.resolve(iter->second));
        continue;
      }

      bool success;
      enum_t proposed_value;
      do {
        proposed_value = absl::Uniform<enum_t>(gen);
        success        = used_vals.insert(proposed_value).second;
      } while (not success);
      mapping.try_emplace(std::string(names_[i]), proposed_value);
    }

    type->SetMembers(std::move(mapping));
    type->complete();
    ctx.current_frame().regs_.set(result, type::Type(type));
  }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(stringify(result), " = enum (",
                        absl::StrJoin(names_, ", "), ")");
  }

  type::Enum* type;
  std::vector<std::string_view> names_;
  absl::flat_hash_map<uint64_t, RegOr<uint64_t>> specified_values_;
  Reg result;
};

struct FlagsInstruction
    : base::Extend<FlagsInstruction>::With<ByteCodeExtension, InlineExtension> {
  void Apply(interpretter::ExecutionContext& ctx) const {
    using flags_t = ir::FlagsVal::underlying_type;

    absl::flat_hash_set<flags_t> used_vals;

    for (auto const& [index, reg_or_value] : specified_values_) {
      used_vals.insert(ctx.resolve(reg_or_value));
    }

    absl::BitGen gen;

    absl::flat_hash_map<std::string, ir::FlagsVal> mapping;

    for (size_t i = 0; i < names_.size(); ++i) {
      auto iter = specified_values_.find(i);
      if (iter != specified_values_.end()) {
        mapping.emplace(names_[i], ctx.resolve(iter->second));
        continue;
      }

      bool success;
      flags_t proposed_value;
      do {
        proposed_value = flags_t{1} << absl::Uniform<flags_t>(
                             absl::IntervalClosedOpen, gen, 0,
                             std::numeric_limits<flags_t>::digits);
        success = used_vals.insert(proposed_value).second;
      } while (not success);
      mapping.try_emplace(std::string(names_[i]), proposed_value);
    }

    type->SetMembers(std::move(mapping));
    type->complete();
    ctx.current_frame().regs_.set(result, type::Type(type));
  }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(stringify(result), " = flags (",
                        absl::StrJoin(names_, ", "), ")");
  }

  type::Flags* type;
  std::vector<std::string_view> names_;
  absl::flat_hash_map<uint64_t, RegOr<uint64_t>> specified_values_;
  Reg result;
};

struct OpaqueTypeInstruction
    : base::Extend<OpaqueTypeInstruction>::With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = opaque %1$s";

  void Apply(interpretter::ExecutionContext& ctx) const {
    ctx.current_frame().regs_.set(
        result, type::Type(type::Allocate<type::Opaque>(mod)));
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

    std::vector<type::Type> rhs_types;
    rhs_types.reserve(rhs.size());
    for (auto const& t : rhs) { rhs_types.push_back(ctx.resolve(t)); }

    ctx.current_frame().regs_.set(
        result,
        type::Type(type::Func(std::move(params), std::move(rhs_types))));
  }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(
        stringify(result), " = (",
        absl::StrJoin(lhs, ", ",
                      [](std::string* out, RegOr<type::Type> const& r) {
                        out->append(stringify(r));
                      }),
        ") -> (",
        absl::StrJoin(rhs, ", ",
                      [](std::string* out, RegOr<type::Type> const& r) {
                        out->append(stringify(r));
                      }),
        ")");
  }

  std::vector<RegOr<type::Type>> lhs, rhs;
  Reg result;
};

struct PtrInstruction
    : base::Extend<PtrInstruction>::With<ByteCodeExtension, InlineExtension,
                                         DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = ptr %1$s";

  void Apply(interpretter::ExecutionContext& ctx) const {
    ctx.current_frame().regs_.set(result, Apply(ctx.resolve(operand)));
  }
  static type::Type Apply(type::Type operand) { return type::Ptr(operand); }

  RegOr<type::Type> operand;
  Reg result;
};

struct BufPtrInstruction
    : base::Extend<BufPtrInstruction>::With<ByteCodeExtension, InlineExtension,
                                            DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = buf-ptr %1$s";

  void Apply(interpretter::ExecutionContext& ctx) const {
    ctx.current_frame().regs_.set(result, Apply(ctx.resolve(operand)));
  }
  static type::Type Apply(type::Type operand) { return type::BufPtr(operand); }

  RegOr<type::Type> operand;
  Reg result;
};

struct StructInstruction
    : base::Extend<StructInstruction>::With<ByteCodeExtension,
                                            InlineExtension> {
  // TODO field.type() can be null. If the field type is inferred from the
  // initial value.
  void Apply(interpretter::ExecutionContext& ctx) const {
    std::vector<type::Struct::Field> struct_fields;
    struct_fields.reserve(fields.size());
    for (auto const& field : fields) {
      absl::flat_hash_set<ir::Hashtag> tags;
      if (field.exported()) { tags.insert(ir::Hashtag::Export); }

      if (ir::Value const* init_val = field.initial_value()) {
        type::Type t = ctx.resolve(field.type());
        struct_fields.push_back(type::Struct::Field{
            .name          = std::string(field.name()),
            .type          = t,
            .initial_value = *init_val,
            .hashtags      = std::move(tags),
        });
      } else {
        struct_fields.push_back(type::Struct::Field{
            .name          = std::string(field.name()),
            .type          = ctx.resolve(field.type()),
            .initial_value = ir::Value(),
            .hashtags      = std::move(tags),
        });
      }
    }

    struct_->AppendFields(std::move(struct_fields));
    if (move_assign) { struct_->SetMoveAssignment(*move_assign); }
    if (dtor) { struct_->SetDestructor(*dtor); }
    ctx.current_frame().regs_.set(result, type::Type(struct_));
  }

  std::string to_string() const {
    // TODO
    return absl::StrCat(stringify(result), " = struct TODO");
  }

  type::Struct* struct_;
  std::vector<StructField> fields;
  std::optional<ir::Fn> move_assign;
  std::optional<ir::Fn> dtor;
  Reg result;
};

struct ArrayInstruction
    : base::Extend<ArrayInstruction>::With<ByteCodeExtension, InlineExtension,
                                           DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = array %1$s %2$s";
  using length_t = int64_t;

  void Apply(interpretter::ExecutionContext& ctx) const {
    ctx.current_frame().regs_.set(
        result,
        type::Type(type::Arr(ctx.resolve(length), ctx.resolve(data_type))));
  }

  RegOr<length_t> length;
  RegOr<type::Type> data_type;
  Reg result;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_TYPE_H
