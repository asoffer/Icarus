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

struct EnumerationInstruction
    : base::Extend<EnumerationInstruction>::With<ByteCodeExtension,
                                                 InlineExtension> {
  enum class Kind { Enum, Flags };

  void Apply(interpretter::ExecutionContext& ctx) const {
    NOT_YET();
    // using enum_t = uint64_t;

    // std::vector<std::pair<std::string_view, std::optional<enum_t>>> enumerators;
    // enumerators.reserve(num_enumerators);
    // for (uint16_t i = 0; i < num_enumerators; ++i) {
    //   enumerators.emplace_back(iter->read<std::string_view>(), std::nullopt);
    // }

    // absl::flat_hash_set<enum_t> vals;

    // specified_values_
    // for (uint16_t i = 0; i < num_specified; ++i) {
    //   uint64_t index            = iter->read<uint64_t>();
    //   auto b                    = iter->read<bool>();
    //   enum_t val                = ctx->ReadAndResolve<enum_t>(b, iter);
    //   enumerators[index].second = val;
    //   vals.insert(val);
    // }

    // absl::BitGen gen;

    // switch (kind_) {
    //   case Kind::Enum: {
    //     for (auto& [name, maybe_val] : enumerators) {
    //       if (not maybe_val.has_value()) {
    //         bool success;
    //         enum_t x;
    //         do {
    //           x         = absl::Uniform<enum_t>(gen);
    //           success   = vals.insert(x).second;
    //           maybe_val = x;
    //         } while (not success);
    //       }
    //     }
    //     absl::flat_hash_map<std::string, ir::EnumVal> mapping;

    //     for (auto [name, maybe_val] : enumerators) {
    //       ASSERT(maybe_val.has_value() == true);
    //       mapping.emplace(std::string(name), ir::EnumVal{maybe_val.value()});
    //     }

    //     ctx->current_frame()->regs_.set(
    //         iter->read<ir::Reg>(),
    //         type::Allocate<type::Enum>(mod, std::move(mapping)));
    //   } break;
    //   case Kind::Flags: {
    //     for (auto& [name, maybe_val] : enumerators) {
    //       if (not maybe_val.has_value()) {
    //         bool success;
    //         enum_t x;
    //         do {
    //           x       = absl::Uniform<enum_t>(absl::IntervalClosedOpen, gen, 0,
    //                                     std::numeric_limits<enum_t>::digits);
    //           success = vals.insert(x).second;
    //           maybe_val = x;
    //         } while (not success);
    //       }
    //     }

    //     absl::flat_hash_map<std::string, ir::FlagsVal> mapping;

    //     for (auto [name, maybe_val] : enumerators) {
    //       ASSERT(maybe_val.has_value() == true);
    //       mapping.emplace(std::string(name),
    //                       ir::FlagsVal{enum_t{1} << maybe_val.value()});
    //     }

    //     ctx->current_frame()->regs_.set(
    //         iter->read<ir::Reg>(),
    //         type::Allocate<type::Flags>(mod, std::move(mapping)));
    //   } break;
    // }
  }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(stringify(result),
                        kind_ == Kind::Enum ? " = enum (" : " = flags (",
                        absl::StrJoin(names_, ", "), ")");
  }

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

struct StructInstruction
    : base::Extend<StructInstruction>::With<ByteCodeExtension,
                                            InlineExtension> {
  void Apply(interpretter::ExecutionContext& ctx) const {
    NOT_YET();
    //       uint16_t num = iter->read<uint16_t>();
    //       module::BasicModule const *mod =
    //           iter->read<module::BasicModule const *>();
    //       type::Struct *struct_type = iter->read<type::Struct *>();
    //
    //       std::vector<type::Struct::Field> fields;
    //       fields.reserve(num);
    //       for (uint16_t i = 0; i < num; ++i) {
    //         std::string_view name = iter->read<std::string_view>();
    //         if (iter->read<bool>()) {
    //           type::Type const *t = iter->read<type::Type const *>();
    //
    //           ir::Value init_val = iter->read<ir::Value>();
    //
    //           fields.push_back(type::Struct::Field{
    //               .name          = std::string(name),
    //               .type          = t,
    //               .initial_value = init_val,
    //               .hashtags_     = {},
    //           });
    //         } else {
    //           fields.push_back(type::Struct::Field{
    //               .name = std::string(name),
    //               .type = ctx->resolve(
    //                   iter->read<ir::RegOr<type::Type const *>>().get()),
    //               .initial_value = ir::Value(),
    //               .hashtags_     = {},
    //           });
    //         }
    //       }
    //
    //       struct_type->AppendFields(std::move(fields));
    //
    //       if (iter->read<bool>()) {
    //         struct_type->SetMoveAssignment(iter->read<ir::Fn>());
    //       }
    //
    //       if (iter->read<bool>()) {
    //         struct_type->SetDestructor(iter->read<ir::Fn>());
    //       }
    //
    //       type::Struct const *const_struct_type = struct_type;
    //       ctx->current_frame()->regs_.set(iter->read<ir::Reg>(),
    //       const_struct_type);
    //
  }

  std::string to_string() const {
    // TODO
    return absl::StrCat(stringify(result), " = struct TODO");
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
