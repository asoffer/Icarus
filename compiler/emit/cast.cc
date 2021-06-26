#include "ast/ast.h"
#include "compiler/compiler.h"
#include "ir/value/char.h"

namespace compiler {

void Compiler::EmitCopyInit(
    ast::Cast const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  EmitCopyAssign(to[0], type::Typed<ir::Value>(EmitValue(node), t));
}

void Compiler::EmitMoveInit(
    ast::Cast const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  EmitMoveAssign(type::Typed<ir::RegOr<ir::addr_t>>(*to[0], t),
                 type::Typed<ir::Value>(EmitValue(node), t));
}

ir::Value Compiler::EmitValue(ast::Cast const *node) {
  type::Type to_type = context().qual_types(node)[0].type();
  auto values        = EmitValue(node->expr());

  if (to_type == type::Type_) { return ir::Value(values.get<type::Type>()); }

  auto from_type = context().qual_types(node->expr())[0].type();

  if (to_type == from_type or
          from_type == type::Type(type::BufPtr(type::Memory)) or
          to_type == type::Type(type::BufPtr(type::Memory))) {
    return values;
  }

  if (to_type == type::Char) {
    ASSERT((from_type == type::U8 or from_type == type::I8) == true);
    return ir::Value(
        builder().CastTo<ir::Char>(type::Typed<ir::Value>(values, from_type)));
  } else if (from_type == type::Char) {
    ASSERT(type::IsIntegral(to_type) == true);
    return ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                      uint32_t, uint64_t>(to_type, [&]<typename T>() {
      return ir::Value(
          builder().CastTo<T>(type::Typed<ir::Value>(values, from_type)));
    });
  } else if (type::IsNumeric(from_type)) {
    if (type::IsIntegral(from_type)) {
      return ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                        uint32_t, uint64_t, float, double>(
          to_type, [&]<typename T>() {
            return ir::Value(
                builder().CastTo<T>(type::Typed<ir::Value>(values, from_type)));
          });
    } else {
      return ApplyTypes<float, double>(to_type, [&]<typename T>() {
        return ir::Value(
            builder().CastTo<T>(type::Typed<ir::Value>(values, from_type)));
      });
    }
  } else if (from_type == type::NullPtr) {
    return ir::Value(ir::Null());
  } else if (auto const *enum_type = from_type.if_as<type::Enum>()) {
    return ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                      uint32_t, uint64_t>(to_type, [&]<typename T>() {
      return ir::Value(
          builder().CastTo<T>(type::Typed<ir::Value>(values, enum_type)));
    });
  } else if (auto const *flags_type = from_type.if_as<type::Flags>()) {
    return ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                      uint32_t, uint64_t>(to_type, [&]<typename T>() {
      return ir::Value(
          builder().CastTo<T>(type::Typed<ir::Value>(values, from_type)));
    });
  } else {
    NOT_YET(from_type, " to ", to_type);
  }
}

void Compiler::EmitMoveAssign(
    ast::Cast const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  EmitMoveAssign(to[0], type::Typed<ir::Value>(EmitValue(node), t));
}

void Compiler::EmitCopyAssign(
    ast::Cast const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  EmitCopyAssign(to[0], type::Typed<ir::Value>(EmitValue(node), t));
}

}  // namespace compiler
