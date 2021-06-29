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

void Compiler::EmitToBuffer(ast::Cast const *node, base::untyped_buffer &out) {
  type::Type to_type = context().qual_types(node)[0].type();
  auto from_type     = context().qual_types(node->expr())[0].type();
  EmitToBuffer(node->expr(), out);

  if (to_type == from_type or
      from_type == type::Type(type::BufPtr(type::Byte)) or
      to_type == type::Type(type::BufPtr(type::Byte))) {
    return;
  }

  if (to_type == type::Char) {
    if (from_type == type::U8) {
      auto result =
          builder().Cast<uint8_t, ir::Char>(out.get<ir::RegOr<uint8_t>>(0));
      out.clear();
      out.append(result);
    } else if (from_type == type::I8) {
      auto result =
          builder().Cast<int8_t, ir::Char>(out.get<ir::RegOr<int8_t>>(0));
      out.clear();
      out.append(result);
    } else {
      UNREACHABLE(from_type);
    }
  } else if (from_type == type::Char) {
    ASSERT(type::IsIntegral(to_type) == true);
    ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t, uint32_t,
               uint64_t>(to_type, [&]<typename T>() {
      auto result =
          builder().Cast<ir::Char, T>(out.get<ir::RegOr<ir::Char>>(0));
      out.clear();
      out.append(result);
    });
  } else if (type::IsNumeric(from_type)) {
    if (type::IsIntegral(from_type)) {
      ApplyTypes<ir::Integer, int8_t, int16_t, int32_t, int64_t, uint8_t,
                 uint16_t, uint32_t, uint64_t, float,
                 double>(to_type, [&]<typename To>() {
        ApplyTypes<ir::Integer, int8_t, int16_t, int32_t, int64_t, uint8_t,
                   uint16_t, uint32_t, uint64_t>(
            from_type, [&]<typename From>() {
              auto result =
                  builder().Cast<From, To>(out.get<ir::RegOr<From>>(0));
              out.clear();
              out.append(result);
            });
      });
    } else {
      ApplyTypes<float, double>(to_type, [&]<typename To>() {
        ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                   uint32_t, uint64_t, float, double>(
            from_type, [&]<typename From>() {
              auto result =
                  builder().Cast<From, To>(out.get<ir::RegOr<From>>(0));
              out.clear();
              out.append(result);
            });
      });
    }
  } else if (from_type == type::NullPtr) {
    out.clear();
    out.append(ir::RegOr<ir::addr_t>(ir::Null()));
  } else if (auto const *enum_type = from_type.if_as<type::Enum>()) {
    ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t, uint32_t,
               uint64_t>(to_type, [&]<typename T>() {
      auto result = builder().Cast<type::Enum::underlying_type, T>(
          out.get<ir::RegOr<type::Enum::underlying_type>>(0));
      out.clear();
      out.append(result);
    });
  } else if (auto const *flags_type = from_type.if_as<type::Flags>()) {
    return ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                      uint32_t, uint64_t>(to_type, [&]<typename T>() {
      auto result = builder().Cast<type::Flags::underlying_type, T>(
          out.get<ir::RegOr<type::Flags::underlying_type>>(0));
      out.clear();
      out.append(result);
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
