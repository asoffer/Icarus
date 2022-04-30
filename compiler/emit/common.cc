#include "compiler/emit/common.h"

#include "absl/cleanup/cleanup.h"
#include "compiler/instructions.h"
#include "type/cast.h"
#include "type/enum.h"
#include "type/flags.h"

namespace compiler {

ir::Reg RegisterReferencing(SubroutineBlockReference current, type::Type t,
                            ir::PartialResultRef const &value) {
  if (t.is_big() or t.is<type::Pointer>()) {
    return current.block->Append(ir::RegisterInstruction<ir::addr_t>{
        .operand = value.get<ir::addr_t>(),
        .result  = current.subroutine->Reserve(),
    });
  } else {
    if (auto const *p = t.if_as<type::Primitive>()) {
      return p->Apply([&]<typename T>() {
        if constexpr (base::meta<T> == base::meta<ir::Integer>) {
          return current.block->Append(ir::RegisterInstruction<ir::addr_t>{
              .operand = value.get<ir::addr_t>(),
              .result  = current.subroutine->Reserve(),
          });
        } else {
          return current.block->Append(ir::RegisterInstruction<T>{
              .operand = value.get<T>(),
              .result  = current.subroutine->Reserve(),
          });
        }
      });
    } else if (auto const *e = t.if_as<type::Enum>()) {
      return current.block->Append(
          ir::RegisterInstruction<type::Enum::underlying_type>{
              .operand = value.get<type::Enum::underlying_type>(),
              .result  = current.subroutine->Reserve(),
          });
    } else if (auto const *e = t.if_as<type::Flags>()) {
      return current.block->Append(
          ir::RegisterInstruction<type::Flags::underlying_type>{
              .operand = value.get<type::Flags::underlying_type>(),
              .result  = current.subroutine->Reserve(),
          });
    } else {
      NOT_YET(t);
    }
  }
}

ir::RegOr<ir::addr_t> PtrFix(SubroutineBlockReference current,
                             ir::RegOr<ir::addr_t> addr,
                             type::Type desired_type) {
  if (desired_type.get()->is_big()) { return addr.reg(); }
  return current.block->Append(ir::LoadInstruction{
      .type   = desired_type,
      .addr   = addr,
      .result = current.subroutine->Reserve(),
  });
}

void EmitCast(CompilationDataReference ref, type::Type from, type::Type to,
              ir::PartialResultBuffer &buffer) {
#if defined(ICARUS_DEBUG)
  ASSERT(buffer.size() != 0u);
  absl::Cleanup c = [size = buffer.size(), &buffer] {
    ASSERT(size == buffer.size());
  };
#endif  // defined(ICARUS_DEBUG)

  // Ignore no-op conversions.
  if (to == from or type::CanCastInPlace(from, to)) { return; }

  // Allow any conversion to raw byte buffer pointers.
  if (from == type::Type(type::BufPtr(type::Byte)) or
      to == type::Type(type::BufPtr(type::Byte))) {
    return;
  }

  if (type::IsNumeric(from)) {
    if (auto const *enum_type = to.if_as<type::Enum>()) {
      ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t, uint32_t,
                 uint64_t>(from, [&]<typename T>() {
        EmitCast<T, type::Enum::underlying_type>(ref, buffer);
      });
    } else if (auto const *flags_type = to.if_as<type::Flags>()) {
      return ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                        uint32_t, uint64_t>(from, [&]<typename T>() {
        EmitCast<T, type::Flags::underlying_type>(ref, buffer);
      });
    } else {
      ApplyTypes<ir::Integer, int8_t, int16_t, int32_t, int64_t, uint8_t,
                 uint16_t, uint32_t, uint64_t, float, double>(
          to, [&]<typename To>() {
            ApplyTypes<ir::Integer, int8_t, int16_t, int32_t, int64_t, uint8_t,
                       uint16_t, uint32_t, uint64_t, float, double>(
                from,
                [&]<typename From>() { EmitCast<From, To>(ref, buffer); });
          });
    }
    return;
  }

  if (from == type::NullPtr) {
    buffer.pop_back();
    buffer.append(ir::Null());
    return;
  }

  if (auto const *enum_type = from.if_as<type::Enum>()) {
    ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t, uint32_t,
               uint64_t>(to, [&]<typename T>() {
      EmitCast<type::Enum::underlying_type, T>(ref, buffer);
    });
    return;
  }

  if (auto const *flags_type = from.if_as<type::Flags>()) {
    return ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                      uint32_t, uint64_t>(to, [&]<typename T>() {
      EmitCast<type::Flags::underlying_type, T>(ref, buffer);
    });
    return;
  }

  UNREACHABLE(from, " to ", to);
}

void ApplyImplicitCasts(CompilationDataReference ref, type::Type from,
                        type::QualType to, ir::PartialResultBuffer &buffer) {
  ASSERT(type::CanCastImplicitly(from, to.type()) == true);

  if (from == to.type()) { return; }
  if (to.type().is<type::Slice>()) {
    if (from.is<type::Slice>()) { return; }
    if (auto const *a = from.if_as<type::Array>()) {
      ir::RegOr<ir::addr_t> data   = buffer[0].get<ir::addr_t>();
      type::Slice::length_t length = a->length().value();
      auto alloc                   = ref.state().TmpAlloca(to.type());

      ref.current_block()->Append(ir::StoreInstruction<ir::addr_t>{
          .value    = data,
          .location = ref.current_block()->Append(type::SliceDataInstruction{
              .slice  = alloc,
              .result = ref.current().subroutine->Reserve(),
          }),
      });
      ref.current_block()->Append(ir::StoreInstruction<type::Slice::length_t>{
          .value    = length,
          .location = ref.current_block()->Append(type::SliceLengthInstruction{
              .slice  = alloc,
              .result = ref.current().subroutine->Reserve(),
          }),
      });
      buffer.clear();
      buffer.append(alloc);
    }
  }

  auto const *bufptr_from_type = from.if_as<type::BufferPointer>();
  auto const *ptr_to_type      = to.type().if_as<type::Pointer>();
  if (bufptr_from_type and ptr_to_type and
      type::CanCastImplicitly(bufptr_from_type, ptr_to_type)) {
    return;
  }

  if (from == type::Integer and type::IsIntegral(to.type())) {
    to.type().as<type::Primitive>().Apply([&]<typename T>() {
      if constexpr (std::is_integral_v<T>) {
        ir::RegOr<T> result =
            ref.current_block()->Append(ir::CastInstruction<T(ir::Integer)>{
                .value  = buffer.back().get<ir::addr_t>(),
                .result = ref.current().subroutine->Reserve(),
            });
        buffer.pop_back();
        buffer.append(result);
      } else {
        UNREACHABLE(typeid(T).name());
      }
    });
  }
}

void ApplyImplicitCasts(CompilationDataReference ref, type::Type from,
                        type::QualType to, ir::CompleteResultBuffer &buffer) {
  ASSERT(type::CanCastImplicitly(from, to.type()) == true);

  if (from == to.type()) { return; }
  if (to.type().is<type::Slice>()) {
    if (from.is<type::Slice>()) { return; }
    if (auto const *a = from.if_as<type::Array>()) {
      ir::addr_t data              = buffer[0].get<ir::addr_t>();
      type::Slice::length_t length = a->length().value();
      auto alloc                   = ref.state().TmpAlloca(to.type());
      ref.current_block()->Append(ir::StoreInstruction<ir::addr_t>{
          .value    = data,
          .location = ref.current_block()->Append(type::SliceDataInstruction{
              .slice  = alloc,
              .result = ref.current().subroutine->Reserve(),
          }),
      });
      ref.current_block()->Append(ir::StoreInstruction<type::Slice::length_t>{
          .value    = length,
          .location = ref.current_block()->Append(type::SliceLengthInstruction{
              .slice  = alloc,
              .result = ref.current().subroutine->Reserve(),
          }),
      });
      buffer.clear();
      buffer.append(alloc);
    }
  }

  auto const *bufptr_from_type = from.if_as<type::BufferPointer>();
  auto const *ptr_to_type      = to.type().if_as<type::Pointer>();
  if (bufptr_from_type and ptr_to_type and
      type::CanCastImplicitly(bufptr_from_type, ptr_to_type)) {
    return;
  }

  if (from == type::Integer and type::IsIntegral(to.type())) {
    to.type().as<type::Primitive>().Apply([&]<typename T>() {
      if constexpr (std::is_integral_v<T>) {
        auto result = buffer.back().get<ir::Integer>();
        buffer.pop_back();
        buffer.append(static_cast<T>(result.value()));
      } else {
        UNREACHABLE(typeid(T).name());
      }
    });
  }
}

}  // namespace compiler
