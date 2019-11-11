#ifndef ICARUS_IR_CMD_CAST_H
#define ICARUS_IR_CMD_CAST_H

#include <string_view>

#include "ir/basic_block.h"
#include "ir/cmd/util.h"
#include "ir/cmd_buffer.h"
#include "ir/reg.h"
#include "ir/reg_or.h"

namespace ir {

struct CastCmd {
  constexpr static cmd_index_t index = 26;

  static std::string DebugString(base::untyped_buffer::const_iterator* iter) {
    return "NOT_YET";
  }
};

template <typename ToType, typename FromType>
RegOr<ToType> Cast(RegOr<FromType> r) {
  if (r.is_reg()) {
    auto& blk = *GetBuilder().CurrentBlock();
    blk.cmd_buffer_.append_index<CastCmd>();
    blk.cmd_buffer_.append(PrimitiveIndex<ToType>());
    blk.cmd_buffer_.append(PrimitiveIndex<FromType>());
    blk.cmd_buffer_.append(r.reg());
    Reg result = MakeResult<ToType>();
    blk.cmd_buffer_.append(result);
    return result;
  } else {
    return ToType(r.value());
  }
}

template <typename ToType>
RegOr<ToType> CastTo(type::Type const* from_type, ir::Results const& r) {
  if (from_type == type::Get<ToType>()) { return r.get<ToType>(0); }
  if (from_type == type::Int8) {
    return Cast<ToType, int8_t>(r.get<int8_t>(0));
  } else if (from_type == type::Nat8) {
    return Cast<ToType, uint8_t>(r.get<uint8_t>(0));
  } else if (from_type == type::Int16) {
    return Cast<ToType, int16_t>(r.get<int16_t>(0));
  } else if (from_type == type::Nat16) {
    return Cast<ToType, uint16_t>(r.get<uint16_t>(0));
  } else if (from_type == type::Int32) {
    return Cast<ToType, int32_t>(r.get<int32_t>(0));
  } else if (from_type == type::Nat32) {
    return Cast<ToType, uint32_t>(r.get<uint32_t>(0));
  } else if (from_type == type::Int64) {
    return Cast<ToType, int64_t>(r.get<int64_t>(0));
  } else if (from_type == type::Nat64) {
    return Cast<ToType, uint64_t>(r.get<uint64_t>(0));
  } else if (from_type == type::Float32) {
    return Cast<ToType, float>(r.get<float>(0));
  } else if (from_type == type::Float64) {
    return Cast<ToType, double>(r.get<double>(0));
  } else {
    UNREACHABLE();
  }
}
}  // namespace ir

#endif  // ICARUS_IR_CMD_CAST_H
