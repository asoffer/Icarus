#ifndef ICARUS_IR_CMD_TYPES_H
#define ICARUS_IR_CMD_TYPES_H

#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/container/flat_hash_map.h"
#include "absl/types/span.h"
#include "ir/basic_block.h"
#include "ir/cmd/util.h"
#include "ir/cmd_buffer.h"
#include "ir/reg.h"
#include "type/tuple.h"
#include "type/variant.h"

struct Module;

namespace ir {

template <cmd_index_t Index, typename T, T (*Fn)(std::vector<T>)>
struct VariadicCmd {
  constexpr static cmd_index_t index = Index;
  using type                         = T;
  constexpr static auto* fn_ptr      = Fn;

  static std::optional<BlockIndex> Execute(base::untyped_buffer::iterator* iter,
                                           std::vector<Addr> const& ret_slots,
                                           backend::ExecContext* ctx) {
    uint16_t num    = iter->read<uint16_t>();
    uint8_t current = 0;

    std::vector<T> vals;
    vals.reserve(num);
    {
      std::vector<bool> bits;
      bits.reserve(num);
      for (uint16_t i = 0; i < num; ++i) {
        if (i % 8 == 0) { current = ReverseByte(iter->read<uint8_t>()); }
        bits.push_back(current & 1);
        current >>= 1;
      }

      for (bool b : bits) {
        vals.push_back(b ? ctx->resolve<T>(iter->read<Reg>())
                         : iter->read<T>());
      }
      DEBUG_LOG("variadic")(vals);
    }

    auto& frame = ctx->call_stack.top();
    frame.regs_.set(GetOffset(frame.fn_, iter->read<Reg>()),
                    Fn(std::move(vals)));

    return std::nullopt;
  }

 private:
  static constexpr uint8_t ReverseByte(uint8_t byte) {
    byte = ((byte & 0b11110000) >> 4) | ((byte & 0b00001111) << 4);
    byte = ((byte & 0b11001100) >> 2) | ((byte & 0b00110011) << 2);
    byte = ((byte & 0b10101010) >> 1) | ((byte & 0b01010101) << 1);
    return byte;
  }
};

using VariantCmd = VariadicCmd<16, type::Type const*, type::Var>;
using TupleCmd   = VariadicCmd<17, type::Type const*, type::Tup>;
using ArrowCmd   = internal::BinaryCmd<18, /*std::equal_to<>*/, type::Type const*>;
using PtrCmd     = internal::UnaryCmd<19, type::Ptr, type::Type const*>;
namespace internal {

template <typename CmdType>
RegOr<typename CmdType::type> MakeVariadicImpl(
    absl::Span<RegOr<typename CmdType::type> const> vals) {
  using T = typename CmdType::type;
  {
    std::vector<T> vs;
    vs.reserve(vals.size());
    if (absl::c_all_of(vals, [&](RegOr<T> t) {
          if (t.is_reg_) { return false; }
          vs.push_back(t.val_);
          return true;
        })) {
      return CmdType::fn_ptr(vs);
    }
  }

  auto& blk = GetBlock();
  blk.cmd_buffer_.append_index<VariantCmd>();
  blk.cmd_buffer_.append<uint16_t>(vals.size());
  uint8_t reg_mask = 0;
  for (size_t i = 0; i < vals.size(); ++i) {
    if (vals[i].is_reg_) { reg_mask |= (1 << (7 - (i % 8))); }
    if (i % 8 == 7) {
      blk.cmd_buffer_.append(reg_mask);
      reg_mask = 0;
    }
  }
  if (vals.size() % 8 != 0) { blk.cmd_buffer_.append(reg_mask); }

  absl::c_for_each(vals, [&](RegOr<T> t) {
    if (t.is_reg_) {
      blk.cmd_buffer_.append(t.reg_);
    } else {
      blk.cmd_buffer_.append(t.val_);
    }
  });

  Reg result = MakeResult(type::Get<T>());
  blk.cmd_buffer_.append(result);
  DEBUG_LOG("variadic")(blk.cmd_buffer_.to_string());
  return RegOr<T>{result};
}

}  // namespace internal

inline RegOr<type::Type const*> MakeVariant(
    absl::Span<RegOr<type::Type const*> const> types) {
  return internal::MakeVariadicImpl<VariantCmd>(types);
}

inline RegOr<type::Type const*> MakeTuple(
    absl::Span<RegOr<type::Type const*> const> types) {
  return internal::MakeVariadicImpl<TupleCmd>(types);
}

// struct EnumCmd {
//   constexpr static cmd_index_t index = 15;
// 
//   enum class : uint8_t ValueKind{kIsReg, kIsVal, kNone};
// 
//   static std::optional<BlockIndex> Execute(base::untyped_buffer::iterator* iter,
//                                            std::vector<Addr> const& ret_slots,
//                                            backend::ExecContext* ctx) {
//     bool is_enum = iter->read<bool>();
//     auto* mod    = iter->read<::Module*>();
//     auto num     = iter->read<size_t>();
//     absl::flat_hash_map<std::string, int32_t> members;
//     // Used for rejection sampling. At some point we should optimize this when
//     // the rejection probability gets high (if, e.g., a small enum is chosen and
//     // most values are taken.
//     absl::flat_hash_set<int32_t> taken;
//     std::vector<std::string_view> to_be_inserted;
//     for (size_t i = 0; i < num; ++i) {
//       auto name = iter->read<std::string_view>();
//       switch (iter->read<ValueKind>()) {
//         case ValueKind::kIsReg: {
//           int32_t val  = ctx->resolve<int32_t>(iter->read<Reg>());
//           bool success = taken.insert(val).second;
//           ASSERT(success == true);
//           members.emplace(std::string{name}, val);
//         } break;
//         case ValueKind::kIsValue: {
//           auto val = iter->read<int32_t>();
//           members.emplace(std::string{name}, val);
//           bool success = taken.insert(val).second;
//           ASSERT(success == true);
//         } break;
//         case ValueKind::kNone: to_be_inserted.push_back(name); break;
//       }
//     }
// 
//     absl::BitGen gen;
//     for (std::string_view enumerator : to_be_inserted) {
//       std::string name = s;
//       int32_t x;
//       {
//       try_again:
//         x = absl::Uniform(gen, std::numeric_limits<int32_t>::lowest(),
//                           std::numeric_limits<int32_t>::max());
// 
//         bool success = taken.insert(x).second;
//         if (!success) { goto try_again; }
//       }
//       members.emplace(x, s);
//     }
// 
//     // TODO flags case too!
//     auto& frame = ctx->call_stack.top();
//     frame.regs_.set(GetOffset(frame.fn_, iter->read<Reg>()),
//                     new type::Enum(mod, std::move(members)));
//     return std::nullopt;
//   }
// };
// 
// namespace internal {
// inline Reg MakeImpl(
//     bool is_enum, ::Module* mod,
//     absl::Span<std::pair<std::string_view, std::optional<RegOr<int32_t>>>>
//         enumerators) {
//   auto& blk = GetBlock();
//   blk.cmd_buffer_.append_index<EnumCmd>();
//   blk.cmd_buffer_.append(is_enum);
//   blk.cmd_buffer_.append(mod);
//   blk.cmd_buffer_.append<size_t>(enumerators.size());
//   for (auto const & [ name, val ] : enumerators) {
//     blk.cmd_buffer_.append(name);
//     if (val.has_value()) {
//       if (val->is_reg_) {
//         blk.cmd_buffer_.append(EnumCmd::ValueKind::kIsReg);
//       } else {
//         blk.cmd_buffer_.append(EnumCmd::ValueKind::kVal);
//       }
//     } else {
//       blk.cmd_buffer_.append(EnumCmd::ValueKind::kNone);
//     }
//     blk.cmd_buffer_.append(name);
//   }
//   Reg result = MakeResult(type::Get<T>());
//   blk.cmd_buffer_.append(result);
// }
// 
// inline Reg MakeEnum(
//     ::Module* mod,
//     absl::Span<std::pair<std::string_view, std::optional<RegOr<int32_t>>>>
//         enumerators) {
//   return MakeImpl(true, mod, enumerators);
// }
// 
// inline Reg MakeFlags(
//     ::Module* mod,
//     absl::Span<std::pair<std::string_view, std::optional<RegOr<int32_t>>>>
//         enumerators) {
//   return MakeImpl(false, mod, enumerators);
// }
}  // namespace ir

#endif  // ICARUS_IR_CMD_TYPES_H
