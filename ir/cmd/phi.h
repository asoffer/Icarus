#ifndef ICARUS_IR_CMD_PHI_H
#define ICARUS_IR_CMD_PHI_H

#include "ir/cmd/util.h"

namespace ir {
struct PhiCmd {
  constexpr static cmd_index_t index = 36;

  static std::string DebugString(base::untyped_buffer::const_iterator *iter) {
    NOT_YET();
  }
};

template <typename T>
RegOr<T> Phi(Reg r, absl::Span<BasicBlock const *const> blocks,
             absl::Span<RegOr<T> const> values) {
  ASSERT(blocks.size() == values.size());
  if (values.size() == 1u) { return values[0]; }

  auto &blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<PhiCmd>();
  blk.cmd_buffer_.append(PrimitiveIndex<T>());
  blk.cmd_buffer_.append<uint16_t>(values.size());
  for (auto block : blocks) { blk.cmd_buffer_.append(block); }
  internal::Serialize<uint16_t>(&blk.cmd_buffer_, values);

  Reg result = MakeResult<T>();
  blk.cmd_buffer_.append(result);
  return result;
}

template <typename T>
RegOr<T> Phi(absl::Span<BasicBlock const *const> blocks,
             absl::Span<RegOr<T> const> values) {
  return Phi(MakeResult<T>(), blocks, values);
}

inline Results Phi(type::Type const *type,
                   absl::flat_hash_map<BasicBlock *, Results> const &values) {
  if (values.size() == 1) { return values.begin()->second; }
  return type::Apply(type, [&](auto tag) {
    using T = typename decltype(tag)::type;
    std::vector<RegOr<T>> vals;
    vals.reserve(values.size());
    std::vector<BasicBlock const *> blocks;
    blocks.reserve(values.size());
    for (auto const &[key, val] : values) {
      blocks.push_back(key);
      vals.push_back(val.template get<T>(0));
    }
    return Results{Phi<T>(blocks, vals)};
  });
}
}  // namespace ir

#endif  // ICARUS_IR_CMD_PHI_H
