#ifndef ICARUS_IR_CMD_PHI_H
#define ICARUS_IR_CMD_PHI_H

#include "absl/strings/str_cat.h"
#include "ir/cmd/util.h"

namespace ir {
struct PhiCmd {
  constexpr static cmd_index_t index = 36;

  static std::string DebugString(base::untyped_buffer::const_iterator *iter) {
    auto prime_index = iter->read<uint8_t>();
    auto num_options = iter->read<uint16_t>();
    std::vector<BasicBlock const *> blocks;
    blocks.reserve(num_options);
    for (uint16_t i = 0; i < num_options; ++i) {
      blocks.push_back(iter->read<BasicBlock const *>());
    }

    auto vals = internal::Deserialize<uint16_t, int64_t>(
        iter, [](Reg reg) -> RegOr<int64_t> { return reg; });

    auto result_reg = iter->read<Reg>();
    using base::stringify;
    std::string out = absl::StrCat(stringify(result_reg), "= phi\n");
    for (uint16_t i = 0; i < num_options; ++i) {
      absl::StrAppend(&out, "      [", stringify(blocks[i]), " => ",
                      stringify(vals[i]), "]\n");
    }
    return out;
  }
};

}  // namespace ir

#endif  // ICARUS_IR_CMD_PHI_H
