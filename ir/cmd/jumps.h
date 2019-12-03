#ifndef ICARUS_IR_CMD_JUMP_H
#define ICARUS_IR_CMD_JUMP_H

#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "absl/types/span.h"
#include "base/debug.h"
#include "base/untyped_buffer.h"
#include "ir/cmd/util.h"
#include "ir/reg.h"
#include "ir/reg_or.h"

namespace ir {

struct JumpCmd {
  constexpr static cmd_index_t index = 20;
  enum class Kind : uint8_t { kRet, kUncond, kCond, kChoose };

  static std::string DebugString(base::untyped_buffer::const_iterator* iter) {
    using base::stringify;
    switch (iter->read<Kind>()) {
      case Kind::kRet: return "ret";
      case Kind::kUncond:
        return absl::StrCat("uncond ",
                            stringify(iter->read<BasicBlock const*>()));
        break;
      case Kind::kCond: {
        auto reg         = iter->read<Reg>();
        auto false_block = iter->read<BasicBlock const*>();
        auto true_block  = iter->read<BasicBlock const*>();
        return absl::StrCat("cond ", stringify(reg),
                            " false: ", stringify(false_block),
                            ", true: ", stringify(true_block));
      }
      case Kind::kChoose: {
        auto num = iter->read<uint16_t>();
        std::vector<std::pair<std::string_view, BasicBlock*>> entries;
        entries.reserve(num);
        for (uint16_t i = 0; i < num; ++i) {
          entries.emplace_back(iter->read<std::string_view>(), nullptr);
        }

        for (uint16_t i = 0; i < num; ++i) {
          entries[i].second = iter->read<BasicBlock*>();
        }

        return absl::StrCat(
            "choose ",
            absl::StrJoin(
                entries, " ",
                [](std::string* out,
                   std::pair<std::string_view, BasicBlock*> const& p) {
                  using base::stringify;
                  absl::StrAppend(out, p.first, " -> ", stringify(p.second));
                }));
      }
      default: UNREACHABLE();
    }
  }
};

}  // namespace ir

#endif  // ICARUS_IR_CMD_JUMP_H
