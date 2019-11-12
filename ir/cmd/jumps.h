#ifndef ICARUS_IR_CMD_JUMP_H
#define ICARUS_IR_CMD_JUMP_H

#include "absl/strings/str_join.h"
#include "absl/types/span.h"
#include "base/debug.h"
#include "ir/cmd/util.h"
#include "ir/cmd_buffer.h"
#include "ir/reg.h"
#include "ir/reg_or.h"

namespace ir {

struct JumpCmd {
  constexpr static cmd_index_t index = 20;
  enum class Kind : uint8_t { kRet, kUncond, kCond, kChoose };

  static std::string DebugString(base::untyped_buffer::const_iterator* iter) {
    std::string s;
    using base::stringify;
    switch (iter->read<Kind>()) {
      case Kind::kRet: s.append("ret"); break;
      case Kind::kUncond:
        s.append(stringify(iter->read<BasicBlock const*>()));
        break;
      case Kind::kCond: {
        s.append(stringify(iter->read<Reg>()));
        s.append(" false: ");
        s.append(stringify(iter->read<BasicBlock const*>()));
        s.append(", true: ");
        s.append(stringify(iter->read<BasicBlock const*>()));
      } break;
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

        s.append(absl::StrJoin(entries, " ",
                      [](std::string* out,
                         std::pair<std::string_view, BasicBlock*> const& p) {
                        using base::stringify;
                        absl::StrAppend(out, p.first, " -> ",
                                        stringify(p.second));
                      }));
      } break;
      default: UNREACHABLE();
    }
    return s;
  }
};

}  // namespace ir

#endif  // ICARUS_IR_CMD_JUMP_H
