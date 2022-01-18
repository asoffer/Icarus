#ifndef ICARUS_IR_INSTRUCTIONS_JUMP_H
#define ICARUS_IR_INSTRUCTIONS_JUMP_H

#include <cstring>
#include <memory>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "base/meta.h"
#include "base/universal_print.h"
#include "core/arguments.h"
#include "ir/value/arguments.h"
#include "ir/value/block.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "type/qual_type.h"

namespace ir {
struct BasicBlock;

struct JumpCmd {
  static JumpCmd Unreachable() { return JumpCmd(UnreachableJump{}); }
  static JumpCmd Return() { return JumpCmd(RetJump{}); }
  static JumpCmd Uncond(BasicBlock* block) {
    return JumpCmd(UncondJump{block});
  }
  static JumpCmd ToBlock(Block b, BasicBlock* after) {
    return JumpCmd(BlockJump{b, after});
  }
  static JumpCmd Cond(Reg r, BasicBlock* true_block, BasicBlock* false_block) {
    return JumpCmd(CondJump{r, true_block, false_block});
  }

  static JumpCmd Cond(RegOr<bool> r, BasicBlock* true_block,
                      BasicBlock* false_block) {
    return r.is_reg()
               ? JumpCmd(CondJump{r.reg(), true_block, false_block})
               : JumpCmd(UncondJump{r.value() ? true_block : false_block});
  }

  JumpCmd(JumpCmd const&)     = default;
  JumpCmd(JumpCmd&&) noexcept = default;

  JumpCmd& operator=(JumpCmd const&) = default;
  JumpCmd& operator=(JumpCmd&&) noexcept = default;

  struct UnreachableJump {};
  struct RetJump {};
  struct BlockJump {
    Block block;
    BasicBlock* after;
  };
  struct UncondJump {
    BasicBlock* block;
  };
  struct CondJump {
    Reg reg;
    BasicBlock* true_block;
    BasicBlock* false_block;
  };

  enum class Kind { Unreachable, Return, Uncond, Cond, BlockJump };
  Kind kind() const { return static_cast<Kind>(jump_.index()); }

  template <typename Fn>
  auto Visit(Fn&& fn) const {
    auto k = kind();
    switch (k) {
      case Kind::Unreachable: return fn(std::get<0>(jump_));
      case Kind::Return: return fn(std::get<1>(jump_));
      case Kind::Uncond: return fn(std::get<2>(jump_));
      case Kind::Cond: return fn(std::get<3>(jump_));
      case Kind::BlockJump: return fn(std::get<4>(jump_));
    }
    UNREACHABLE();
  }

  template <typename Fn>
  auto Visit(Fn&& fn) {
    auto k = kind();
    switch (k) {
      case Kind::Unreachable: return fn(std::get<0>(jump_));
      case Kind::Return: return fn(std::get<1>(jump_));
      case Kind::Uncond: return fn(std::get<2>(jump_));
      case Kind::Cond: return fn(std::get<3>(jump_));
      case Kind::BlockJump: return fn(std::get<4>(jump_));
    }
    UNREACHABLE();
  }

  Reg CondReg() const { return std::get<CondJump>(jump_).reg; }

  BasicBlock* CondTarget(bool b) const {
    if (auto* u = std::get_if<CondJump>(&jump_)) {
      return b ? u->true_block : u->false_block;
    } else {
      return nullptr;
    }
  }
  BasicBlock* UncondTarget() const {
    if (auto* u = std::get_if<UncondJump>(&jump_)) {
      return u->block;
    } else {
      return nullptr;
    }
  }

  std::string DebugString() const {
    return Visit([](auto const& j) -> std::string {
      using type = std::decay_t<decltype(j)>;
      if constexpr (std::is_same_v<type, UnreachableJump>) {
        return "unreachable";
      } else if constexpr (std::is_same_v<type, RetJump>) {
        return "return";
      } else if constexpr (std::is_same_v<type, UncondJump>) {
        return absl::StrFormat("uncond %p", j.block);
      } else if constexpr (std::is_same_v<type, CondJump>) {
        return absl::StrFormat("cond %s false: %p, true: %p",
                               base::UniversalPrintToString(j.reg),
                               j.false_block, j.true_block);
      } else if constexpr (std::is_same_v<type, BlockJump>) {
        return absl::StrFormat("jump scope-block-%s (%p)", j.block, j.after);
      } else {
        static_assert(base::always_false<type>());
      }
    });
  }

 private:
  template <typename T>
  explicit JumpCmd(T&& val) : jump_(std::forward<T>(val)) {}

  std::variant<UnreachableJump, RetJump, UncondJump, CondJump, BlockJump> jump_;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTIONS_JUMP_H
