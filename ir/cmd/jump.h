#ifndef ICARUS_IR_CMD_JUMP_H
#define ICARUS_IR_CMD_JUMP_H

#include <cstring>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <memory>
#include <variant>

#include "base/stringify.h"
#include "base/util.h"
#include "ir/reg.h"

namespace ir {
struct BasicBlock;

struct JumpCmd {
  static JumpCmd Return() { return JumpCmd(RetJump{}); }
  static JumpCmd Uncond(BasicBlock* block) {
    return JumpCmd(UncondJump{block});
  }
  static JumpCmd Cond(Reg r, BasicBlock* true_block, BasicBlock* false_block) {
    return JumpCmd(CondJump{r, true_block, false_block});
  }
  static JumpCmd Choose(absl::Span<std::string_view const> blocks) {
    return JumpCmd(ChooseJump{blocks});
  }

  JumpCmd(JumpCmd const&)     = default;
  JumpCmd(JumpCmd&&) noexcept = default;

  JumpCmd& operator=(JumpCmd const&) = default;
  JumpCmd& operator=(JumpCmd&&) noexcept = default;

  struct RetJump {};
  struct UncondJump {
    BasicBlock * block;
  };
  struct CondJump {
    Reg reg;
    BasicBlock * true_block;
    BasicBlock * false_block;
  };
  struct ChooseJump {
    explicit ChooseJump(absl::Span<std::string_view const> b)
        : num_(b.size()), blocks_(new std::string_view[b.size()]) {
      std::memcpy(blocks_.get(), b.data(), sizeof(std::string_view) * b.size());
    }
    ChooseJump(ChooseJump const& j)
        : ChooseJump(absl::MakeSpan(&j.blocks_.get()[0], j.num_)) {}

    ChooseJump(ChooseJump&&) = default;

    ChooseJump& operator=(ChooseJump const& j) {
      if (&j == this) { return *this; }
      num_ = j.num_;
      blocks_ =
          std::unique_ptr<std::string_view[]>(new std::string_view[j.num_]);
      std::memcpy(blocks_.get(), j.blocks_.get(),
                  sizeof(std::string_view) * j.num_);
      return *this;
    }

    size_t size() const { return num_; }
    absl::Span<std::string_view const> blocks() const {
      return absl::MakeSpan(blocks_.get(), num_);
    }

    ChooseJump& operator=(ChooseJump&&) = default;

   private:
    size_t num_;
    std::unique_ptr<std::string_view[]> blocks_;
  };

  enum class Kind { Return, Uncond, Cond, Choose };
  Kind kind() const { return static_cast<Kind>(jump_.index()); }

  template <typename Fn>
  auto Visit(Fn&& fn) const {
    auto k = kind();
    switch (k) {
      case Kind::Return: return fn(std::get<0>(jump_));
      case Kind::Uncond: return fn(std::get<1>(jump_));
      case Kind::Cond: return fn(std::get<2>(jump_));
      case Kind::Choose: return fn(std::get<3>(jump_));
    }
    UNREACHABLE();
  }

  template <typename Fn>
  auto Visit(Fn&& fn) {
    auto k = kind();
    switch (k) {
      case Kind::Return: return fn(std::get<0>(jump_));
      case Kind::Uncond: return fn(std::get<1>(jump_));
      case Kind::Cond: return fn(std::get<2>(jump_));
      case Kind::Choose: return fn(std::get<3>(jump_));
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
    return Visit(
        [](auto const& j) -> std::string {
          using type = std::decay_t<decltype(j)>;
          using base::stringify;
          if constexpr (std::is_same_v<type, RetJump>) {
            return "return";
          } else if constexpr (std::is_same_v<type, UncondJump>) {
            return absl::StrCat("uncond ", stringify(j.block));
          } else if constexpr (std::is_same_v<type, CondJump>) {
            return absl::StrCat("cond ", stringify(j.reg),
                                " false: ", stringify(j.false_block),
                                ", true: ", stringify(j.true_block));
          } else if constexpr (std::is_same_v<type, ChooseJump>) {
            std::string out = "choose( ";
            for (std::string_view name : j.blocks()) {
              absl::StrAppend(&out, name, " ");
            }
            absl::StrAppend(&out, ")");
            return out;
          } else {
            static_assert(base::always_false<type>());
          }
        });
  }

 private:
  template <typename T>
  explicit JumpCmd(T&& val) : jump_(std::forward<T>(val)) {}

  std::variant<RetJump, UncondJump, CondJump, ChooseJump> jump_;
};

}  // namespace ir

#endif  // ICARUS_IR_CMD_JUMP_H
