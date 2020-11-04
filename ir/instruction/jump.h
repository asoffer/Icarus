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
#include "base/stringify.h"
#include "core/arguments.h"
#include "ir/value/reg.h"
#include "ir/value/value.h"
#include "type/typed_value.h"

namespace ir {
struct BasicBlock;

struct JumpCmd {
  static JumpCmd Unreachable() { return JumpCmd(UnreachableJump{}); }
  static JumpCmd Return() { return JumpCmd(RetJump{}); }
  static JumpCmd Uncond(BasicBlock* block) {
    return JumpCmd(UncondJump{block});
  }
  static JumpCmd Cond(Reg r, BasicBlock* true_block, BasicBlock* false_block) {
    return JumpCmd(CondJump{r, true_block, false_block});
  }
  static JumpCmd Choose(std::vector<std::string_view> names,
                        std::vector<BasicBlock*> blocks,
                        std::vector<core::Arguments<type::Typed<Value>>> args) {
    return JumpCmd(
        ChooseJump(std::move(names), std::move(blocks), std::move(args)));
  }

  JumpCmd(JumpCmd const&)     = default;
  JumpCmd(JumpCmd&&) noexcept = default;

  JumpCmd& operator=(JumpCmd const&) = default;
  JumpCmd& operator=(JumpCmd&&) noexcept = default;

  struct UnreachableJump {};
  struct RetJump {};
  struct UncondJump {
    BasicBlock* block;
  };
  struct CondJump {
    Reg reg;
    BasicBlock* true_block;
    BasicBlock* false_block;
  };
  struct ChooseJump {
    explicit ChooseJump(std::vector<std::string_view> names,
                        std::vector<BasicBlock*> blocks,
                        std::vector<core::Arguments<type::Typed<Value>>> args)
        : names_(std::move(names)),
          blocks_(std::move(blocks)),
          args_(std::move(args)) {}

    size_t size() const { return names_.size(); }
    absl::Span<std::string_view const> names() const { return names_; }
    absl::Span<BasicBlock* const> blocks() const { return blocks_; }
    absl::Span<core::Arguments<type::Typed<Value>> const> args() const {
      return args_;
    }

   private:
    std::vector<std::string_view> names_;
    std::vector<BasicBlock*> blocks_;
    std::vector<core::Arguments<type::Typed<Value>>> args_;
  };

  enum class Kind { Unreachable, Return, Uncond, Cond, Choose };
  Kind kind() const { return static_cast<Kind>(jump_.index()); }

  template <typename Fn>
  auto Visit(Fn&& fn) const {
    auto k = kind();
    switch (k) {
      case Kind::Unreachable: return fn(std::get<0>(jump_));
      case Kind::Return: return fn(std::get<1>(jump_));
      case Kind::Uncond: return fn(std::get<2>(jump_));
      case Kind::Cond: return fn(std::get<3>(jump_));
      case Kind::Choose: return fn(std::get<4>(jump_));
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
      case Kind::Choose: return fn(std::get<4>(jump_));
    }
    UNREACHABLE();
  }

  Reg CondReg() const { return std::get<CondJump>(jump_).reg; }

  ChooseJump const* IfAsChooseJump() const {
    return std::get_if<ChooseJump>(&jump_);
  }

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
      using base::stringify;
      if constexpr (std::is_same_v<type, UnreachableJump>) {
        return "unreachable";
      } else if constexpr (std::is_same_v<type, RetJump>) {
        return "return";
      } else if constexpr (std::is_same_v<type, UncondJump>) {
        return absl::StrCat("uncond ", stringify(j.block));
      } else if constexpr (std::is_same_v<type, CondJump>) {
        return absl::StrCat("cond ", stringify(j.reg),
                            " false: ", stringify(j.false_block),
                            ", true: ", stringify(j.true_block));
      } else if constexpr (std::is_same_v<type, ChooseJump>) {
        std::string out = "choose( ";
        for (std::string_view name : j.names()) {
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

  std::variant<UnreachableJump, RetJump, UncondJump, CondJump, ChooseJump>
      jump_;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTIONS_JUMP_H
