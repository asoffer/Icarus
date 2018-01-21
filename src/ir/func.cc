#include "func.h"

#include "../type/type.h"

namespace IR {
Func *Func::Current{nullptr};

std::vector<std::unique_ptr<Func>> Func::All;
Val Func::Argument(u32 n) {
  return Val::Reg(Register(n), ir_type->input AT(n));
}

Func::Func(::Function *fn_type,
           std::vector<std::pair<std::string, AST::Expression *>> args)
    : type_(fn_type), ir_type(fn_type->ToIR()), args_(std::move(args)),
      num_regs_(static_cast<i32>(fn_type->input.size())) {
  ASSERT_EQ(args_.size(), fn_type->input.size());
  blocks_.push_back(std::move(Block(this)));
  i32 num_args = static_cast<i32>(args_.size());
  for (i32 i = 0; i < num_args; ++i) {
    reg_map_[Register(static_cast<i32>(i))] = CmdIndex{0, i - num_args};
  }
}

std::unordered_map<const Block *, std::unordered_set<const Block *>>
Func::GetIncoming() const {
  std::unordered_map<const Block *, std::unordered_set<const Block *>> incoming;
  for (const auto &b : blocks_) {
    const auto &last = b.cmds_.back();
    switch (last.op_code_) {
    case Op::UncondJump:
      incoming[&block(std::get<BlockIndex>(last.args[0].value))].insert(&b);
      break;
    case Op::CondJump:
      incoming[&block(std::get<BlockIndex>(last.args[1].value))].insert(&b);
      incoming[&block(std::get<BlockIndex>(last.args[2].value))].insert(&b);
      break;
    case Op::ReturnJump: /* Nothing to do */ break;
    default: dump(); UNREACHABLE(static_cast<int>(last.op_code_));
    }
  }
  // Hack: First entry depends on itself.
  incoming[&block(entry())].insert(&block(entry()));
  return incoming;
}

} // namespace IR
