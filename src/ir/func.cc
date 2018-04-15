#include "func.h"

#include "../type/function.h"
#include "../ast/ast.h"

namespace IR {
Func *Func::Current{nullptr};

Val Func::Argument(u32 n) {
  return Val::Reg(Register(n), ir_type->input AT(n));
}

// TODO there's no reason to take args because they can be computed from the
// function literal.
Func::Func(Module *mod, AST::FunctionLiteral *fn_lit,
           std::vector<std::pair<std::string, AST::Expression *>> args)
    : fn_lit_(fn_lit),
      type_(&fn_lit->type->as<type::Function>()),
      ir_type(type_->ToIR()),
      args_(std::move(args)),
      num_regs_(static_cast<i32>(type_->input.size())),
      mod_(mod) {
  ASSERT_EQ(args_.size(), type_->input.size());
  blocks_.push_back(std::move(Block(this)));
  i32 num_args = static_cast<i32>(args_.size());
  for (i32 i = 0; i < num_args; ++i) {
    reg_map_[Register(static_cast<i32>(i))] =
        CmdIndex{BlockIndex{0}, i - num_args};
  }
}

Func::Func(Module *mod, const type::Function *fn_type,
           std::vector<std::pair<std::string, AST::Expression *>> args)
    : type_(fn_type),
      ir_type(fn_type->ToIR()),
      args_(std::move(args)),
      num_regs_(static_cast<i32>(fn_type->input.size())),
      mod_(mod) {
  ASSERT_EQ(args_.size(), fn_type->input.size());
  blocks_.push_back(std::move(Block(this)));
  i32 num_args = static_cast<i32>(args_.size());
  for (i32 i = 0; i < num_args; ++i) {
    reg_map_[Register(static_cast<i32>(i))] =
        CmdIndex{BlockIndex{0}, i - num_args};
  }
}

std::unordered_map<const Block *, std::unordered_set<const Block *>>
Func::GetIncomingBlocks() const {
  std::unordered_map<const Block *, std::unordered_set<const Block *>> incoming;
  for (const auto &b : blocks_) {
    ASSERT_GT(b.cmds_.size(), 0u);
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
