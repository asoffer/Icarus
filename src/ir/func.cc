#include "ir/func.h"

#include "ast/function_literal.h"
#include "type/function.h"

namespace type {
const Type *Ptr(const Type *);
}  // namespace type

namespace IR {
Func *Func::Current{nullptr};

Val Func::Argument(u32 n) const {
  auto *arg_type = type_->input AT(n);
  if (arg_type->is_big()) { arg_type = type::Ptr(arg_type); }
  return Val::Reg(Register(n), arg_type);
}

Val Func::Return(u32 n) const {
  return Val::Reg(Register(type_->input.size() + n),
                  type::Ptr(type_->output AT(n)));
}

// TODO there's no reason to take args because they can be computed from the
// function literal.
Func::Func(Module *mod, AST::GeneratedFunction *fn,
           std::vector<std::pair<std::string, AST::Expression *>> args)
    : gened_fn_(fn),
      type_(&fn->type->as<type::Function>()),
      args_(std::move(args)),
      num_regs_(static_cast<i32>(type_->input.size() + type_->output.size())),
      mod_(mod) {
  ASSERT(args_.size() == type_->input.size());
  blocks_.emplace_back(this);
}

Func::Func(Module *mod, const type::Function *fn_type,
           std::vector<std::pair<std::string, AST::Expression *>> args)
    : type_(fn_type),
      args_(std::move(args)),
      num_regs_(static_cast<i32>(type_->input.size() + type_->output.size())),
      mod_(mod) {
  ASSERT(args_.size() == fn_type->input.size());
  blocks_.emplace_back(this);
}

std::unordered_map<const BasicBlock *, std::unordered_set<const BasicBlock *>>
Func::GetIncomingBlocks() const {
  std::unordered_map<const BasicBlock *, std::unordered_set<const BasicBlock *>>
      incoming;
  for (const auto &b : blocks_) {
    ASSERT(b.cmds_.size() > 0u);
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

void Func::CheckInvariants() const {
  for (const auto &expr : preconditions_) {
    LOG << "Precondition: " << expr->to_string(0);
  }
  for (const auto &expr : postconditions_) {
    LOG << "Postcondition: " << expr->to_string(0);
  }
}

} // namespace IR
