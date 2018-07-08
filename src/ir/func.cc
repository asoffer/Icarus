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
  // Set the references for arguments and returns
  for (i32 i = 0;
       i < static_cast<i32>(type_->input.size() + type_->output.size()); ++i) {
    references_[Register{i}];
  }
  ASSERT(args_.size() == type_->input.size());
  blocks_.emplace_back(this);
}

Func::Func(Module *mod, const type::Function *fn_type,
           std::vector<std::pair<std::string, AST::Expression *>> args)
    : type_(fn_type),
      args_(std::move(args)),
      num_regs_(static_cast<i32>(type_->input.size() + type_->output.size())),
      mod_(mod) {
  // Set the references for arguments and returns
  for (i32 i = 0;
       i < static_cast<i32>(type_->input.size() + type_->output.size()); ++i) {
    references_[Register{i}];
  }
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

Cmd const *Func::Command(Register reg) const {
  auto iter = reg_to_cmd_.find(reg);
  if (iter == reg_to_cmd_.end()) { return nullptr; }
  return &Command(iter->second);
}

static std::vector<std::pair<IR::Func, prop::PropertyMap>> InvariantsFor(
    IR::Func *fn, const std::vector<AST::Expression *> &exprs) {
  std::vector<std::pair<IR::Func, prop::PropertyMap>> result;
  // Resreve to guarantee pointer stability.
  for (const auto &expr : exprs) {
    auto & [ func, prop_map ] = result.emplace_back(
        std::piecewise_construct,
        std::forward_as_tuple(fn->mod_, type::Func(fn->type_->input, {type::Bool}),
                              fn->args_),
        std::forward_as_tuple());

    CURRENT_FUNC(&func) {
      IR::BasicBlock::Current = func.entry();
      // TODO bound constants?
      Context ctx(fn->mod_);
      IR::SetReturn(0, expr->EmitIR(&ctx)[0]);
      IR::ReturnJump();
    }
    prop_map = prop::PropertyMap(&func);
  }
  return result;
}

void Func::ComputeInvariants() {
  preconditions_  = InvariantsFor(this, precondition_exprs_);
  postconditions_ = InvariantsFor(this, postcondition_exprs_);
}

void Func::CheckInvariants() {
  // auto prop_map = prop::PropertyMap(this);
  for (const auto& block : blocks_) {
    for (const auto& cmd : block.cmds_) {
      if (cmd.op_code_ != Op::Call) { continue; }
      if (std::holds_alternative<IR::Register>(cmd.args.back().value)) { continue; }
      // Only care about calls to functions known at compile-time (hence
      // ignoring the register above.

      ASSERT(std::holds_alternative<IR::Func *>(cmd.args.back().value));
      auto *fn = std::get<IR::Func *>(cmd.args.back().value);
      for (const auto & [ precond, prop_map ] : fn->preconditions_) {
        // TODO avoid this copy. Pass in some sort of view-type?
        auto args = cmd.args;
        args.pop_back();
        auto prop_copy = prop_map.with_args(args);
        LOG << prop_copy.Returns();

        // TODO Insert properties and recompute
        fn->dump();
      }
    }
  }
}

} // namespace IR
