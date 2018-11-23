#include "ir/func.h"

#include "architecture.h"
#include "ast/function_literal.h"
#include "ir/arguments.h"
#include "property/property.h"
#include "property/property_map.h"
#include "type/function.h"
#include "type/pointer.h"

namespace type {
Pointer const *Ptr(const Type *);
}  // namespace type

namespace ir {
thread_local Func *Func::Current{nullptr};

Register Func::Argument(u32 n) const { return Register(reg_map_.at(n)); }

Register Func::Return(u32 n) const {
  return Register(reg_map_.at(type_->input.size() + n));
}

Func::Func(Module *mod, type::Function const *fn_type,
           base::vector<std::pair<std::string, ast::Expression *>> args)
    : type_(fn_type),
      args_(std::move(args)),
      num_regs_(static_cast<i32>(type_->input.size() + type_->output.size())),
      mod_(mod) {
  // Set the references for arguments and returns
  for (i32 i = -static_cast<i32>(type_->output.size());
       i < static_cast<i32>(type_->input.size()); ++i) {
    references_[Register{i}];
  }

  for (size_t i = 0; i < args_.size(); ++i) { lookup_[args_[i].first] = i; }

  auto arch = Architecture::InterprettingMachine();
  i32 i     = 0;
  for (auto *t : type_->input) {
    auto entry = arch.MoveForwardToAlignment(t, reg_size_);
    reg_map_.emplace(i++, Register(entry));
    reg_size_ = entry + arch.bytes(t);
  }

  // Return registers are just negative integers starting at -1 and decreasing
  for (auto *t : type_->output) {
    --neg_bound_;
    reg_map_.emplace(neg_bound_, Register(neg_bound_));
  }

  ASSERT(args_.size() == fn_type->input.size());
  blocks_.emplace_back(this);
}

base::unordered_map<BasicBlock const *, std::unordered_set<BasicBlock const *>>
Func::GetIncomingBlocks() const {
  base::unordered_map<BasicBlock const *,
                      std::unordered_set<BasicBlock const *>>
      incoming;
  for (auto const &b : blocks_) {
    ASSERT(b.cmds_.size() > 0u);
    auto const &last = b.cmds_.back();
    switch (last.op_code_) {
      case Op::UncondJump: incoming[&block(last.block_)].insert(&b); break;
      case Op::CondJump:
        incoming[&block(last.cond_jump_.blocks_[0])].insert(&b);
        incoming[&block(last.cond_jump_.blocks_[1])].insert(&b);
        break;
      case Op::ReturnJump: /* Nothing to do */ break;
      default: UNREACHABLE(ir::OpCodeStr(last.op_code_));
    }
  }
  return incoming;
}

Cmd const *Func::Command(Register reg) const {
  auto iter = reg_to_cmd_.find(reg);
  if (iter == reg_to_cmd_.end()) { return nullptr; }
  return &Command(iter->second);
}

static base::vector<std::pair<ir::Func, prop::PropertyMap>> InvariantsFor(
    ir::Func *fn, base::vector<ast::Expression *> const &exprs) {
  base::vector<std::pair<ir::Func, prop::PropertyMap>> result;
  // Resreve to guarantee pointer stability.
  for (auto const &expr : exprs) {
    auto & [ func, prop_map ] = result.emplace_back(
        std::piecewise_construct,
        std::forward_as_tuple(
            fn->mod_, type::Func(fn->type_->input, {type::Bool}), fn->args_),
        std::forward_as_tuple());

    CURRENT_FUNC(&func) {
      ir::BasicBlock::Current = func.entry();
      // TODO bound constants?
      Context ctx(fn->mod_);
      ir::SetRet(0, expr->EmitIR(&ctx)[0], &ctx);
      ir::ReturnJump();
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
  std::vector<std::pair<BasicBlock const *, ir::Cmd const *>> cmds;
  for (const auto &block : blocks_) {
    for (const auto &cmd : block.cmds_) {
      if (cmd.op_code_ != Op::Call) { continue; }
      // If it's a register it isn't known at compile time and therefore is not
      // allowed to have preconditions. If it's a foreign function we also don't
      // allow preconditions. This can be handled correctly by declaring the
      // foreign function locally and wrapping it.
      if (cmd.call_.fn_.is_reg_) { continue; }
      if (!cmd.call_.fn_.val_.is_fn()) { continue; }
      if (cmd.call_.fn_.val_.func()->preconditions_.empty()) { continue; }
      cmds.emplace_back(&block, &cmd);
    }
  }
  if (cmds.empty()) { return; }

  auto prop_map = prop::PropertyMap(this);

  for (auto const & [ block, cmd ] : cmds) {
    // TODO can preconditions be foreign functions?!
    for (auto const & [ precond, precond_prop_map ] :
         cmd->call_.fn_.val_.func()->preconditions_) {
      auto prop_copy = precond_prop_map.with_args(*cmd->call_.arguments_,
                                                  prop_map.view_.at(block));

      prop::BoolProp prop = prop_copy.Returns();
      if (!prop.can_be_true_) {
        LOG << "Provably false!";
      } else if (prop.can_be_false_) {
        LOG << "Not provably true!";
      } else {
        LOG << "Okay";
      }
    }
  }
}

std::ostream &operator<<(std::ostream &os, ir::Func const &f) {
  os << "\n" << f.name() << ": " << f.type_->to_string();
  for (size_t i = 0; i < f.blocks_.size(); ++i) {
    os << "\n block #" << i << "\n" << f.blocks_[i];
  }
  return os;
}

std::string Func::name() const {
  std::stringstream ss;
  ss << this;
  return ss.str();
}

}  // namespace ir
