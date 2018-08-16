#include "ir/func.h"

#include "architecture.h"
#include "ast/function_literal.h"
#include "property/property_map.h"
#include "type/function.h"

namespace type {
const Type *Ptr(const Type *);
}  // namespace type

namespace IR {
Func *Func::Current{nullptr};

Val Func::Argument(u32 n) const {
  auto *arg_type = type_->input.at(n);
  if (arg_type->is_big()) { arg_type = type::Ptr(arg_type); }
  return Val::Reg(Register(reg_map_.at(n)), arg_type);
}

Val Func::Return(u32 n) const {
  return Val::Reg(Register(reg_map_.at(type_->input.size() + n)),
                  type::Ptr(type_->output.at(n)));
}

// TODO there's no reason to take args because they can be computed from the
// function literal.
Func::Func(Module *mod, AST::GeneratedFunction *fn,
           base::vector<std::pair<std::string, AST::Expression *>> args)
    : gened_fn_(fn),
      type_(&fn->type->as<type::Function>()),
      args_(std::move(args)),
      num_regs_(static_cast<i32>(type_->input.size() + type_->output.size())),
      mod_(mod) {
  // Set the references for arguments and returns
  for (i32 i = -static_cast<i32>(type_->output.size());
       i < static_cast<i32>(type_->input.size()); ++i) {
    references_[Register{i}];
  }

  auto arch  = Architecture::InterprettingMachine();
  i32 i = 0;
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

  ASSERT(args_.size() == type_->input.size());
  blocks_.emplace_back(this);
}

Func::Func(Module *mod, const type::Function *fn_type,
           base::vector<std::pair<std::string, AST::Expression *>> args)
    : type_(fn_type),
      args_(std::move(args)),
      num_regs_(static_cast<i32>(type_->input.size() + type_->output.size())),
      mod_(mod) {
  // Set the references for arguments and returns
  for (i32 i = -static_cast<i32>(type_->output.size());
       i < static_cast<i32>(type_->input.size()); ++i) {
    references_[Register{i}];
  }

  auto arch  = Architecture::InterprettingMachine();
  i32 i = 0;
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

base::unordered_map<const BasicBlock *, std::unordered_set<const BasicBlock *>>
Func::GetIncomingBlocks() const {
  base::unordered_map<const BasicBlock *, std::unordered_set<const BasicBlock *>>
      incoming;
  for (const auto &b : blocks_) {
    ASSERT(b.cmds_.size() > 0u);
    const auto &last = b.cmds_.back();
    switch (last.op_code_) {
    case Op::UncondJump:
      incoming[&block(last.uncond_jump_.block_)].insert(&b);
      break;
    case Op::CondJump:
      incoming[&block(last.cond_jump_.blocks_[0])].insert(&b);
      incoming[&block(last.cond_jump_.blocks_[1])].insert(&b);
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

static base::vector<std::pair<IR::Func, prop::PropertyMap>> InvariantsFor(
    IR::Func *fn, const base::vector<AST::Expression *> &exprs) {
  base::vector<std::pair<IR::Func, prop::PropertyMap>> result;
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
  auto prop_map = prop::PropertyMap(this);

  for (const auto &block : blocks_) {
    for (const auto &cmd : block.cmds_) {
      if (cmd.op_code_ != Op::Call) { continue; }
      // If it's a register it isn't known at compile time and therefore is not
      // allowed to have preconditions. If it's a foreign function we also don't
      // allow preconditions. This can be handled correctly by declaring the
      // foreign function locally and wrapping it.
      if (cmd.call_.which_active_ != 0x01) { continue; }

      for (const auto & [ precond, precond_prop_map ] :
           cmd.call_.fn_->preconditions_) {
        auto prop_copy = precond_prop_map.with_args(*cmd.call_.long_args_,
                                                    prop_map.view_.at(&block));

        prop::DefaultProperty<bool> prop = prop_copy.Returns();
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
}

void Func::dump() const {
  std::cerr << name() << ": " << type_->to_string();
  for (size_t i = 0; i < blocks_.size(); ++i) {
    std::cerr << "\n block #" << i << std::endl;
    blocks_[i].dump(2);
  }
}

std::string Func::name() const {
  std::stringstream ss;
  ss << this;
  return ss.str();
}

}  // namespace IR
