#include "ir/builder.h"

#include <memory>

#include "absl/strings/str_cat.h"
#include "base/traverse.h"
#include "ir/blocks/group.h"
#include "type/array.h"

namespace ir {
namespace {

struct MappedBlock {
  MappedBlock(BasicBlock *p = nullptr) : data_(reinterpret_cast<uintptr_t>(p)) {}
  void visit() { data_ |= uintptr_t{1}; }
  bool seen() const { return data_ & uintptr_t{1}; }
  BasicBlock *operator->() { return get(); }
  BasicBlock &operator*() & { return *get(); }

  BasicBlock *get() {
    return reinterpret_cast<BasicBlock *>(data_ & ~uintptr_t{1});
  }
 private:
  uintptr_t data_;
};

// If the type `t` is not big, creates a new register referencing the value (or
// register) held in `value`. If `t` is big, `value` is either another register
// or the address of the big value and a new register referencing that address
// (or register) is created.
ir::Reg RegisterReferencing(ir::Builder &builder, type::Type t,
                            ir::PartialResultRef const &value) {
  if (t.is_big()) {
    return builder.CurrentBlock()->Append(ir::RegisterInstruction<ir::addr_t>{
        .operand = value.get<ir::addr_t>(),
        .result  = builder.CurrentGroup()->Reserve(),
    });
  } else {
    if (auto const *p = t.if_as<type::Primitive>()) {
      p->Apply([&]<typename T>() {
        return builder.CurrentBlock()->Append(ir::RegisterInstruction<T>{
            .operand = value.get<T>(),
            .result  = builder.CurrentGroup()->Reserve(),
        });
      });
    }
    NOT_YET();
  }
}

}  // namespace

BasicBlock *Builder::AddBlock() { return CurrentGroup()->AppendBlock(); }
BasicBlock *Builder::AddBlock(std::string header) {
  return CurrentGroup()->AppendBlock(BasicBlock::DebugInfo{
      .header = std::move(header),
  });
}

BasicBlock *Builder::AddBlock(BasicBlock const &to_copy) {
  return CurrentGroup()->AppendBlock(to_copy);
}

SetCurrent::SetCurrent(internal::BlockGroupBase &group, Builder &builder)
    : builder_(builder),
      old_group_(builder_.CurrentGroup()),
      old_block_(builder_.CurrentBlock()),
      old_termination_state_(builder_.current_.block_termination_state_) {
  builder_.CurrentGroup()  = &group;
  builder_.current_.block_ = group.entry();
  builder_.current_.block_termination_state_ =
      Builder::BlockTerminationState::kMoreStatements;
}

SetCurrent::~SetCurrent() {
  builder_.CurrentGroup()                    = old_group_;
  builder_.CurrentBlock()                    = old_block_;
  builder_.current_.block_termination_state_ = old_termination_state_;
}

Reg Builder::Alloca(type::Type t) { return CurrentGroup()->Alloca(t); }

Reg Builder::TmpAlloca(type::Type t) {
  auto reg = Alloca(t);
  current_.temporaries_to_destroy_.emplace_back(reg, t);
  return reg;
}

ir::OutParams Builder::OutParams(
    absl::Span<type::Type const> types,
    absl::Span<type::Typed<ir::RegOr<addr_t>> const> to) {
  std::vector<Reg> regs;
  regs.reserve(types.size());
  for (size_t i = 0; i < types.size(); ++i) {
    regs.push_back(types[i].get()->is_big()
                       ? (to.empty() ? TmpAlloca(types[i]) : to[i]->reg())
                       : CurrentGroup()->Reserve());
  }
  return ir::OutParams(std::move(regs));
}

void Builder::Call(RegOr<Fn> const &fn, type::Function const *f,
                   PartialResultBuffer args, ir::OutParams outs) {
  ASSERT(args.num_entries() == f->params().size());

  // TODO: this call should return the constructed registers rather than forcing
  // the caller to do it.
  for (auto const &p : f->params()) {
    if (auto const ptr = p.value.type().if_as<type::Pointer>()) {
      if (auto const *prim = ptr->pointee().if_as<type::Primitive>()) {
        CurrentBlock()->load_store_cache().clear(prim->meta());
      } else {
        CurrentBlock()->load_store_cache().clear();
        break;
      }
    }
  }

  CurrentBlock()->Append(
      CallInstruction(f, fn, std::move(args), std::move(outs)));
}

static void ClearJumps(JumpCmd const &jump, BasicBlock *from) {
  jump.Visit([&](auto &j) {
    using type = std::decay_t<decltype(j)>;
    using base::stringify;
    if constexpr (std::is_same_v<type, JumpCmd::UncondJump>) {
      j.block->erase_incoming(from);
    } else if constexpr (std::is_same_v<type, JumpCmd::CondJump>) {
      j.true_block->erase_incoming(from);
      j.false_block->erase_incoming(from);
    }
  });
}

void Builder::UncondJump(BasicBlock *block) {
  ClearJumps(CurrentBlock()->jump(), CurrentBlock());
  block->insert_incoming(CurrentBlock());
  CurrentBlock()->set_jump(JumpCmd::Uncond(block));
}

void Builder::ReturnJump() {
  block_termination_state() = BlockTerminationState::kReturn;
  CurrentBlock()->set_jump(JumpCmd::Return());
}

void Builder::CondJump(RegOr<bool> cond, BasicBlock *true_block,
                       BasicBlock *false_block) {
  ClearJumps(CurrentBlock()->jump(), CurrentBlock());
  if (cond.is_reg()) {
    true_block->insert_incoming(CurrentBlock());
    false_block->insert_incoming(CurrentBlock());
    CurrentBlock()->set_jump(
        JumpCmd::Cond(cond.reg(), true_block, false_block));
  } else {
    return UncondJump(cond.value() ? true_block : false_block);
  }
}

void Builder::Move(type::Typed<RegOr<addr_t>> to, type::Typed<Reg> from) {
  CurrentBlock()->Append(
      ir::MoveInstruction{.type = to.type(), .from = *from, .to = *to});
}

void Builder::Copy(type::Typed<RegOr<addr_t>> to, type::Typed<Reg> from) {
  CurrentBlock()->Append(
      ir::CopyInstruction{.type = to.type(), .from = *from, .to = *to});
}

Reg Builder::Align(RegOr<type::Type> r) {
  return CurrentBlock()->Append(
      TypeInfoInstruction{.kind   = TypeInfoInstruction::Kind::Alignment,
                          .type   = r,
                          .result = CurrentGroup()->Reserve()});
}

Reg Builder::Bytes(RegOr<type::Type> r) {
  return CurrentBlock()->Append(
      TypeInfoInstruction{.kind   = TypeInfoInstruction::Kind::Bytes,
                          .type   = r,
                          .result = CurrentGroup()->Reserve()});
}

Reg Builder::PtrIncr(RegOr<addr_t> ptr, RegOr<int64_t> inc,
                     type::Pointer const *t) {
  auto &cache = CurrentBlock()->offset_cache();
  if (auto result = cache.get(ptr, inc, OffsetCache::Kind::Passed)) {
    return *result;
  }
  Reg result = CurrentGroup()->Reserve();
  cache.set(ptr, inc, OffsetCache::Kind::Passed, result);
  return CurrentBlock()->Append(PtrIncrInstruction{
      .addr = ptr, .index = inc, .ptr = t, .result = result});
}

type::Typed<Reg> Builder::FieldRef(RegOr<addr_t> r, type::Struct const *t,
                                   int64_t n) {
  auto &cache = CurrentBlock()->offset_cache();
  if (auto result = cache.get(r, n, OffsetCache::Kind::Into)) {
    return type::Typed<Reg>(*result, t->fields()[n].type);
  }
  Reg result = CurrentGroup()->Reserve();
  cache.set(r, n, OffsetCache::Kind::Into, result);
  CurrentBlock()->Append(StructIndexInstruction{
      .addr = r, .index = n, .struct_type = t, .result = result});
  return type::Typed<Reg>(result, t->fields()[n].type);
}

void Builder::MakeBlock(Block block, std::vector<RegOr<Fn>> befores,
                        std::vector<RegOr<Jump>> afters) {
  MakeBlockInstruction inst{.block   = block,
                            .befores = std::move(befores),
                            .afters  = std::move(afters)};
  CurrentBlock()->Append(std::move(inst));
}

void Builder::MakeScope(Scope scope, std::vector<RegOr<Jump>> inits,
                        std::vector<RegOr<Fn>> dones,
                        absl::flat_hash_map<std::string_view, Block> blocks) {
  MakeScopeInstruction inst{.scope  = scope,
                            .inits  = std::move(inits),
                            .dones  = std::move(dones),
                            .blocks = std::move(blocks)};
  CurrentBlock()->Append(std::move(inst));
}

void Builder::InlineJumpIntoCurrent(
    Jump to_be_inlined, PartialResultBuffer const &arguments,
    absl::flat_hash_map<std::string_view, BasicBlock *> const &names) {
  auto const *jump           = CompiledJump::From(to_be_inlined);
  auto *start_block          = CurrentBlock();
  size_t inlined_start_index = CurrentGroup()->blocks().size();

  auto *into = CurrentGroup();

  LOG("", "%s", *CurrentGroup());

  CurrentBlock() = start_block;
  size_t i       = 0;
  if (type::Type state_type = jump->type()->state()) {
    RegisterReferencing(*this, state_type, arguments[i++]);
  }

  for (auto const &p : jump->type()->params()) {
    RegisterReferencing(*this, p.value, arguments[i++]);
  }

  // Update the register count. This must be done after we've added the
  // register-forwarding instructions which use this count to choose a register
  // number.
  Inliner inliner(into->num_regs(), jump->num_args());
  into->MergeAllocationsFrom(*jump, inliner);

  absl::flat_hash_map<BasicBlock const *, Arguments const *>
      choose_argument_cache;

  absl::flat_hash_map<BasicBlock const *, MappedBlock> rename_map;
  // TODO: DebugInfo
  rename_map.try_emplace(jump->entry(), AddBlock(*jump->entry()));

  std::queue<BasicBlock const *> to_process;
  to_process.push(jump->entry());

  while (not to_process.empty()) {
    auto const *block = to_process.front();
    to_process.pop();

    auto &mapped_block = rename_map.at(block);
    if (mapped_block.seen()) { continue; }
    mapped_block.visit();

    base::Traverse(inliner, *mapped_block);
    LOG("", "Traversing %p => %p\n%s", block, mapped_block.get(), *block);
    mapped_block->jump().Visit([&](auto &j) {
      constexpr auto type = base::meta<std::decay_t<decltype(j)>>;
      if constexpr (type == base::meta<JumpCmd::JumpExitJump>) {
        auto *b        = names.at(j.name);
        CurrentBlock() = mapped_block.get();
        auto arguments =
            choose_argument_cache.at(rename_map.at(j.choose_block).get());
        base::Traverse(inliner, arguments);

        // TODO: Call(___, ____, arguments.buffer(), ___);
        LOG("", "Jumping to preexisting %p => %p", mapped_block.get(), b);
        CurrentBlock()->set_jump(JumpCmd::Uncond(b));

      } else if constexpr (type == base::meta<JumpCmd::UncondJump>) {
        auto [iter, inserted] = rename_map.try_emplace(j.block);
        if (inserted) {
          to_process.push(j.block);
          iter->second = AddBlock(*j.block);  // TODO: DebugInfo
        }
        j.block = iter->second.get();
        j.block->insert_incoming(mapped_block.get());
      } else if constexpr (type == base::meta<JumpCmd::CondJump>) {
        auto [true_iter, true_inserted] = rename_map.try_emplace(j.true_block);
        if (true_inserted) {
          to_process.push(j.true_block);
          true_iter->second = AddBlock(*j.true_block);  // TODO: DebugInfo
        }

        j.true_block = rename_map.at(true_iter->second.get()).get();
        j.true_block->insert_incoming(mapped_block.get());

        auto [false_iter, false_inserted] =
            rename_map.try_emplace(j.false_block);
        if (false_inserted) {
          to_process.push(j.false_block);
          false_iter->second = AddBlock(*j.false_block);  // TODO: DebugInfo
        }

        j.false_block = rename_map.at(false_iter->second.get()).get();
        j.false_block->insert_incoming(mapped_block.get());
      } else if constexpr (type == base::meta<JumpCmd::ChooseJump>) {
        size_t index = std::distance(
            j.names().begin(), std::find_if(j.names().begin(), j.names().end(),
                                            [&](std::string_view name) {
                                              return names.contains(name);
                                            }));
        BasicBlock const *b = j.blocks()[index];
        // TODO: DebugInfo
        auto [iter, inserted] =
            rename_map.try_emplace(b, AddBlock(*b));
        ASSERT(inserted == true);
        to_process.push(b);

        choose_argument_cache.emplace(mapped_block.get(), &j.args()[index]);
        mapped_block->set_jump(JumpCmd::Uncond(iter->second.get()));
      } else {
        UNREACHABLE(*block, *CurrentGroup());
      }
    });
  }

  start_block->set_jump(JumpCmd::Uncond(rename_map.at(jump->entry()).get()));
}

}  // namespace ir
