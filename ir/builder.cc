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
  if (t.is_big() or t.is<type::Pointer>()) {
    return builder.CurrentBlock()->Append(ir::RegisterInstruction<ir::addr_t>{
        .operand = value.get<ir::addr_t>(),
        .result  = builder.CurrentGroup()->Reserve(),
    });
  } else {
    if (auto const *p = t.if_as<type::Primitive>()) {
      return p->Apply([&]<typename T>() {
        return builder.CurrentBlock()->Append(ir::RegisterInstruction<T>{
            .operand = value.get<T>(),
            .result  = builder.CurrentGroup()->Reserve(),
        });
      });
    } else {
      NOT_YET(t);
    }
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

void Builder::InlineJumpIntoCurrent(Jump to_be_inlined,
                                    PartialResultBuffer const &arguments,
                                    ScopeState const &state) {
  auto const *jump           = CompiledJump::From(to_be_inlined);
  auto *start_block          = CurrentBlock();
  size_t inlined_start_index = CurrentGroup()->blocks().size();

  auto *into = CurrentGroup();
  CurrentBlock() = start_block;

  // Update the register count. This must be done after we've added the
  // register-forwarding instructions which use this count to choose a register
  // number.
  Inliner inliner(into->num_regs(), jump->num_args());

  size_t i = 0;
  if (type::Type state_type = jump->type()->state()) {
    RegisterReferencing(*this, state_type, arguments[i++]);
  }

  for (auto const &p : jump->type()->params()) {
    RegisterReferencing(*this, p.value.type(), arguments[i++]);
  }

  into->MergeAllocationsFrom(*jump, inliner);

  absl::flat_hash_map<BasicBlock const *, Arguments> choose_argument_cache;

  absl::node_hash_map<BasicBlock const *, MappedBlock> rename_map;
  // TODO: DebugInfo
  rename_map.try_emplace(jump->entry(), AddBlock(*jump->entry()));

  std::queue<BasicBlock const *> to_process;
  to_process.push(jump->entry());

  auto try_enqueue = [&](ir::BasicBlock *&b) {
    auto [iter, inserted] = rename_map.try_emplace(b);
    if (inserted) {
      to_process.push(b);
      iter->second = AddBlock(*b);  // TODO: DebugInfo
    }
    b = iter->second.get();
  };

  while (not to_process.empty()) {
    auto const *block = to_process.front();
    to_process.pop();

    auto &mapped_block = rename_map.at(block);
    if (mapped_block.seen()) { continue; }
    mapped_block.visit();
    base::Traverse(inliner, *mapped_block);
    mapped_block->jump().Visit([&](auto &j) {
      constexpr auto type = base::meta<std::decay_t<decltype(j)>>;
      if constexpr (type == base::meta<JumpCmd::JumpExitJump>) {
        auto *b        = state.names.at(j.name);
        CurrentBlock() = mapped_block.get();

        auto arguments = choose_argument_cache.at(j.choose_block);
        ASSERT(arguments.size() == j.argument_types.size());
        base::Traverse(inliner, arguments);

        auto &s = *ir::CompiledScope::From(state.scope);
        // clang-format off
        auto *overload_set =
              (j.name == "done") ? &s.exit()
            : (j.name == "start") ? nullptr
            : &ir::CompiledBlock::From(s.block(j.name))->before();
        // clang-format on
        if (overload_set) {
          auto overload = overload_set->Lookup(j.argument_types);
          ASSERT(overload.has_value() == true);

          size_t i = 0;
          Arguments prepared_arguments;
          for (auto const &param : overload->type()->params()) {
            PartialResultBuffer buffer;
            if (i < j.argument_types.pos().size()) {
              buffer.append(arguments[i]);
              // TODO: Should be taking arg by qual_type.
              if (j.argument_types[i].quals() >= type::Quals::Ref()) {
                RegOr<addr_t> addr = buffer.get<addr_t>(0);
                buffer.clear();
                buffer.append(PtrFix(addr, j.argument_types[i].type()));
              }
              ApplyImplicitCasts(j.argument_types[i].type(), param.value,
                                 buffer);
              prepared_arguments.pos_insert(buffer[0]);
            } else {
              buffer.append(arguments[param.name]);
              // TODO: Should be taking arg by qual_type.
              if (j.argument_types[param.name].quals() >= type::Quals::Ref()) {
                RegOr<addr_t> addr = buffer.get<addr_t>(0);
                buffer.clear();
                Load(addr, j.argument_types[param.name].type(), buffer);
              }
              ApplyImplicitCasts(j.argument_types[param.name].type(),
                                 param.value, buffer);
              prepared_arguments.named_insert(param.name, buffer[0]);
            }
            ++i;
          }

          auto out_params = OutParams(overload->type()->return_types());
          Call(*overload, overload->type(), prepared_arguments.buffer(),
               out_params);
          if (not out_params.empty()) {
            auto &phis = state.set_phis.at(j.name);
            ASSERT(phis.size() == out_params.size());
            auto phi_iter = phis.begin();
            for (ir::Reg out : out_params) {
              (*phi_iter)(mapped_block.get(), out);
              ++phi_iter;
            }
          }
        }

        CurrentBlock()->set_jump(JumpCmd::Uncond(b));

      } else if constexpr (type == base::meta<JumpCmd::UncondJump>) {
        try_enqueue(j.block);
      } else if constexpr (type == base::meta<JumpCmd::CondJump>) {
        auto old = j.reg;
        base::Traverse(inliner, j.reg);
        try_enqueue(j.true_block);
        try_enqueue(j.false_block);
      } else if constexpr (type == base::meta<JumpCmd::ChooseJump>) {
        size_t index = std::distance(
            j.names().begin(), std::find_if(j.names().begin(), j.names().end(),
                                            [&](std::string_view name) {
                                              return state.names.contains(name);
                                            }));
        BasicBlock const *b   = j.blocks()[index];
        auto [iter, inserted] = rename_map.try_emplace(b);
        ASSERT(inserted == true);
        to_process.push(b);
        iter->second = AddBlock(*b);  // TODO: DebugInfo
        b            = iter->second.get();
        choose_argument_cache.emplace(
            block, std::move(std::move(j).arguments()[index]));
        mapped_block->set_jump(JumpCmd::Uncond(iter->second.get()));
      } else {
        UNREACHABLE(*block, *CurrentGroup());
      }
    });
  }

  start_block->set_jump(JumpCmd::Uncond(rename_map.at(jump->entry()).get()));
}

void Builder::ApplyImplicitCasts(type::Type from, type::QualType to,
                                 PartialResultBuffer &buffer) {
  ASSERT(type::CanCastImplicitly(from, to.type()) == true);
  if (from == to.type()) { return; }
  if (from.is<type::Slice>() and to.type().is<type::Slice>()) { return; }

  auto const *bufptr_from_type = from.if_as<type::BufferPointer>();
  auto const *ptr_to_type      = to.type().if_as<type::Pointer>();
  if (bufptr_from_type and ptr_to_type and
      type::CanCastImplicitly(bufptr_from_type, ptr_to_type)) {
    return;
  }

  if (from == type::Integer and type::IsIntegral(to.type())) {
    to.type().as<type::Primitive>().Apply([&]<typename T>() {
      if constexpr (std::is_integral_v<T>) {
        RegOr<T> result = Cast<Integer, T>(buffer.back().get<Integer>());
        buffer.pop_back();
        buffer.append(result);
      } else {
        UNREACHABLE(typeid(T).name());
      }
    });
  }
}

}  // namespace ir
