#include <optional>
#include <queue>
#include <string_view>
#include <type_traits>
#include <utility>

#include "absl/cleanup/cleanup.h"
#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "absl/strings/str_format.h"
#include "absl/types/span.h"
#include "ast/ast.h"
#include "base/debug.h"
#include "base/extend.h"
#include "base/extend/absl_hash.h"
#include "base/meta.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "ir/blocks/basic.h"
#include "ir/blocks/group.h"
#include "ir/compiled_scope.h"
#include "ir/instruction/core.h"
#include "ir/instruction/jump.h"
#include "ir/value/char.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "ir/value/scope.h"

namespace compiler {
namespace {

// BasicBlockMapping tracks a mapping from basic blocks in a scope being inlined
// to their corresponding created basic block in their inlined location.
struct BasicBlockMapping {
  explicit BasicBlockMapping(ir::Builder &builder, ir::Scope scope) {
    for (auto const *block : scope->blocks()) {
      auto [iter, inserted] = mapping_.try_emplace(
          block, builder.CurrentGroup()->AppendBlock(*block));
      ASSERT(inserted == true);
    }
  }

  ir::BasicBlock *operator()(ir::BasicBlock const *from) const {
    auto iter = mapping_.find(from);
    ASSERT(iter != mapping_.end());
    return iter->second;
  }

  auto begin() const { return mapping_.begin(); }
  auto end() const { return mapping_.end(); }

 private:
  absl::flat_hash_map<ir::BasicBlock const *, ir::BasicBlock *> mapping_;
};

ir::BasicBlock *InlineScope(
    Compiler &c, absl::Span<ast::BlockNode const> blocks,
    ir::Scope to_be_inlined, ir::PartialResultBuffer const &arguments,
    absl::Span<std::pair<ir::BasicBlock *, ir::BasicBlock *>>
        block_entry_exit) {
  auto landing = c.builder().CurrentGroup()->AppendBlock();

  auto *start_block          = c.builder().CurrentBlock();
  size_t inlined_start_index = c.builder().CurrentGroup()->blocks().size();
  auto *into                 = c.builder().CurrentGroup();

  // Update the register count. This must be done after we've added the
  // register-forwarding instructions which use this count to choose a register
  // number.
  ir::Inliner inliner(into->num_regs(), to_be_inlined->num_args());

  size_t j = 0;
  for (auto const &p : to_be_inlined.type()->params()) {
    RegisterReferencing(c.builder(), p.value.type(), arguments[j++]);
  }

  c.builder().CurrentBlock() = start_block;

  size_t block_index = 0;
  for (auto const &block : blocks) {
    size_t param_index            = 0;
    auto *parameter_binding_block = c.builder().CurrentGroup()->AppendBlock();
    c.builder().CurrentBlock()    = parameter_binding_block;
    for (auto const &param : block.params()) {
      ir::Reg r =
          to_be_inlined.parameters(ir::Block(block_index))[param_index++];
      inliner(r);
      auto const &id        = param.value->ids()[0];
      type::Type param_type = c.context().qual_types(&id)[0].type();
      ir::PartialResultBuffer buffer;
      buffer.append(r);
      c.EmitCopyAssign(type::Typed(c.builder().addr(&id), param_type),
                       type::Typed(buffer[0], param_type));
    }
    auto &entry = block_entry_exit[block_index].first;
    c.builder().UncondJump(entry);
    entry = parameter_binding_block;
    ++block_index;
  }

  into->MergeAllocationsFrom(*to_be_inlined, inliner);
  BasicBlockMapping mapping(c.builder(), to_be_inlined);
  for (auto [from_block, to_block] : mapping) {
    base::Traverse(inliner, *to_block);

    to_block->jump().Visit([&, jump = &to_block->jump()](auto &j) {
      constexpr auto type = base::meta<std::decay_t<decltype(j)>>;
      if constexpr (type == base::meta<ir::JumpCmd::UncondJump>) {
        j.block = mapping(j.block);
      } else if constexpr (type == base::meta<ir::JumpCmd::CondJump>) {
        base::Traverse(inliner, j.reg);
        j.true_block  = mapping(j.true_block);
        j.false_block = mapping(j.false_block);
      } else if constexpr (type == base::meta<ir::JumpCmd::RetJump>) {
        *jump = ir::JumpCmd::Uncond(landing);
      } else if constexpr (type == base::meta<ir::JumpCmd::UnreachableJump>) {
        return;
      } else if constexpr (type == base::meta<ir::JumpCmd::BlockJump>) {
        auto [entry, exit] = block_entry_exit[j.block.value()];
        auto exit_block    = mapping(to_be_inlined.connection(j.block).second);
        *jump              = ir::JumpCmd::Uncond(entry);
        c.builder().CurrentBlock() = exit;
        c.builder().UncondJump(exit_block);
      } else {
        UNREACHABLE(type);
      }
    });

  }

  start_block->set_jump(ir::JumpCmd::Uncond(mapping(to_be_inlined->entry())));
  return landing;
}

}  // namespace

void Compiler::EmitToBuffer(ast::ScopeNode const *node,
                            ir::PartialResultBuffer &out) {
  LOG("ScopeNode", "Emitting IR for ScopeNode");
  auto unbound_scope = *EvaluateOrDiagnoseAs<ir::UnboundScope>(node->name());

  ir::ScopeContext scope_context = context().ScopeContext(node);

  // Constant arguments need to be computed entirely before being used to
  // instantiate a generic function.
  ir::CompleteResultBuffer buffer;
  auto constant_arguments =
      EmitConstantArguments(*this, node->arguments(), buffer);

  ir::Scope scope =
      unbound_scope.bind(work_resources(), scope_context, constant_arguments);

  ast::ScopeLiteral const *scope_lit = unbound_scope.literal();

  auto find_subcontext_result =
      FindInstantiation(*this, scope_lit, scope_context, constant_arguments);
  auto &context = find_subcontext_result.context;

  EnsureComplete({
      .kind    = WorkItem::Kind::EmitScopeBody,
      .node    = scope_lit,
      .context = &context,
  });

  if (node->hashtags.contains(ir::Hashtag::Const)) {
    context.ir().WriteByteCode<EmitByteCode>(scope);
    auto blocks = InterpretScopeAtCompileTime(scope, constant_arguments);

    for (auto block : blocks) {
      ir::PartialResultBuffer ignored;
      EmitToBuffer(&node->blocks()[block.value()], ignored);
    }
  } else {
    ir::PartialResultBuffer argument_buffer;

    auto *start = builder().CurrentBlock();
    EmitArguments(*this, scope.type()->params(), {/* TODO: Defaults */},
                  node->arguments(), constant_arguments, argument_buffer);

    std::vector<std::pair<ir::BasicBlock *, ir::BasicBlock *>> block_entry_exit;
    block_entry_exit.reserve(node->blocks().size());

    for (auto const &block : node->blocks()) {
      auto &[entry, exit]      = block_entry_exit.emplace_back();
      entry                    = builder().CurrentGroup()->AppendBlock();
      builder().CurrentBlock() = entry;
      ir::PartialResultBuffer ignored;
      EmitToBuffer(&block, ignored);
      exit = builder().CurrentBlock();
    }

    builder().CurrentBlock() = start;

    builder().CurrentBlock() =
        InlineScope(*this, node->blocks(), scope, argument_buffer,
                    absl::MakeSpan(block_entry_exit));
  }
  LOG("ScopeNode", "%s", *builder().CurrentGroup());
}

void Compiler::EmitCopyInit(
    ast::ScopeNode const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  // TODO: Implement this properly.
  EmitCopyAssign(node, to);
}

void Compiler::EmitMoveInit(
    ast::ScopeNode const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  // TODO: Implement this properly.
  EmitMoveAssign(node, to);
}
void Compiler::EmitCopyAssign(
    ast::ScopeNode const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  // TODO: Implement this properly.
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  EmitCopyAssign(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitMoveAssign(
    ast::ScopeNode const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  // TODO: Implement this properly.
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  EmitMoveAssign(to[0], type::Typed(buffer[0], t));
}

}  // namespace compiler
