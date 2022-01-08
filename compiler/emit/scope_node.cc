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
#include "compiler/common.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/module.h"
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

absl::flat_hash_map<ir::BasicBlock const *, ir::BasicBlock *>
InsertUnadjustedBlocks(ir::internal::BlockGroupBase &to,
                       ir::internal::BlockGroupBase const &from) {
  absl::flat_hash_map<ir::BasicBlock const *, ir::BasicBlock *> result;
  for (auto const *block : from.blocks()) {
    auto [iter, inserted] = result.try_emplace(block, to.AppendBlock(*block));
    ASSERT(inserted == true);
  }
  return result;
}

void AdjustInstructions(
    ir::Inliner &inliner,
    absl::flat_hash_map<ir::BasicBlock const *, ir::BasicBlock *> const
        &block_mapping) {
  for (auto const &[from, to] : block_mapping) { base::Traverse(inliner, *to); }
}

ir::BasicBlock *AdjustJumpsAndEmitBlocks(
    Compiler &c, ast::ScopeNode const *node, ir::Scope scope,
    ir::Inliner &inliner,
    absl::flat_hash_map<ir::BasicBlock const *, ir::BasicBlock *> const
        &block_mapping) {
  auto *landing = c.builder().CurrentGroup()->AppendBlock();
  for (auto const &[from, to] : block_mapping) {
    to->jump().Visit([&, jump = &to->jump()](auto &j) {
      constexpr auto type = base::meta<std::decay_t<decltype(j)>>;
      if constexpr (type == base::meta<ir::JumpCmd::UncondJump>) {
        j.block = block_mapping.find(j.block)->second;
      } else if constexpr (type == base::meta<ir::JumpCmd::CondJump>) {
        base::Traverse(inliner, j.reg);
        j.true_block  = block_mapping.find(j.true_block)->second;
        j.false_block = block_mapping.find(j.false_block)->second;
      } else if constexpr (type == base::meta<ir::JumpCmd::RetJump>) {
        *jump = ir::JumpCmd::Uncond(landing);
      } else if constexpr (type == base::meta<ir::JumpCmd::UnreachableJump>) {
        return;
      } else if constexpr (type == base::meta<ir::JumpCmd::BlockJump>) {
        // Save the exit before we modify `*jump`.
        auto *exit          = block_mapping.find(j.after)->second;
        size_t block_index = j.block.value();
        auto *block_to_emit = &node->blocks()[block_index];

        auto *block_start = c.builder().CurrentGroup()->AppendBlock();
        *jump             = ir::JumpCmd::Uncond(block_start);

        c.builder().CurrentBlock() = block_start;

        type::Type t = c.context().qual_types(block_to_emit)[0].type();
        if (t.is<type::Block>()) {
          size_t param_index = 0;
          for (auto const &param : block_to_emit->params()) {
            ir::Reg r = scope.parameters(ir::Block(block_index))[param_index++];
            inliner(r);
            auto const &id        = param.value->ids()[0];
            type::Type param_type = c.context().qual_types(&id)[0].type();
            ir::PartialResultBuffer buffer;
            buffer.append(r);
            c.builder().set_addr(&id, c.builder().Alloca(param_type));
            c.EmitCopyAssign(type::Typed(c.builder().addr(&id), param_type),
                             type::Typed(buffer[0], param_type));
          }

          ir::PartialResultBuffer ignored;
          c.EmitToBuffer(block_to_emit, ignored);
          c.builder().UncondJump(exit);
        } else {
          auto const &gb = t.as<type::Generic<type::Block>>();
          // TODO: From here we need to find the instantiation, which requires
          // knowing the bound arguments and types thereof.

          //FindInstantiation
          // gb.Instantiate();
          LOG("", "%s", gb.to_string());
          NOT_YET();
        }
      } else {
        UNREACHABLE(type);
      }
    });
  }
  return landing;
}

}  // namespace

void Compiler::EmitToBuffer(ast::ScopeNode const *node,
                            ir::PartialResultBuffer &out) {
  LOG("ScopeNode", "Emitting IR for ScopeNode");

  auto const *callee = context().CallMetadata(node).resolved();
  CompilationData data{
      .context        = &ModuleFor(callee)->as<CompiledModule>().context(),
      .work_resources = work_resources(),
      .resources      = resources(),
  };
  Compiler c(&data);
  auto unbound_scope = *c.EvaluateOrDiagnoseAs<ir::UnboundScope>(callee);

  auto scope_context = context().LoadConstant<ir::ScopeContext>(node);

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

    ir::Inliner inliner(builder().CurrentGroup()->num_regs(),
                        scope->num_args());

    size_t j = 0;
    for (auto const &p : scope.type()->params()) {
      RegisterReferencing(builder(), p.value.type(), argument_buffer[j++]);
    }

    builder().CurrentGroup()->MergeAllocationsFrom(*scope, inliner);
    auto block_mapping =
        InsertUnadjustedBlocks(*builder().CurrentGroup(), *scope);
    AdjustInstructions(inliner, block_mapping);

    builder().CurrentBlock() = start;
    builder().UncondJump(block_mapping.find(scope->entry())->second);

    auto *landing =
        AdjustJumpsAndEmitBlocks(*this, node, scope, inliner, block_mapping);
    builder().CurrentBlock() = landing;
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
