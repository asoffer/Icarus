#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {
namespace {

void EmitJump(Compiler &c, absl::Span<ast::JumpOption const> options) {
  std::vector<std::string_view> names;
  names.reserve(options.size());

  std::vector<ir::BasicBlock *> blocks;
  blocks.reserve(options.size());

  std::vector<core::Arguments<type::Typed<ir::Value>>> args;
  args.reserve(options.size());

  auto current_block = c.builder().CurrentBlock();

  LOG("EmitJump", "Emitting options...");
  for (auto const &opt : options) {
    ir::BasicBlock *block = c.builder().AddBlock(
        absl::StrCat("Args computation for block `", opt.block(), "`."));

    LOG("EmitJump", "... %s (%p)", opt.block(), block);

    blocks.push_back(block);
    names.push_back(opt.block());

    c.builder().CurrentBlock() = block;

    args.push_back(opt.args().Transform([&c](auto const &expr) {
      return type::Typed<ir::Value>(c.EmitValue(expr.get()),
                                    c.type_of(expr.get()));
    }));
  }

  c.builder().CurrentBlock() = current_block;
  c.builder().ChooseJump(std::move(names), std::move(blocks), std::move(args));
}

}  // namespace

ir::Value Compiler::EmitValue(ast::ConditionalGoto const *node) {
  auto condition    = EmitValue(node->condition());
  auto *true_block  = builder().AddBlock("ConditionalGoto-true");
  auto *false_block = builder().AddBlock("ConditionalGoto-false");
  builder().CondJump(condition.get<ir::RegOr<bool>>(), true_block, false_block);

  builder().CurrentBlock() = true_block;
  EmitJump(*this, node->true_options());

  builder().CurrentBlock() = false_block;
  EmitJump(*this, node->false_options());

  return ir::Value();
}

ir::Value Compiler::EmitValue(ast::UnconditionalGoto const *node) {
  LOG("Goto", "Emit %s", node->DebugString());
  auto *block = builder().AddBlock("UncoditionalGoto");
  builder().UncondJump(block);

  builder().CurrentBlock() = block;
  EmitJump(*this, node->options());
  return ir::Value();
}

}  // namespace compiler
