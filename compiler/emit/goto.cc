#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {
namespace {

void EmitJump(Compiler &c, absl::Span<ast::JumpOption const> options) {
  std::vector<std::string_view> names;
  names.reserve(options.size());

  std::vector<ir::BasicBlock *> blocks;
  blocks.reserve(options.size());

  std::vector<core::Arguments<std::pair<ir::Value, type::QualType>>> args;
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

    // Note, jumps arguments cannot be completely evaluated with EmitValue
    // because they eventually need to be "prepared" before being passed to an
    // overload set on entry into a block node. This requires some implicit
    // casts which may only want to look at references rather than values, and
    // an extra load instruction will lose access to the reference we care
    // about. Thus, rather than emitting the values here, we emit references if
    // the qualified type is itself a reference and values otherwise.
    args.push_back(opt.args().Transform([&c](auto const &expr) {
      auto qt = c.context().qual_types(expr.get())[0];
      return std::pair<ir::Value, type::QualType>(
          qt.quals() >= type::Quals::Ref() ? ir::Value(c.EmitRef(expr.get()))
                                           : c.EmitValue(expr.get()),
          qt);
    }));
    c.builder().JumpExitJump(std::string(opt.block()), current_block);
  }

  c.builder().CurrentBlock() = current_block;

  // TODO: MakeAllDestructions
  c.builder().FinishTemporariesWith(
      [&c](type::Typed<ir::Reg> r) { c.EmitDestroy(r); });

  c.builder().block_termination_state() =
      ir::Builder::BlockTerminationState::kReturn;
  c.builder().ChooseJump(std::move(names), std::move(blocks), std::move(args));
}

}  // namespace

void Compiler::EmitToBuffer(ast::ConditionalGoto const *node,
                            base::untyped_buffer &) {
  auto condition    = EmitValue(node->condition());
  auto *true_block  = builder().AddBlock("ConditionalGoto-true");
  auto *false_block = builder().AddBlock("ConditionalGoto-false");
  builder().CondJump(condition.get<ir::RegOr<bool>>(), true_block, false_block);

  builder().CurrentBlock() = true_block;
  EmitJump(*this, node->true_options());

  builder().CurrentBlock() = false_block;
  EmitJump(*this, node->false_options());
}

void Compiler::EmitToBuffer(ast::UnconditionalGoto const *node,
                            base::untyped_buffer &) {
  LOG("Goto", "Emit %s", node->DebugString());
  auto *block = builder().AddBlock("UncoditionalGoto");
  builder().UncondJump(block);

  builder().CurrentBlock() = block;
  EmitJump(*this, node->options());
}

}  // namespace compiler
