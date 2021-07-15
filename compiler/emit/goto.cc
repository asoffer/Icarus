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

    // Note, jumps arguments cannot be completely evaluated with EmitToBuffer
    // because they eventually need to be "prepared" before being passed to an
    // overload set on entry into a block node. This requires some implicit
    // casts which may only want to look at references rather than values, and
    // an extra load instruction will lose access to the reference we care
    // about. Thus, rather than emitting the values here, we emit references if
    // the qualified type is itself a reference and values otherwise.
    args.push_back(opt.args().Transform([&c](auto const &expr) {
      auto qt = c.context().qual_types(expr.get())[0];
      if (qt.quals() >= type::Quals::Ref()) {
        return std::pair<ir::Value, type::QualType>(
            ir::Value(c.EmitRef(expr.get())), qt);
      } else {
        ir::PartialResultBuffer buffer;
        c.EmitToBuffer(expr.get(), buffer);
        return std::pair<ir::Value, type::QualType>(
            ToValue(buffer[0].raw(), qt.type()), qt);
      }
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
                            ir::PartialResultBuffer &) {
  auto condition    = EmitAs<bool>(node->condition());
  auto *true_block  = builder().AddBlock("ConditionalGoto-true");
  auto *false_block = builder().AddBlock("ConditionalGoto-false");
  builder().CondJump(condition, true_block, false_block);

  builder().CurrentBlock() = true_block;
  EmitJump(*this, node->true_options());

  builder().CurrentBlock() = false_block;
  EmitJump(*this, node->false_options());
}

void Compiler::EmitToBuffer(ast::UnconditionalGoto const *node,
                            ir::PartialResultBuffer &) {
  LOG("Goto", "Emit %s", node->DebugString());
  auto *block = builder().AddBlock("UncoditionalGoto");
  builder().UncondJump(block);

  builder().CurrentBlock() = block;
  EmitJump(*this, node->options());
}

}  // namespace compiler
