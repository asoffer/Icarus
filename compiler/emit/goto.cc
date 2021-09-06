#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {
namespace {

void EmitJump(Compiler &c, absl::Span<ast::JumpOption const> options) {
  std::vector<std::string_view> names;
  names.reserve(options.size());

  std::vector<ir::BasicBlock *> blocks;
  blocks.reserve(options.size());

  std::vector<ir::Arguments> all_args;
  blocks.reserve(options.size());

  auto current_block = c.builder().CurrentBlock();

  LOG("EmitJump", "Emitting options...");
  for (auto const &opt : options) {
    core::Arguments<type::QualType> argument_types;

    ir::BasicBlock *block = c.builder().AddBlock(
        absl::StrCat("Args computation for block `", opt.block(), "`."));

    LOG("EmitJump", "... %s (%p)", opt.block(), block);

    c.builder().CurrentBlock() = block;

    // Note, jumps arguments cannot be completely evaluated with EmitToBuffer
    // because they eventually need to be "prepared" before being passed to an
    // overload set on entry into a block node. This requires some implicit
    // casts which may only want to look at references rather than values, and
    // an extra load instruction will lose access to the reference we care
    // about. Thus, rather than emitting the values here, we emit references if
    // the qualified type is itself a reference and values otherwise.
    ir::Arguments arguments;
    for (auto const &expr : opt.args().pos()) {
      auto qt = c.context().qual_types(expr.get())[0];
      argument_types.pos_emplace(qt);
      if (qt.quals() >= type::Quals::Ref()) {
        arguments.pos_insert(c.EmitRef(expr.get()));
      } else {
        size_t size = arguments.buffer().num_entries();
        c.EmitToBuffer(expr.get(), arguments.buffer());
        arguments.pos_set_in_place(size);
      }
    }

    for (auto const &[name, expr] : opt.args().named()) {
      auto qt = c.context().qual_types(expr.get())[0];
      argument_types.named_emplace(name, qt);
      if (qt.quals() >= type::Quals::Ref()) {
        arguments.named_insert(name, c.EmitRef(expr.get()));
      } else {
        size_t size = arguments.buffer().num_entries();
        c.EmitToBuffer(expr.get(), arguments.buffer());
        arguments.named_set_in_place(name, size);
      }
    }

    blocks.push_back(block);
    names.push_back(opt.block());
    all_args.emplace_back(std::move(arguments));

    c.builder().CurrentBlock()->set_jump(ir::JumpCmd::JumpExit(
        std::string(opt.block()), current_block, std::move(argument_types)));
  }

  c.builder().CurrentBlock() = current_block;

  // TODO: MakeAllDestructions
  c.builder().FinishTemporariesWith(
      [&c](type::Typed<ir::Reg> r) { c.EmitDestroy(r); });

  c.builder().block_termination_state() =
      ir::Builder::BlockTerminationState::kReturn;
  c.builder().CurrentBlock()->set_jump(ir::JumpCmd::Choose(
      std::move(names), std::move(blocks), std::move(all_args)));
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
