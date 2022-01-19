#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"

namespace compiler {
namespace {

void EmitConstantIf(Compiler &c, ast::IfStmt const *node,
                    ir::PartialResultBuffer &out) {
  if (*c.EvaluateOrDiagnoseAs<bool>(&node->condition())) {
    EmitIrForStatements(c, &node->true_scope(), node->true_block());
  } else if (node->has_false_block()) {
    EmitIrForStatements(c, &node->false_scope(), node->false_block());
  }
}

void EmitNonConstantIf(Compiler &c, ast::IfStmt const *node,
                       ir::PartialResultBuffer &out) {
  auto *true_block = c.current().subroutine->AppendBlock();
  auto *false_block =
      node->has_false_block() ? c.current().subroutine->AppendBlock() : nullptr;
  auto *landing =
      c.state().scaffolding.back().landing_blocks.at(&node->true_scope());

  ir::RegOr<bool> condition =
      EmitCast(c, c.context().typed(&node->condition()), type::Bool)
          .back()
          .get<bool>();
  c.DestroyTemporaries();
  c.current_block()->set_jump(ir::JumpCmd::Cond(
      condition, true_block, false_block ? false_block : landing));
  c.current_block() = true_block;
  EmitIrForStatements(c, &node->true_scope(), node->true_block());

  if (not node->has_false_block()) { return; }

  c.current_block() = false_block;
  EmitIrForStatements(c, &node->false_scope(), node->false_block());
  c.current_block()->set_jump(ir::JumpCmd::Uncond(landing));
  c.current_block() = landing;
}

}  // namespace

void Compiler::EmitToBuffer(ast::Module const *node,
                            ir::PartialResultBuffer &out) {
  NOT_YET();
}

void Compiler::EmitToBuffer(ast::ArgumentType const *node,
                            ir::PartialResultBuffer &out) {
  out.append(context().arg_type(node->name()));
}

void Compiler::EmitToBuffer(ast::BuiltinFn const *node,
                            ir::PartialResultBuffer &out) {
  out.append(ir::Fn(node->value()));
}

void Compiler::EmitToBuffer(ast::Import const *node,
                            ir::PartialResultBuffer &out) {
  context().LoadConstant(node, out);
}

void Compiler::EmitToBuffer(ast::Label const *node,
                            ir::PartialResultBuffer &out) {
  out.append(node->value());
}

void Compiler::EmitToBuffer(ast::IfStmt const *node,
                            ir::PartialResultBuffer &out) {
  if (node->hashtags.contains(ir::Hashtag::Const)) {
    EmitConstantIf(*this, node, out);
  } else {
    EmitNonConstantIf(*this, node, out);
  }
}

void Compiler::EmitToBuffer(ast::WhileStmt const *node,
                            ir::PartialResultBuffer &out) {
  auto *start_block = current().subroutine->AppendBlock();
  auto *body_block  = current().subroutine->AppendBlock();
  auto *landing     = current().subroutine->AppendBlock();

  current_block()->set_jump(ir::JumpCmd::Uncond(start_block));

  current_block() = start_block;
  ir::RegOr<bool> condition =
      EmitCast(*this, context().typed(&node->condition()), type::Bool)
          .back()
          .get<bool>();
  DestroyTemporaries();
  current_block()->set_jump(ir::JumpCmd::Cond(condition, body_block, landing));

  current_block() = body_block;
  EmitIrForStatements(*this, &node->body_scope(), node->body());
  current_block()->set_jump(ir::JumpCmd::Uncond(start_block));

  current_block() = landing;
}

}  // namespace compiler
