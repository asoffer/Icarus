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
  auto *true_block  = c.builder().CurrentGroup()->AppendBlock();
  auto *false_block = node->has_false_block()
                          ? c.builder().CurrentGroup()->AppendBlock()
                          : nullptr;
  auto *landing = c.builder().landing(&node->true_scope());

  ir::RegOr<bool> condition = c.EmitWithCastTo<bool>(
      c.context().qual_types(&node->condition())[0].type(), &node->condition());
  c.builder().CondJump(condition, true_block,
                     false_block ? false_block : landing);
  c.DestroyTemporaries();

  c.builder().CurrentBlock() = true_block;
  EmitIrForStatements(c, &node->true_scope(), node->true_block());

  if (not node->has_false_block()) { return; }

  c.builder().CurrentBlock() = false_block;
  EmitIrForStatements(c, &node->false_scope(), node->false_block());
  c.builder().UncondJump(landing);
  c.builder().CurrentBlock() = landing;
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
  auto *start_block = builder().CurrentGroup()->AppendBlock();
  auto *body_block  = builder().CurrentGroup()->AppendBlock();
  auto *landing     = builder().CurrentGroup()->AppendBlock();

  builder().UncondJump(start_block);

  builder().CurrentBlock()  = start_block;
  ir::RegOr<bool> condition = EmitWithCastTo<bool>(
      context().qual_types(&node->condition())[0].type(), &node->condition());
  DestroyTemporaries();

  builder().CondJump(condition, body_block, landing);

  builder().CurrentBlock() = body_block;
  EmitIrForStatements(*this, &node->body_scope(), node->body());
  builder().UncondJump(start_block);

  builder().CurrentBlock() = landing;
}

}  // namespace compiler
