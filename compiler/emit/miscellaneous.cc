#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "ir/value/module_id.h"

namespace compiler {

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
  auto module_id = context().imported_module(node);
  ASSERT(module_id != ir::ModuleId::Invalid());
  out.append(module_id);
}

void Compiler::EmitToBuffer(ast::Label const *node,
                            ir::PartialResultBuffer &out) {
  out.append(node->value());
}

void Compiler::EmitToBuffer(ast::IfStmt const *node,
                            ir::PartialResultBuffer &out) {
  auto *true_block  = builder().AddBlock();
  auto *false_block = node->has_false_block() ? builder().AddBlock() : nullptr;
  auto *landing     = builder().AddBlock();

  ir::RegOr<bool> condition = EmitAs<bool>(&node->condition());
  builder().CondJump(condition, true_block,
                     false_block ? false_block : landing);

  builder().CurrentBlock() = true_block;
  builder().block_termination_state() =
      ir::Builder::BlockTerminationState::kMoreStatements;
  ir::PartialResultBuffer buffer;
  for (auto const *stmt : node->true_block()) {
    buffer.clear();
    EmitToBuffer(stmt, buffer);
  }

  MakeAllDestructions(*this, &node->true_scope());

  switch (builder().block_termination_state()) {
    case ir::Builder::BlockTerminationState::kMoreStatements:
    case ir::Builder::BlockTerminationState::kNoTerminator:
      builder().UncondJump(landing);
      break;
    default: break;
  }

  if (node->has_false_block()) {
    builder().CurrentBlock() = false_block;
    builder().block_termination_state() =
        ir::Builder::BlockTerminationState::kMoreStatements;
    for (auto const *stmt : node->false_block()) {
      buffer.clear();
      EmitToBuffer(stmt, buffer);
    }

    MakeAllDestructions(*this, &node->false_scope());

    switch (builder().block_termination_state()) {
      case ir::Builder::BlockTerminationState::kMoreStatements:
      case ir::Builder::BlockTerminationState::kNoTerminator:
        builder().UncondJump(landing);
        break;
      default: break;
    }
  }

  builder().block_termination_state() =
      ir::Builder::BlockTerminationState::kMoreStatements;
  builder().CurrentBlock() = landing;
}

void Compiler::EmitToBuffer(ast::WhileStmt const *node,
                            ir::PartialResultBuffer &out) {
  auto start_block = builder().AddBlock();
  auto body_block  = builder().AddBlock();
  auto landing     = builder().AddBlock();

  builder().UncondJump(start_block);

  builder().CurrentBlock()  = start_block;
  ir::RegOr<bool> condition = EmitAs<bool>(&node->condition());
  builder().CondJump(condition, body_block, landing);

  builder().CurrentBlock() = body_block;
  builder().block_termination_state() =
      ir::Builder::BlockTerminationState::kMoreStatements;
  ir::PartialResultBuffer buffer;
  for (auto const *stmt : node->body()) {
    buffer.clear();
    EmitToBuffer(stmt, buffer);
  }

  MakeAllDestructions(*this, &node->body_scope());
  switch (builder().block_termination_state()) {
    case ir::Builder::BlockTerminationState::kMoreStatements:
    case ir::Builder::BlockTerminationState::kNoTerminator:
      builder().UncondJump(start_block);
      break;
    default: break;
  }

  builder().block_termination_state() =
      ir::Builder::BlockTerminationState::kMoreStatements;
  builder().CurrentBlock() = landing;
}

}  // namespace compiler
