#include "ast/ast.h"
#include "compiler/compiler.h"
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
  ir::PartialResultBuffer buffer;
  for (auto const *stmt : node->true_block()) {
    buffer.clear();
    EmitToBuffer(stmt, buffer);
  }
  builder().UncondJump(landing);

  if (node->has_false_block()) {
    builder().CurrentBlock() = false_block;
    for (auto const *stmt : node->false_block()) {
      buffer.clear();
      EmitToBuffer(stmt, buffer);
    }
    builder().UncondJump(landing);
  }

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
  ir::PartialResultBuffer buffer;
  for (auto const *stmt : node->body()) {
    buffer.clear();
    EmitToBuffer(stmt, buffer);
  }
  builder().UncondJump(start_block);

  builder().CurrentBlock() = landing;
}

}  // namespace compiler
