#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "ir/value/module_id.h"

namespace compiler {

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
    if (*EvaluateOrDiagnoseAs<bool>(&node->condition())) {
      ir::PartialResultBuffer buffer;
      for (auto const *stmt : node->true_block()) {
        buffer.clear();
        EmitToBuffer(stmt, buffer);
      }

      MakeAllDestructions(*this, &node->true_scope());
    } else if (node->has_false_block()) {
      ir::PartialResultBuffer buffer;
      for (auto const *stmt : node->false_block()) {
        buffer.clear();
        EmitToBuffer(stmt, buffer);
      }

      MakeAllDestructions(*this, &node->false_scope());
    }
  } else {
    auto *true_block  = builder().CurrentGroup()->AppendBlock();
    auto *false_block = node->has_false_block()
                            ? builder().CurrentGroup()->AppendBlock()
                            : nullptr;
    auto *landing = builder().CurrentGroup()->AppendBlock();

    ir::RegOr<bool> condition = EmitAs<bool>(&node->condition());
    builder().CondJump(condition, true_block,
                       false_block ? false_block : landing);

    builder().CurrentBlock() = true_block;
    ir::PartialResultBuffer buffer;
    for (auto const *stmt : node->true_block()) {
      buffer.clear();
      EmitToBuffer(stmt, buffer);
    }

    MakeAllDestructions(*this, &node->true_scope());
    builder().UncondJump(landing);

    if (node->has_false_block()) {
      builder().CurrentBlock() = false_block;
      for (auto const *stmt : node->false_block()) {
        buffer.clear();
        EmitToBuffer(stmt, buffer);
      }

      MakeAllDestructions(*this, &node->false_scope());
      builder().UncondJump(landing);
    }

    builder().CurrentBlock() = landing;
  }
}

void Compiler::EmitToBuffer(ast::WhileStmt const *node,
                            ir::PartialResultBuffer &out) {
  auto start_block = builder().CurrentGroup()->AppendBlock();
  auto body_block  = builder().CurrentGroup()->AppendBlock();
  auto landing     = builder().CurrentGroup()->AppendBlock();

  builder().UncondJump(start_block);

  builder().CurrentBlock()  = start_block;
  ir::RegOr<bool> condition = EmitAs<bool>(&node->condition());
  builder().CondJump(condition, body_block, landing);

  builder().CurrentBlock() = body_block;
  EmitIrForStatements(*this, node->body());
  MakeAllDestructions(*this, &node->body_scope());
  builder().UncondJump(start_block);
  builder().CurrentBlock() = landing;
}

}  // namespace compiler
