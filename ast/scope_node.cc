#include "ast/scope_node.h"


#include <sstream>
#include "ast/access.h"
#include "ast/block_literal.h"
#include "ast/block_node.h"
#include "core/fn_args.h"
#include "ast/function_literal.h"
#include "ast/identifier.h"
#include "ast/scope_literal.h"
#include "backend/eval.h"
#include "base/util.h"
#include "ir/components.h"
#include "ir/compiled_fn.h"
#include "misc/context.h"
#include "core/scope.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/type.h"

namespace ast {
using ::matcher::InheritsFrom;

std::string ScopeNode::to_string(size_t n) const {
  std::stringstream ss;
  ss << name_->to_string(n) << " ";
  if (!args_.empty()) { ss << "(" << args_.to_string() << ") "; }
  for (auto const &block : blocks_) { ss << block.to_string(n); }
  return ss.str();
}

void ScopeNode::assign_scope(core::Scope *scope) {
  scope_ = scope;
  name_->assign_scope(scope);
  args_.Apply([scope](auto &expr) { expr->assign_scope(scope); });
  for (auto &block : blocks_) { block.assign_scope(scope); }
}

void ScopeNode::DependentDecls(DeclDepGraph *g,
                               Declaration *d) const {
  args_.Apply([g, d](auto const &expr) { expr->DependentDecls(g, d); });
  for (auto &block : blocks_) { block.DependentDecls(g, d); }
}

VerifyResult ScopeNode::VerifyType(Context *ctx) {
  ASSIGN_OR(return _, auto name_result, name_->VerifyType(ctx));

  auto arg_types =
      args_.Transform([ctx, this](auto &arg) { return arg->VerifyType(ctx); });
  // TODO type check

  for (auto &block : blocks_) { block.VerifyType(ctx); }

  // TODO check the scope type makes sense.
  if (!name_result.const_) {
    ctx->error_log()->NonConstantScopeName(name_->span);
    return VerifyResult::Error();
  }

  auto *scope_lit = backend::EvaluateAs<ScopeLiteral *>(name_.get(), ctx);
  OverloadSet init_os, done_os;
  for (auto &decl : scope_lit->decls_) {
    if (decl.id_ == "init") {
      init_os.emplace(&decl, *ctx->prior_verification_attempt(&decl));
    } else if (decl.id_ == "done") {
      done_os.emplace(&decl, *ctx->prior_verification_attempt(&decl));
    }
  }

  auto arg_results =
      args_.Transform([ctx](std::unique_ptr<Expression> const &arg) {
        auto *arg_ptr = const_cast<Expression *>(arg.get());
        return std::pair{arg_ptr, arg_ptr->VerifyType(ctx)};
      });

  ASSIGN_OR(return _, std::ignore,
                   VerifyDispatch(this, init_os, arg_results, ctx));
  ASSIGN_OR(
      return _, std::ignore,
             VerifyDispatch(ExprPtr{this, true}, done_os, /* TODO */ {}, ctx));
  // TODO
  for (auto &block_node : blocks_) { block_node.stmts_.VerifyType(ctx); }

  // TODO check that all the blocks make sense and emit errors

  // TODO compute what type this should return
  // TODO can this evaluate to anything?
  // TODO constant is wrong.
  return ctx->set_result(this, VerifyResult::Constant(type::Void()));
}

void ScopeNode::ExtractJumps(JumpExprs *rets) const {
  for (auto &block : blocks_) { block.ExtractJumps(rets); }
}

ir::Results ScopeNode::EmitIr(Context *ctx) {
  ctx->yields_stack_.emplace_back();
  base::defer d([&]() { ctx->yields_stack_.pop_back(); });

  auto init_block = ir::CompiledFn::Current->AddBlock();
  auto land_block = ir::CompiledFn::Current->AddBlock();

  absl::flat_hash_map<ir::Block, ir::BlockIndex> block_map{
      {ir::Block::Start(), init_block}, {ir::Block::Exit(), land_block}};

  absl::flat_hash_map<std::string_view, std::tuple<ir::Block, BlockNode *>>
      name_to_block;
  auto *scope_lit = backend::EvaluateAs<ScopeLiteral *>(name_.get(), ctx);
  for (auto &decl : scope_lit->decls_) {
    name_to_block.emplace(
        std::piecewise_construct, std::forward_as_tuple(decl.id_),
        std::forward_as_tuple(
            backend::EvaluateAs<ir::Block>(
                type::Typed<Expression *>{&decl, type::Blk()}, ctx),
            nullptr));
  }

  for (auto &block_node : blocks_) {
    if (auto *block_id = block_node.name_->if_as<Identifier>()) {
      if (auto iter = name_to_block.find(block_id->token);
          iter != name_to_block.end()) {
        std::get<1>(iter->second) = &block_node;
        block_map.emplace(std::get<0>(iter->second),
                          ir::CompiledFn::Current->AddBlock());
      } else {
        base::Log() << block_id->token;
        NOT_YET();
      }
    } else {
      NOT_YET(block_node.name_->to_string(0));
    }
  }

  ir::UncondJump(init_block);

  ir::BasicBlock::Current = init_block;
  ASSERT_NOT_NULL(ctx->dispatch_table(this))
      ->EmitInlineCall(
          args_.Transform([ctx](std::unique_ptr<Expression> const &expr) {
            return std::pair(const_cast<Expression *>(expr.get()),
                             expr->EmitIr(ctx));
          }),
          ASSERT_NOT_NULL(ctx->type_of(this)), block_map, ctx);

  for (auto [block_name, block_and_node] : name_to_block) {
    if (block_name == "init" || block_name == "done") { continue; }
    auto &[block, node]     = block_and_node;
    ir::BasicBlock::Current = block_map.at(block);
    ASSERT_NOT_NULL(ctx->dispatch_table(block.get()))
        ->EmitInlineCall({}, /* TODO block type */ type::Blk(), {}, ctx);

    ASSERT_NOT_NULL(node)->EmitIr(ctx);

    ASSERT_NOT_NULL(ctx->dispatch_table(ExprPtr{block.get(), true}))
        ->EmitInlineCall({}, /* TODO block type */ type::Blk(), block_map, ctx);
  }

  ir::UncondJump(land_block);
  ir::BasicBlock::Current = land_block;
  ASSERT_NOT_NULL(ctx->dispatch_table(ExprPtr{this, true}))
      ->EmitInlineCall({}, ASSERT_NOT_NULL(ctx->type_of(this)), {}, ctx);

  return ir::Results{};
}

std::vector<ir::RegisterOr<ir::Addr>> ScopeNode::EmitLVal(Context *) {
  UNREACHABLE(this);
}
}  // namespace ast
