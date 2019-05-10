#include "ast/scope_node.h"


#include <sstream>

#include "ast/access.h"
#include "ast/block_literal.h"
#include "ast/block_node.h"
#include "ast/function_literal.h"
#include "ast/identifier.h"
#include "ast/scope_literal.h"
#include "backend/eval.h"
#include "base/util.h"
#include "core/fn_args.h"
#include "core/scope.h"
#include "ir/compiled_fn.h"
#include "ir/components.h"
#include "misc/context.h"
#include "type/cast.h"
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

  auto arg_results =
      args_.Transform([ctx](std::unique_ptr<Expression> const &arg) {
        return std::pair{arg.get(), arg->VerifyType(ctx)};
      });

  auto *mod       = scope_lit->decls_.at(0).mod_;
  bool swap_bc    = ctx->mod_ != mod;
  Module *old_mod = std::exchange(ctx->mod_, mod);
  if (swap_bc) { ctx->constants_ = &ctx->mod_->dep_data_.front(); }
  base::defer d([&] {
    ctx->mod_ = old_mod;
    if (swap_bc) { ctx->constants_ = &ctx->mod_->dep_data_.front(); }
  });

  std::vector<type::Type const*> done_return_types;
  for (auto &decl : scope_lit->decls_) {
    if (decl.id_ == "init") {
      init_os.emplace(&decl,
                      *ASSERT_NOT_NULL(ctx->prior_verification_attempt(&decl)));
    } else if (decl.id_ == "done") {
      // TODO generics?
      done_return_types.push_back(
          type::Tup(ASSERT_NOT_NULL(ctx->prior_verification_attempt(&decl))
                        ->type_->as<type::Function>()
                        .output));
    }
  }

  if (done_return_types.empty()) {
    NOT_YET("log an error");
    return VerifyResult::Error();
  }

  type::Type const *combined = done_return_types.at(0);
  for (auto iter = std::next(done_return_types.begin());
       iter != done_return_types.end(); ++iter) {
    combined = type::Meet(combined, *iter);
    if (combined == nullptr) { NOT_YET("log an error"); }
  }

  ASSIGN_OR(
      return _, std::ignore,
             VerifyDispatch(ExprPtr{this, 0x02}, init_os, arg_results, ctx));

  for (auto &block_node : blocks_) { block_node.stmts_.VerifyType(ctx); }

  return VerifyResult::Constant(combined);
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
    if (decl.id_ == "init") {
      continue;
    } else if (decl.id_ == "done") {
      continue;
    } else {
      auto bs = backend::EvaluateAs<ir::BlockSequence>(
          type::Typed<Expression *>{&decl, type::Block}, ctx);
      ASSERT(bs.size() == 1u);
      name_to_block.emplace(std::piecewise_construct,
                            std::forward_as_tuple(decl.id_),
                            std::forward_as_tuple(bs.at(0), nullptr));
    }
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

  // TODO this lambda thing is an awful hack.
  ASSERT_NOT_NULL([&] {
    auto *mod       = scope_lit->decls_.at(0).mod_;
    bool swap_bc    = ctx->mod_ != mod;
    Module *old_mod = std::exchange(ctx->mod_, mod);
    if (swap_bc) { ctx->constants_ = &ctx->mod_->dep_data_.front(); }
    base::defer d([&] {
      ctx->mod_ = old_mod;
      if (swap_bc) { ctx->constants_ = &ctx->mod_->dep_data_.front(); }
    });
    return ctx->dispatch_table(ExprPtr{this, 0x02});
  }())
      ->EmitInlineCall(
          args_.Transform([ctx](std::unique_ptr<Expression> const &expr) {
            return std::pair(expr.get(), expr->EmitIr(ctx));
          }),
          block_map, ctx);

  for (auto [block_name, block_and_node] : name_to_block) {
    if (block_name == "init" || block_name == "done") { continue; }
    auto &[block, node]     = block_and_node;
    auto iter = block_map.find(block);
    if (iter == block_map.end()) { continue; }
    ir::BasicBlock::Current = iter->second;
    ASSERT_NOT_NULL(node)->EmitIr(ctx);

    ASSERT_NOT_NULL([&] {
      auto *mod       = scope_lit->decls_.at(0).mod_;
      bool swap_bc    = ctx->mod_ != mod;
      Module *old_mod = std::exchange(ctx->mod_, mod);
      if (swap_bc) { ctx->constants_ = &ctx->mod_->dep_data_.front(); }
      base::defer d([&] {
        ctx->mod_ = old_mod;
        if (swap_bc) { ctx->constants_ = &ctx->mod_->dep_data_.front(); }
      });
      return ctx->dispatch_table(ExprPtr{block.get(), 0x02});
    }())
        ->EmitInlineCall({}, block_map, ctx);
  }

  ir::UncondJump(land_block);
  ir::BasicBlock::Current = land_block;

  // TODO this lambda thing is an awful hack.
  base::Log() << this->to_string(0);
  return ASSERT_NOT_NULL([&] {
           auto *mod       = scope_lit->decls_.at(0).mod_;
           bool swap_bc    = ctx->mod_ != mod;
           Module *old_mod = std::exchange(ctx->mod_, mod);
           if (swap_bc) { ctx->constants_ = &ctx->mod_->dep_data_.front(); }
           base::defer d([&] {
             ctx->mod_ = old_mod;
             if (swap_bc) { ctx->constants_ = &ctx->mod_->dep_data_.front(); }
           });
           return ctx->dispatch_table(this);
         }())
      ->EmitInlineCall({}, {}, ctx);
}

std::vector<ir::RegisterOr<ir::Addr>> ScopeNode::EmitLVal(Context *) {
  UNREACHABLE(this);
}
}  // namespace ast
