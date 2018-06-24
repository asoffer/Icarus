#include "ast/codeblock.h"

#include "base/util.h"
#include "ir/func.h"

namespace IR {
namespace {
Val Contextualize(AST::CodeBlock code, std::vector<IR::Val> args) {
  args.push_back(IR::Val::CodeBlock(std::move(code)));
  ASSERT(Func::Current != nullptr);
  Cmd cmd(type::Code, Op::Contextualize, {std::move(args)});
  Func::Current->block(BasicBlock::Current).cmds_.push_back(std::move(cmd));
  return cmd.reg();
}
}  // namespace
}  // namespace IR

namespace AST {
std::string CodeBlock::to_string(size_t n) const {
  return std::visit(
      base::overloaded{[](const std::string &s) -> std::string {
                         return "error(" + s + ")";
                       },
                       [n](const Statements &stmts) -> std::string {
                         if (stmts.content_.empty()) { return "{{}}"; }
                         return "{{\n" + stmts.to_string(n + 1) + "\n}}";
                       }},
      content_);
}

std::vector<IR::Val> CodeBlock::EmitIR(Context *) {
  std::vector<IR::Val> args;
  auto copy = *this;
  if (auto *stmts = std::get_if<AST::Statements>(&copy.content_)) {
    stmts->SaveReferences(scope_, &args);
  }
  return {IR::Contextualize(std::move(copy), std::move(args))};
}

std::vector<IR::Val> CodeBlock::EmitLVal(Context *) { UNREACHABLE(*this); }

bool operator==(const CodeBlock &lhs, const CodeBlock &rhs) {
  if (auto *lhs_stmts = std::get_if<Statements>(&lhs.content_)) {
    if (auto *rhs_stmts = std::get_if<Statements>(&rhs.content_)) {
      return lhs_stmts->content_ == rhs_stmts->content_;
    } else {
      return false;
    }
  } else {
    if (auto *rhs_stmts = std::get_if<Statements>(&rhs.content_)) {
      return false;
    } else {
      return std::get<std::string>(lhs.content_) ==
             std::get<std::string>(rhs.content_);
    }
  }
}

bool operator<(const CodeBlock &lhs, const CodeBlock &rhs) {
  if (auto *lhs_stmts = std::get_if<Statements>(&lhs.content_)) {
    if (auto *rhs_stmts = std::get_if<Statements>(&rhs.content_)) {
      return lhs_stmts->content_ < rhs_stmts->content_;
    } else {
      return true;
    }
  } else {
    if (auto *rhs_stmts = std::get_if<Statements>(&rhs.content_)) {
      return false;
    } else {
      return std::get<std::string>(lhs.content_) <
             std::get<std::string>(rhs.content_);
    }
  }
}

bool operator>(const CodeBlock &lhs, const CodeBlock &rhs) { return rhs < lhs; }
}  // namespace AST
