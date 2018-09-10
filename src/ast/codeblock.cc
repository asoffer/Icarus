#include "ast/codeblock.h"

#include "base/util.h"
#include "ir/val.h"
#include "ir/func.h"

namespace IR {
namespace {
Val Contextualize(AST::CodeBlock code, base::vector<IR::Val> args) {
  NOT_YET();
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

base::vector<IR::Val> CodeBlock::EmitIR(Context *) {
  base::vector<IR::Val> args;
  auto copy = *this;
  if (auto *stmts = std::get_if<AST::Statements>(&copy.content_)) {
    stmts->SaveReferences(scope_, &args);
  }
  return {IR::Contextualize(std::move(copy), std::move(args))};
}

base::vector<IR::Register> CodeBlock::EmitLVal(Context *) { UNREACHABLE(*this); }

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
