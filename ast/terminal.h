#ifndef ICARUS_AST_TERMINAL_H
#define ICARUS_AST_TERMINAL_H

#include <sstream>

#include "ast/expression.h"
#include "ir/block.h"
#include "misc/context.h"
#include "misc/module.h"

struct Context;

namespace ast {
struct Terminal : public Expression {
  Terminal() = default;
  Terminal(const TextSpan &span, ir::Results results, type::Type const *t)
      : Expression(span), results_(std::move(results)), type_(t) {}
  ~Terminal() override {}

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t) const override {
    if (type_ == type::Bool) {
      return results_.get<bool>(0).val_ ? "true" : "false";
    } else if (type_ == type::Int64) {
      return std::to_string(results_.get<int64_t>(0).val_) + "_i64";
    } else if (type_ == type::Nat64) {
      return std::to_string(results_.get<uint64_t>(0).val_) + "_u64";
    } else if (type_ == type::Int32) {
      return std::to_string(results_.get<int32_t>(0).val_) + "_i32";
    } else if (type_ == type::Nat32) {
      return std::to_string(results_.get<uint32_t>(0).val_) + "_u32";
    } else if (type_ == type::Int16) {
      return std::to_string(results_.get<int16_t>(0).val_) + "_i16";
    } else if (type_ == type::Nat16) {
      return std::to_string(results_.get<uint16_t>(0).val_) + "_u16";
    } else if (type_ == type::Int8) {
      return std::to_string(results_.get<int8_t>(0).val_) + "_i8";
    } else if (type_ == type::Nat8) {
      return std::to_string(results_.get<uint8_t>(0).val_) + "_u8";
    } else if (type_ == type::Type_) {
      return results_.get<type::Type const *>(0).val_->to_string();
    } else if (type_ == type::Block) {
      std::stringstream ss;
      ss << results_.get<ir::Block>(0).val_;
      return ss.str();
    }
    return "<<terminal: " + type_->to_string() + ">>";
  }

  ir::Results results_;
  type::Type const *type_;
};
}  // namespace ast

#endif  // ICARUS_AST_TERMINAL_H
