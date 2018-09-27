#include "ast/array_literal.h"

#include "ast/verify_macros.h"
#include "context.h"
#include "error/log.h"
#include "ir/cmd.h"
#include "type/array.h"
#include "type/pointer.h"

namespace AST {
void ArrayLiteral::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
  for (auto &el : elems_) { el->assign_scope(scope); }
}

std::string ArrayLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "[";
  auto iter = elems_.begin();
  ss << (*iter)->to_string(n);
  ++iter;
  while (iter != elems_.end()) {
    ss << ", " << (*iter)->to_string(n);
    ++iter;
  }
  ss << "]";
  return ss.str();
}

type::Type const *ArrayLiteral::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;

  if (elems_.empty()) {
    type = type::EmptyArray;
    ctx->mod_->types_.buffered_emplace(this, type::EmptyArray);
    return type::EmptyArray;
  }

  for (auto &elem : elems_) {
    elem->VerifyType(ctx);
    HANDLE_CYCLIC_DEPENDENCIES;
    limit_to(elem);
  }

  const type::Type *joined = type::Err;
  for (auto &elem : elems_) {
    joined = type::Join(joined, elem->type);
    if (joined == nullptr) { break; }
  }

  if (joined == nullptr) {
    // type::Types couldn't be joined. Emit an error
    ctx->error_log_.InconsistentArrayType(span);
    type = type::Err;
    limit_to(StageRange::Nothing());
  } else if (joined == type::Err) {
    type = type::Err;  // There were no valid types anywhere in the array
    limit_to(StageRange::Nothing());
  } else {
    type = type::Arr(joined, elems_.size());
    ctx->mod_->types_.buffered_emplace(this, type::Arr(joined, elems_.size()));
  }
  return type;
}

void ArrayLiteral::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  for (auto &elem : elems_) { elem->Validate(ctx); }
}

void ArrayLiteral::SaveReferences(Scope *scope, base::vector<IR::Val> *args) {
  for (auto &elem : elems_) { elem->SaveReferences(scope, args); }
}

void ArrayLiteral::contextualize(
    const Node *correspondant,
    const base::unordered_map<const Expression *, IR::Val> &replacements) {
  for (size_t i = 0; i < elems_.size(); ++i) {
    elems_[i]->contextualize(correspondant->as<ArrayLiteral>().elems_[i].get(),
                             replacements);
  }
}

void ArrayLiteral::ExtractReturns(base::vector<const Expression *> *rets) const {
  for (auto &el : elems_) { el->ExtractReturns(rets); }
}

ArrayLiteral *ArrayLiteral::Clone() const {
  auto *result = new ArrayLiteral;
  result->span = span;
  result->elems_.reserve(elems_.size());
  for (const auto &elem : elems_) {
    result->elems_.emplace_back(elem->Clone());
  }
  return result;
}

base::vector<IR::Val> AST::ArrayLiteral::EmitIR(Context *ctx) {
  // TODO If this is a constant we can just store it somewhere.
  auto *this_type = ctx->mod_->types_.at(this);
  auto alloc = IR::Alloca(this_type);
  auto array_val  = IR::Val::Reg(alloc, type::Ptr(this_type));
  auto *data_type = this_type->as<type::Array>().data_type;
  for (size_t i = 0; i < elems_.size(); ++i) {
    type::EmitMoveInit(
        data_type, data_type, elems_[i]->EmitIR(ctx)[0],
        IR::Index(type::Ptr(this_type), alloc, static_cast<i32>(i)), ctx);
  }
  return {array_val};
}

base::vector<IR::Register> AST::ArrayLiteral::EmitLVal(Context *ctx) { UNREACHABLE(*this); }
}  // namespace AST
