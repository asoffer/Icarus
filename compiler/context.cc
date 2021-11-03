#include "compiler/context.h"
#include "absl/strings/str_format.h"

namespace compiler {

struct Context::Subcontext {
  explicit Subcontext(Context &&context) : context(std::move(context)) {}
  std::vector<type::Type> rets;
  Context context;
};

Context::Context(ir::Module *ir_mod) : ir_module_(*ASSERT_NOT_NULL(ir_mod)) {}

Context::Context(Context &&) = default;
Context::~Context()          = default;

Context::Context(Context *parent) : Context(&ASSERT_NOT_NULL(parent)->ir()) {
  tree_.parent = parent;
}

Context Context::ScratchpadSubcontext() { return Context(this); }

std::string Context::DebugString() const {
  std::string out = "context[";
  for (auto *p = this; p; p = p->parent()) {
    absl::StrAppendFormat(&out, " %p", p);
  }
  return out + " ]";
}

Context::InsertSubcontextResult Context::InsertSubcontext(
    ast::ParameterizedExpression const *node, BoundParameters const &params,
    Context &&context) {
  auto &map = tree_.children[node];
  auto [iter, inserted] =
      map.try_emplace(params, std::make_unique<Subcontext>(std::move(context)));

  if (inserted) {
    LOG("Instantiate", "Context inserted as %p", &iter->second->context);
    for (size_t i = 0; i < params.size(); ++i) {
      auto [qt, ref] = params[i];
      if (not qt.constant()) { continue; }
      iter->second->context.SetConstant(&node->params()[i].value->ids()[0],
                                        ref);
    }

    ASSERT(iter->second->context.parent() == this);
    for (size_t i = 0; i < node->params().size(); ++i) {
      auto const *decl = node->params()[i].value.get();
      auto [qt, ref]   = params[i];
      iter->second->context.set_qual_type(decl, qt);
    }
  }
  auto &[rets, ctx] = *iter->second;

  return InsertSubcontextResult{
      .params   = iter->first,
      .rets     = rets,
      .context  = ctx,
      .inserted = inserted,
  };
}

Context::FindSubcontextResult Context::FindSubcontext(
    ast::ParameterizedExpression const *node, BoundParameters const &params) {
  auto children_iter = tree_.children.find(node);
  if (children_iter == tree_.children.end()) {
    return ASSERT_NOT_NULL(parent())->FindSubcontext(node, params);
  }
  auto &map = children_iter->second;
  auto iter = map.find(params);
  ASSERT(iter != map.end());
  auto &[rets, context] = *iter->second;
  return FindSubcontextResult{
      .fn_type = type::Func(params.types(), rets),
      .context = context,
  };
}

ir::CompiledJump *Context::jump(ast::Jump const *expr) {
  auto iter = ir_jumps_.find(expr);
  return iter == ir_jumps_.end() ? nullptr : &iter->second;
}

absl::Span<type::QualType const> Context::qual_types(
    ast::Expression const *expr) const {
  auto iter = qual_types_.find(expr);
  if (iter != qual_types_.end()) { return iter->second; }
  if (auto const *p = parent()) { return p->qual_types(expr); }
  UNREACHABLE(expr->DebugString(), this);
}

absl::Span<type::QualType const> Context::maybe_qual_type(
    ast::Expression const *expr) const {
  auto iter = qual_types_.find(expr);
  if (iter != qual_types_.end()) { return iter->second; }
  if (parent()) { return parent()->maybe_qual_type(expr); }
  return absl::Span<type::QualType const>();
}

absl::Span<type::QualType const> Context::set_qual_types(
    ast::Expression const *expr, absl::Span<type::QualType const> qts) {
  auto [iter, inserted] = qual_types_.try_emplace(expr, qts.begin(), qts.end());
  return iter->second;
}

absl::Span<type::QualType const> Context::set_qual_type(
    ast::Expression const *expr, type::QualType r) {
  auto [iter, inserted] = qual_types_.try_emplace(expr, 1, r);
  return iter->second;
}

void Context::CompleteType(ast::Expression const *expr, bool success) {
  if (auto iter = qual_types_.find(expr); iter != qual_types_.end()) {
    if (not success) {
      for (auto &qt : iter->second) { qt.MarkError(); }
    }
    return;
  }
  // Note: It is possible that we never find the type, because the original
  // verification had an error.
  if (parent()) { parent()->CompleteType(expr, success); }
}

ir::ModuleId Context::imported_module(ast::Import const *node) {
  auto iter = imported_modules_.find(node);
  if (iter != imported_modules_.end()) { return iter->second; }
  if (parent()) { return parent()->imported_module(node); }
  return ir::ModuleId::Invalid();
}

void Context::set_imported_module(ast::Import const *node,
                                  ir::ModuleId module_id) {
  imported_modules_.emplace(node, module_id);
}

absl::Span<ast::Declaration const *const> Context::decls(
    ast::Identifier const *id) const {
  auto iter = decls_.find(id);
  if (iter == decls_.end()) { return ASSERT_NOT_NULL(parent())->decls(id); }
  return iter->second;
}

void Context::set_decls(ast::Identifier const *id,
                        std::vector<ast::Declaration const *> decls) {
  decls_.emplace(id, std::move(decls));
}

type::Struct *Context::get_struct(ast::StructLiteral const *s) const {
  auto iter = structs_.find(s);
  if (iter != structs_.end()) { return iter->second; }
  if (not parent()) { return nullptr; }
  return parent()->get_struct(s);
}

void Context::set_struct(ast::StructLiteral const *sl, type::Struct *s) {
  structs_.emplace(sl, s);
  reverse_structs_.emplace(s, sl);
}

type::Struct *Context::get_struct(
    ast::ParameterizedStructLiteral const *s) const {
  if (auto iter = param_structs_.find(s); iter != param_structs_.end()) {
    return iter->second;
  }
  if (not parent()) { return nullptr; }
  return parent()->get_struct(s);
}

void Context::set_struct(ast::ParameterizedStructLiteral const *sl,
                         type::Struct *s) {
  param_structs_.emplace(sl, s);
  reverse_structs_.emplace(s, sl);
}

ast::Expression const *Context::ast_struct(type::Struct const *s) const {
  if (auto iter = reverse_structs_.find(s); iter != reverse_structs_.end()) {
    return iter->second;
  }
  if (not parent()) { return nullptr; }
  return parent()->ast_struct(s);
}

bool Context::ShouldVerifyBody(ast::Node const *node) {
  return body_verification_complete_.insert(node).second;
}

void Context::ClearVerifyBody(ast::Node const *node) {
  body_verification_complete_.erase(node);
}

void Context::CompleteConstant(ast::Declaration::Id const *id) {
  auto iter = constants_.find(id);
  ASSERT(iter != constants_.end());
  iter->second.second = true;
}


ir::CompleteResultBuffer const &Context::SetConstant(
    ast::Declaration::Id const *id, ir::CompleteResultRef const &ref) {
  ir::CompleteResultBuffer buffer;
  buffer.append(ref);
  return SetConstant(id, buffer);
}

ir::CompleteResultBuffer const &Context::SetConstant(
    ast::Declaration::Id const *id, ir::CompleteResultBuffer const &buffer) {
  return constants_.try_emplace(id, std::move(buffer), false)
      .first->second.first;
}


ir::CompleteResultBuffer const *Context::Constant(
    ast::Declaration::Id const *id) const {
  auto iter = constants_.find(id);
  return iter != constants_.end() ? &iter->second.first : nullptr;
}

ir::CompleteResultBuffer const *Context::ConstantIfComplete(
    ast::Declaration::Id const *id) const {
  auto iter = constants_.find(id);
  return iter != constants_.end() and iter->second.second ? &iter->second.first
                                                          : nullptr;
}

void Context::SetAllOverloads(ast::Expression const *callee,
                              ast::OverloadSet os) {
  LOG("SetAllOverloads", "%s", callee->DebugString());
  [[maybe_unused]] auto [iter, inserted] =
      all_overloads_.emplace(callee, std::move(os));
  ASSERT(inserted == true);
}

ast::OverloadSet const *Context::AllOverloads(
    ast::Expression const *callee) const {
  auto iter = all_overloads_.find(callee);
  if (iter == all_overloads_.end()) {
    if (parent() == nullptr) { return nullptr; }
    return parent()->AllOverloads(callee);
  } else {
    return &iter->second;
  }
}

absl::Span<ast::ReturnStmt const *const> Context::ReturnsTo(
    base::PtrUnion<ast::FunctionLiteral const, ast::ShortFunctionLiteral const>
        node) const {
  auto const *v = jumps_[node];
  return v ? *v : ASSERT_NOT_NULL(parent())->ReturnsTo(node);
}

absl::Span<ast::YieldStmt const *const> Context::YieldsTo(
    base::PtrUnion<ast::BlockNode const, ast::ScopeNode const> node) const {
  auto const *v = jumps_[node];
  return v ? *v : ASSERT_NOT_NULL(parent())->YieldsTo(node);
}

absl::Span<base::PtrUnion<ast::UnconditionalGoto const,
                          ast::ConditionalGoto const> const>
Context::GoesTo(ast::Jump const *node) const {
  auto const *v = jumps_[node];
  return v ? *v : ASSERT_NOT_NULL(parent())->GoesTo(node);
}

void Context::LoadConstant(ast::Declaration::Id const *id,
                           ir::PartialResultBuffer &out) const {
  if (auto iter = constants_.find(id); iter != constants_.end()) {
    out.append(iter->second.first);
  } else {
    ASSERT_NOT_NULL(parent())->LoadConstant(id, out);
  }
}

}  // namespace compiler
