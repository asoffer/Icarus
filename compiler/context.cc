#include "compiler/context.h"

#include "absl/strings/str_format.h"
#include "ir/value/scope.h"
#include "type/slice.h"

namespace compiler {

struct Context::Subcontext {
  explicit Subcontext(Context &&context) : context(std::move(context)) {}
  core::Parameters<type::QualType> parameter_types;
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
    params.ForEachBinding(
        [&c = iter->second->context](
            ast::Declaration::Id const *id,
            BoundParameters::BoundParameterReference const &binding) {
          auto qt = binding.qual_type();
          if (auto value = binding.value(); not value.empty()) {
            LOG("Instantiate", "    Parameter has type %s and value %s", qt,
                qt.type().Representation(value));
            c.SetConstant(id, value);
          } else {
            LOG("Instantiate", "    Parameter has type %s", qt);
          }

          c.set_qual_type(id, qt);
          c.set_qual_type(&id->declaration(), qt);
        });
  }

  auto &[parameter_types, rets, ctx] = *iter->second;

  parameter_types.clear();
  auto const &c = iter->second->context;
  for (auto const &[name, value, flags] : node->params()) {
    parameter_types.append(name, c.qual_types(value.get())[0], flags);
  }

  return InsertSubcontextResult{
      .params   = parameter_types,
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
  auto &[parameter_types, rets, context] = *iter->second;
  return FindSubcontextResult{
      .fn_type = type::Func(parameter_types, rets),
      .context = context,
  };
}

absl::Span<type::QualType const> Context::qual_types(
    ast::Expression const *expr) const {
  auto iter = qual_types_.find(expr);
  if (iter != qual_types_.end()) { return iter->second; }
  if (auto const *p = parent()) { return p->qual_types(expr); }
  UNREACHABLE(expr->DebugString(), DebugString());
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
  if (auto const *id = expr->if_as<ast::Declaration::Id>()) {
    ASSERT(qts.size() == 1);
    qt_callback()(id, qts[0]);
  }
  return iter->second;
}

absl::Span<type::QualType const> Context::set_qual_type(
    ast::Expression const *expr, type::QualType r) {
  auto [iter, inserted] = qual_types_.try_emplace(expr, 1, r);
  if (auto const *id = expr->if_as<ast::Declaration::Id>()) {
    qt_callback()(id, r);
  }
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

absl::Span<ast::Declaration::Id const *const> Context::decls(
    ast::Identifier const *id) const {
  auto iter = decls_.find(id);
  if (iter == decls_.end()) { return ASSERT_NOT_NULL(parent())->decls(id); }
  return iter->second;
}

void Context::set_decls(ast::Identifier const *id,
                        std::vector<ast::Declaration::Id const *> decls) {
  [[maybe_unused]] auto [iter, inserted] = decls_.emplace(id, std::move(decls));
  ASSERT(inserted == true);
}

ir::CompleteResultBuffer const &Context::SetConstant(
    ast::Declaration::Id const *id, ir::CompleteResultRef const &ref) {
  ir::CompleteResultBuffer buffer;
  buffer.append(ref);
  return SetConstant(id, buffer);
}

ir::CompleteResultBuffer const &Context::SetConstant(
    ast::Declaration::Id const *id, ir::CompleteResultBuffer const &buffer) {
  auto const &buf = constants_.try_emplace(id, buffer).first->second;
  value_callback()(id, buf);
  return buf;
}

ir::CompleteResultBuffer const *Context::Constant(
    ast::Declaration::Id const *id) const {
  auto iter = constants_.find(id);
  if (iter != constants_.end()) { return &iter->second; }
  if (parent() != nullptr) { return parent()->Constant(id); }
  return nullptr;
}

void Context::LoadConstantAddress(ast::Expression const *expr,
                                  ir::PartialResultBuffer &out) const {
  if (auto iter = constants_.find(expr); iter != constants_.end()) {
    out.append(iter->second[0].raw());
  } else {
    ASSERT_NOT_NULL(parent())->LoadConstant(expr, out);
  }
}

void Context::LoadConstant(ast::Expression const *expr,
                           ir::CompleteResultBuffer &out) const {
  if (auto iter = constants_.find(expr); iter != constants_.end()) {
    out.append(iter->second);
  } else {
    ASSERT_NOT_NULL(parent())->LoadConstant(expr, out);
  }
}

void Context::LoadConstant(ast::Expression const *expr,
                           ir::PartialResultBuffer &out) const {
  if (auto iter = constants_.find(expr); iter != constants_.end()) {
    out.append(iter->second);
  } else {
    ASSERT_NOT_NULL(parent())->LoadConstant(expr, out);
  }
}

bool Context::TryLoadConstant(ast::Declaration::Id const *id,
                              ir::PartialResultBuffer &out) const {
  if (auto iter = constants_.find(id); iter != constants_.end()) {
    if (qual_types(id)[0].type().is_big()) {
      out.append(iter->second[0].raw().data());
    } else {
      out.append(iter->second);
    }
    return true;
  } else if (parent() != nullptr) {
    return parent()->TryLoadConstant(id, out);
  }
  return false;
}

type::Type TerminalType(ast::Terminal const &node) {
  type::Type t;
  base::MetaValue mv = node.type();
  if (mv == base::meta<ir::Integer>) {
    t = type::Integer;
  } else if (mv == base::meta<bool>) {
    t = type::Bool;
  } else if (mv == base::meta<ir::Char>) {
    t = type::Char;
  } else if (mv == base::meta<float>) {
    t = type::F32;
  } else if (mv == base::meta<double>) {
    t = type::F64;
  } else if (mv == base::meta<ir::Slice>) {
    t = type::Slc(type::Char);
  } else if (mv == base::meta<ir::addr_t>) {
    t = type::NullPtr;
  } else if (mv == base::meta<type::Type>) {
    t = type::Type_;
  } else {
    UNREACHABLE(mv.name());
  }
  return t;
}

}  // namespace compiler
