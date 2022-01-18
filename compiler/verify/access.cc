#include <string>
#include <utility>

#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/module.h"
#include "compiler/type_for_diagnostic.h"
#include "compiler/verify/verify.h"
#include "diagnostic/message.h"
#include "ir/value/module_id.h"
#include "type/array.h"
#include "type/block.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/overload_set.h"
#include "type/primitive.h"
#include "type/qual_type.h"
#include "type/struct.h"

namespace compiler {
namespace {

struct DeducingAccess {
  static constexpr std::string_view kCategory = "pattern-error";
  static constexpr std::string_view kName     = "deducing-access";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("The type of an object cannot be deduced from the "
                         "type of a member."),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  frontend::SourceView view;
};

struct [[maybe_unused]] IncompleteTypeMemberAccess {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "incomplete-type-member-access";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Cannot access a member of an incomplete type `%s`.",
                         type),
        diagnostic::SourceQuote(&member_view.buffer())
            .Highlighted(member_view.range(), diagnostic::Style::ErrorText()));
  }

  frontend::SourceView member_view;
  type::Type type;
};

struct MissingMember {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "missing-member";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Expressions of type `%s` have no member named `%s`.",
                         type, member),
        diagnostic::SourceQuote(&member_view.buffer())
            .Highlighted(expr_view.range(), diagnostic::Style{})
            .Highlighted(member_view.range(), diagnostic::Style::ErrorText()));
  }

  frontend::SourceView expr_view;
  frontend::SourceView member_view;
  std::string member;
  std::string type;
};

struct MissingConstantMember {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "missing-constant-member";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No member named `%s` in this expression.", member),
        diagnostic::SourceQuote(&member_view.buffer())
            .Highlighted(expr_view.range(), diagnostic::Style{})
            .Highlighted(member_view.range(), diagnostic::Style::ErrorText()));
  }

  frontend::SourceView expr_view;
  frontend::SourceView member_view;
  std::string member;
};

struct NonConstantTypeMemberAccess {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "non-constant-type-member-access";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Cannot access a member of a non-constant type."),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  frontend::SourceView view;
};

struct TypeHasNoMembers {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "type-has-no-members";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("The type `%s` does not have `%s` as a member.", type,
                         member),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  std::string type;
  std::string member;
  frontend::SourceView view;
};

struct NonExportedMember {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-exported-member";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Expressions of type `%s` do not export the member `%s`.", type,
            member),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  std::string member;
  type::Type type;
  frontend::SourceView view;
};

struct NonConstantModuleMemberAccess {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "non-constant-module-member-access";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Cannot access a member of a non-constant module."),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  frontend::SourceView view;
};

struct UndeclaredIdentifierInModule {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "undeclared-identifier-in-module";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Module contains no exported member `%s`", id),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  std::string_view id;
  frontend::SourceView view;
};

// Returns a pair whose first element is the type obtained by dereferencing the
// argument as many times as possible, and whose second element is the number of
// dereferences requried.
std::pair<type::Type, int> DereferenceAll(type::Type t) {
  int num_derefs = 0;
  while (auto *p = t.if_as<type::Pointer>()) {
    t = p->pointee();
    ++num_derefs;
  }
  return std::pair(t, num_derefs);
}

// Verification of an `Access` expression when the operand is a type. This is
// only allowed when the type in question is constant and is an enum or flags.
//
// Note: In order to ensure that when diagnostics are emitted, the requisite
// type information is available, we need to capture the type information (via
// calls to `set_qual_types` before the diagnostic is emitted). This means we
// have to call `set_qual_types` on each return path, possibly before a
// diagnostic, rather than using the returned value when this function is
// called.
absl::Span<type::QualType const> AccessTypeMember(CompilationDataReference c,
                                                  ast::Access const *node,
                                                  type::QualType operand_qt) {
  LOG("Access", "%s", node->DebugString());
  if (not operand_qt.constant()) {
    c.diag().Consume(NonConstantTypeMemberAccess{
        .view = SourceViewFor(node),
    });
    return c.context().set_qual_type(node, type::QualType::Error());
  }
  ASSIGN_OR(return c.context().set_qual_type(node, type::QualType::Error()),
                   auto evaled_type,
                   c.EvaluateOrDiagnoseAs<type::Type>(node->operand()));
  auto qt = type::QualType::Constant(evaled_type);

  if (type::Array const *a = evaled_type.if_as<type::Array>()) {
    if (node->member_name() == "length") {
      return c.context().set_qual_type(
          node, type::QualType::Constant(type::Array::LengthType()));
    } else if (node->member_name() == "element_type") {
      return c.context().set_qual_type(node,
                                       type::QualType::Constant(type::Type_));
    } else {
      auto qts = c.context().set_qual_type(node, type::QualType::Error());
      c.diag().Consume(MissingConstantMember{
          .expr_view = SourceViewFor(node->operand()),
          .member_view =
              frontend::SourceView(SourceBufferFor(node), node->member_range()),
          .member = std::string{node->member_name()},
      });
      return qts;
    }
  }

  // For enums and flags, regardless of whether we can get the value, it's
  // clear that node is supposed to be a member so we should emit an error but
  // carry on assuming that node is an element of that enum type.
  if (auto *e = evaled_type.if_as<type::Enum>()) {
    auto &e_mod = const_cast<module::BasicModule *>(e->defining_module())
                      ->as<compiler::CompiledModule>();
    ast::EnumLiteral const *enum_lit =
        ASSERT_NOT_NULL(e_mod.context().AstLiteral(e));
    c.EnsureComplete({.kind    = WorkItem::Kind::CompleteEnum,
                      .node    = enum_lit,
                      .context = &e_mod.context()});
    if (not e->Get(node->member_name()).has_value()) {
      // We can continue passed this error because we are confident that this is
      // an enumerator, but we should not emit code for it.
      qt.MarkError();
      auto qts = c.context().set_qual_type(node, qt);

      c.diag().Consume(MissingConstantMember{
          .expr_view = SourceViewFor(node->operand()),
          .member_view =
              frontend::SourceView(SourceBufferFor(node), node->member_range()),
          .member = std::string{node->member_name()},
      });
      return qts;
    } else {
      return c.context().set_qual_type(node, qt);
    }
  }

  if (auto *f = evaled_type.if_as<type::Flags>()) {
    auto &f_mod = const_cast<module::BasicModule *>(f->defining_module())
                      ->as<compiler::CompiledModule>();
    ast::EnumLiteral const *flags_lit =
        ASSERT_NOT_NULL(f_mod.context().AstLiteral(f));
    c.EnsureComplete({.kind    = WorkItem::Kind::CompleteEnum,
                      .node    = flags_lit,
                      .context = &f_mod.context()});
    if (not f->Get(node->member_name()).has_value()) {
      // We can continue passed this error because we are confident that this is
      // a flag, but we should not emit code for it.
      qt.MarkError();
      auto qts = c.context().set_qual_type(node, qt);

      c.diag().Consume(MissingConstantMember{
          .expr_view = SourceViewFor(node->operand()),
          .member_view =
              frontend::SourceView(SourceBufferFor(node), node->member_range()),
          .member = std::string(node->member_name()),
      });
      return qts;
    } else {
      return c.context().set_qual_type(node, qt);
    }
  }

  if (auto *s = evaled_type.if_as<type::Struct>()) {
    c.EnsureComplete({
        .kind    = WorkItem::Kind::CompleteStruct,
        .node    = c.context().AstLiteral(s),
        .context = &c.context(),
    });
    if (auto const *member = s->constant(node->member_name())) {
      absl::flat_hash_set<ast::Expression const *> ids;

      auto &s_mod = s->defining_module()->as<compiler::CompiledModule>();
      auto const *struct_lit = s_mod.context().AstLiteral(s);

      if (c.diag().num_consumed() != 0) {
        c.resources().module->set_dependent_module_with_errors();
      }

      for (auto const &decl : struct_lit->as<ast::StructLiteral>().fields()) {
        if (not(decl.flags() & ast::Declaration::f_IsConst)) { continue; }

        if (c.resources().module != &s_mod and
            not decl.hashtags.contains(ir::Hashtag::Export)) {
          continue;
        }
        for (auto const &id : decl.ids()) {
          if (id.name() != node->member_name()) { continue; }
          ids.insert(&id);
        }
      }
      c.context().SetCallMetadata(node, CallMetadata(std::move(ids)));
      return c.context().set_qual_type(node,
                                       type::QualType::Constant(member->type));
    }
  }

  // TODO: Determine whether structs are allowed to have constant members
  // accessible through the type-name. At the moment this is not allowed.
  auto qts = c.context().set_qual_type(node, type::QualType::Error());
  c.diag().Consume(TypeHasNoMembers{
      .type   = ExpressionForDiagnostic(node->operand(), c.context()),
      .member = std::string(node->member_name()),
      .view = frontend::SourceView(SourceBufferFor(node), node->member_range()),
  });
  return qts;
}

// Verifies access to a struct field. The field must be exported if it is in a
// different module.
type::QualType AccessStructMember(CompilationDataReference data,
                                  ast::Access const *node,
                                  type::Struct const *s, type::Quals quals) {
  data.EnsureComplete({
      .kind    = WorkItem::Kind::CompleteStructData,
      .node    = ASSERT_NOT_NULL(data.context().AstLiteral(s)),
      .context = &data.context(),
  });
  ASSERT(s->completeness() >= type::Completeness::DataComplete);

  auto const *member = s->field(node->member_name());
  if (not member) {
    data.diag().Consume(MissingMember{
        .expr_view = SourceViewFor(node->operand()),
        .member_view =
            frontend::SourceView(SourceBufferFor(node), node->member_range()),
        .member = std::string{node->member_name()},
        .type   = TypeForDiagnostic(node->operand(), data.context()),
    });
    return type::QualType::Error();
  }

  type::QualType qt(member->type, quals | type::Quals::Ref());

  // Struct field members need to be exported in addition to the struct itself.
  if (data.resources().module != s->defining_module() and
      not member->hashtags.contains(ir::Hashtag::Export)) {
    data.diag().Consume(NonExportedMember{
        .member = std::string{node->member_name()},
        .type   = s,
        .view   = SourceViewFor(node),
    });

    // We can continue passed this error because we are confident about the
    // fields type, but we should not emit code for it.
    qt.MarkError();
  }

  return qt;
}

// Verifies access to a symbol in a different module. If there are multiple
// symbols of the same name, verify that they form a valid overload set.
type::QualType AccessModuleMember(CompilationDataReference c,
                                  ast::Access const *node,
                                  type::QualType operand_qt) {
  if (not operand_qt.constant()) {
    c.diag().Consume(NonConstantModuleMemberAccess{
        .view = SourceViewFor(node),
    });
    return type::QualType::Error();
  }

  std::optional mod_id = c.EvaluateOrDiagnoseAs<ir::ModuleId>(node->operand());
  if (not mod_id) { return type::QualType::Error(); }

  // There is no way to refer to the current module, but a bug here could cause
  // a deadlock as this module waits for the notification that it's declarations
  // can be exported, so we would prefer to abort.
  auto const &mod = c.importer().get(*mod_id).as<CompiledModule>();
  ASSERT(&mod != c.resources().module);

  // Note: for any declarations read across module boundaries, we set the
  // QualType of the imported declaration on the importing module context. This
  // makes it findable when it's called via an overload set as is type-checked
  // in VerifyCallee.
  auto ids = mod.ExportedDeclarationIds(node->member_name());

  switch (ids.size()) {
    case 0: {
      c.diag().Consume(UndeclaredIdentifierInModule{
          .id   = node->member_name(),
          .view = SourceViewFor(node),
      });
      return type::QualType::Error();
    } break;
    case 1: {
      type::QualType qt = mod.context().qual_types(ids[0])[0];

      if (c.diag().num_consumed() != 0) {
        c.resources().module->set_dependent_module_with_errors();
      }

      if (not qt.ok()) {
        LOG("AccessModuleMember",
            "Found member in a different module that is missing a type. "
            "Suspected error generated from that module: %s",
            node->DebugString());
        return type::QualType::Error();
      } else {
        return c.context().set_qual_type(ids[0], qt)[0];
      }
    } break;
    default: {
      // TODO: these may also be an overload set of scopes
      type::Quals quals = type::Quals::Const();
      absl::flat_hash_set<type::Type> member_types;
      auto const &ctx = mod.context();

      if (c.diag().num_consumed() != 0) {
        c.resources().module->set_dependent_module_with_errors();
      }

      for (auto const *id : ids) {
        auto qt = ctx.qual_types(id)[0];
        if (not qt.ok()) {
          LOG("AccessModuleMember",
              "Found member in a different module that is missing a type. "
              "Suspected error generated from that module: %s",
              id->DebugString());
          return type::QualType::Error();
        }

        quals &= qt.quals();
        c.context().set_qual_type(id, qt);
        member_types.insert(qt.type());
      }

      return type::QualType(type::MakeOverloadSet(member_types), quals);
    } break;
  }
  UNREACHABLE();
}

}  // namespace

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::Access const *node) {
  if (auto qts = context().maybe_qual_type(node); qts.data()) { return qts; }

  ASSIGN_OR(return context().set_qual_types(node, type::QualType::ErrorSpan()),
                   auto operand_qt, VerifyType(node->operand())[0]);

  auto [base_type, num_derefs] = DereferenceAll(operand_qt.type());
  if (base_type == type::Type_) {
    return AccessTypeMember(*this, node, operand_qt);
  } else if (base_type == type::Module) {
    return context().set_qual_type(node,
                                   AccessModuleMember(*this, node, operand_qt));
  } else {
    auto quals = operand_qt.quals();
    if (num_derefs > 0) { quals |= type::Quals::Ref(); }

    if (auto const *s = base_type.if_as<type::Slice>()) {
      if (node->member_name() == "length") {
        return context().set_qual_type(
            node, type::QualType(type::U64, quals | type::Quals::Ref()));
      } else if (node->member_name() == "data") {
        return context().set_qual_type(
            node, type::QualType(type::BufPtr(s->data_type()),
                                 quals | type::Quals::Ref()));
      } else {
        diag().Consume(MissingMember{
            .expr_view   = SourceViewFor(node->operand()),
            .member_view = frontend::SourceView(SourceBufferFor(node),
                                                node->member_range()),
            .member      = std::string{node->member_name()},
            .type        = TypeForDiagnostic(node->operand(), context()),
        });
        return context().set_qual_types(node, type::QualType::ErrorSpan());
      }
    } else if (auto *s = base_type.if_as<type::Struct>()) {
      return context().set_qual_type(node,
                                     AccessStructMember(*this, node, s, quals));
    } else if (base_type == type::ScopeContext) {
      ASSIGN_OR(
          return context().set_qual_types(node, type::QualType::ErrorSpan()),
                 auto scope_context,
                 EvaluateOrDiagnoseAs<ir::ScopeContext>(node->operand()));
      auto block = scope_context.find(node->member_name());
      if (block == ir::Block::Invalid()) {
        diag().Consume(MissingMember{
            .expr_view   = SourceViewFor(node->operand()),
            .member_view = frontend::SourceView(SourceBufferFor(node),
                                                node->member_range()),
            .member      = std::string{node->member_name()},
            .type        = TypeForDiagnostic(node->operand(), context()),
        });
        return context().set_qual_types(node, type::QualType::ErrorSpan());
      } else {
        auto const &[name, possibly_generic_block, block_node] =
            scope_context[block];
        auto const *b = possibly_generic_block.get_if<type::Block>();

        context().SetCallMetadata(
            node, CallMetadata(absl::flat_hash_set<ast::Expression const *>{
                      block_node}));

        return context().set_qual_type(
            node,
            type::QualType::Constant(
                b ? type::Type(b)
                  : type::Type(possibly_generic_block
                                   .get_if<type::Generic<type::Block>>())));
      }
    } else {
      // TODO: Improve this error message. It's not just that the member is
      // missing but that we don't even think it's allowed to have a member on a
      // value of this type. Also, we're emitting the type of the operand even
      // if we want to follow multiple layers of pointers.
      diag().Consume(MissingMember{
          .expr_view = SourceViewFor(node->operand()),
          .member_view =
              frontend::SourceView(SourceBufferFor(node), node->member_range()),
          .member = std::string{node->member_name()},
          .type   = TypeForDiagnostic(node->operand(), context()),
      });
      return context().set_qual_types(node, type::QualType::ErrorSpan());
    }
  }
}

bool PatternTypeVerifier::VerifyPatternType(ast::Access const *node,
                                            type::Type t) {
  context().set_qual_type(node, type::QualType::Constant(t));
  diag().Consume(DeducingAccess{.view = SourceViewFor(node)});
  return false;
}

}  // namespace compiler
