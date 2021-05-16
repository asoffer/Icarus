#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/module.h"
#include "compiler/type_for_diagnostic.h"
#include "diagnostic/message.h"
#include "ir/value/module_id.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/overload_set.h"
#include "type/primitive.h"
#include "type/qual_type.h"
#include "type/struct.h"

#include <string>
#include <utility>

namespace compiler {
namespace {

struct IncompleteTypeMemberAccess {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "incomplete-type-member-access";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Cannot access a member of an incomplete type `%s`.",
                         type),
        diagnostic::SourceQuote(src).Highlighted(
            member_range, diagnostic::Style::ErrorText()));
  }

  frontend::SourceRange member_range;
  type::Type type;
};

struct MissingMember {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "missing-member";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Expressions of type `%s` have no member named `%s`.",
                         type, member),
        diagnostic::SourceQuote(src)
            .Highlighted(expr_range, diagnostic::Style{})
            .Highlighted(member_range, diagnostic::Style::ErrorText()));
  }

  frontend::SourceRange expr_range;
  frontend::SourceRange member_range;
  std::string member;
  std::string type;
};

struct MissingConstantMember {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "missing-constant-member";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No member named `%s` in this expression.", member),
        diagnostic::SourceQuote(src)
            .Highlighted(expr_range, diagnostic::Style{})
            .Highlighted(member_range, diagnostic::Style::ErrorText()));
  }

  frontend::SourceRange expr_range;
  frontend::SourceRange member_range;
  std::string member;
};


struct NonConstantTypeMemberAccess {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "non-constant-type-member-access";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Cannot access a member of a non-constant type."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

struct TypeHasNoMembers {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "type-has-no-members";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("The type `%s` does not have `%s` as a member.", type, member),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  type::Type type;
  std::string member;
  frontend::SourceRange range;
};

struct NonExportedMember {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-exported-member";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Expressions of type `%s` do not export the member `%s`.", type,
            member),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  std::string member;
  type::Type type;
  frontend::SourceRange range;
};

struct NonConstantModuleMemberAccess {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "non-constant-module-member-access";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Cannot access a member of a non-constant module."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

struct UndeclaredIdentifierInModule {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "undeclared-identifier-in-module";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Module contains no exported member `%s`", id),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  std::string_view id;
  frontend::SourceRange range;
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
absl::Span<type::QualType const> AccessTypeMember(Compiler &c,
                                                  ast::Access const *node,
                                                  type::QualType operand_qt) {
  if (not operand_qt.constant()) {
    c.diag().Consume(NonConstantTypeMemberAccess{
        .range = node->range(),
    });
    return c.context().set_qual_type(node, type::QualType::Error());
  }
  ASSIGN_OR(return c.context().set_qual_type(node, type::QualType::Error()),
                   auto evaled_type,
                   c.EvaluateOrDiagnoseAs<type::Type>(node->operand()));
  auto qt = type::QualType::Constant(evaled_type);

  // TODO: Choosing the type here to be i64 to match the length type for
  // arrays but that should maybe be unsigned as well.
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
          .expr_range   = node->operand()->range(),
          .member_range = node->member_range(),
          .member       = std::string{node->member_name()},
      });
      return qts;
    }
  }

  // For enums and flags, regardless of whether we can get the value, it's
  // clear that node is supposed to be a member so we should emit an error but
  // carry on assuming that node is an element of that enum type.
  if (auto *e = evaled_type.if_as<type::Enum>()) {
    if (not e->Get(node->member_name()).has_value()) {
      // We can continue passed this error because we are confident that this is
      // an enumerator, but we should not emit code for it.
      qt.MarkError();
      auto qts = c.context().set_qual_type(node, qt);

      c.diag().Consume(MissingConstantMember{
          .expr_range   = node->operand()->range(),
          .member_range = node->member_range(),
          .member       = std::string{node->member_name()},
      });
      return qts;
    } else {
      return c.context().set_qual_type(node, qt);
    }
  }

  if (auto *f = evaled_type.if_as<type::Flags>()) {
    if (not f->Get(node->member_name()).has_value()) {
      // We can continue passed this error because we are confident that this is
      // a flag, but we should not emit code for it.
      qt.MarkError();
      auto qts = c.context().set_qual_type(node, qt);

      c.diag().Consume(MissingConstantMember{
          .expr_range   = node->operand()->range(),
          .member_range = node->member_range(),
          .member       = std::string(node->member_name()),
      });
      return qts;
    } else {
      return c.context().set_qual_type(node, qt);
    }
  }

  if (auto *s = evaled_type.if_as<type::Struct>()) {
    if (auto const *member = s->constant(node->member_name())) {
      std::vector<ast::Declaration::Id const *> ids;

      auto &s_mod = s->defining_module()->as<compiler::CompiledModule>();
      auto const *ast_struct =
          s_mod.context(&c.context().module()).ast_struct(s);

      if (s_mod.diagnostic_consumer().num_consumed() != 0) {
        c.context().module().set_dependent_module_with_errors();
      }

      for (auto const &decl : ast_struct->as<ast::StructLiteral>().fields()) {
        if (not(decl.flags() & ast::Declaration::f_IsConst)) { continue; }

        if (&c.context().module() != &s_mod and
            not decl.hashtags.contains(ir::Hashtag::Export)) {
          continue;
        }
        for (auto const &id : decl.ids()) {
          if (id.name() != node->member_name()) { continue; }
          ids.push_back(&id);
        }
      }
      c.context().SetAllOverloads(node, ast::OverloadSet(ids));
      return c.context().set_qual_type(node,
                                       type::QualType::Constant(member->type));
    }
  }

  // TODO: Determine whether structs are allowed to have constant members
  // accessible through the type-name. At the moment this is not allowed.
  auto qts = c.context().set_qual_type(node, type::QualType::Error());
  c.diag().Consume(TypeHasNoMembers{
      .type   = evaled_type,
      .member = std::string(node->member_name()),
      .range  = node->member_range(),
  });
  return qts;
}

// Verifies access to a struct field. The field must be exported if it is in a
// different module.
type::QualType AccessStructMember(Compiler &c, ast::Access const *node,
                                  type::Struct const *s, type::Quals quals) {
  // TODO: Figure out how to remove const_cast here.
  switch (c.EnsureDataCompleteness(const_cast<type::Struct *>(s))) {
    case WorkItem::Result::Success: break;
    case WorkItem::Result::Deferred:
      c.diag().Consume(IncompleteTypeMemberAccess{
          .member_range = node->member_range(),
          .type         = s,
      });
      [[fallthrough]];
    case WorkItem::Result::Failure: return type::QualType::Error();
  }
  ASSERT(s->completeness() >= type::Completeness::DataComplete);

  auto const *member = s->field(node->member_name());
  if (not member) {
    c.diag().Consume(MissingMember{
        .expr_range   = node->operand()->range(),
        .member_range = node->member_range(),
        .member       = std::string{node->member_name()},
        .type         = TypeForDiagnostic(node->operand(), c.context()),
    });
    return type::QualType::Error();
  }

  type::QualType qt(member->type, quals | type::Quals::Ref());

  // Struct field members need to be exported in addition to the struct itself.
  if (&c.context().module() != s->defining_module() and
      not member->hashtags.contains(ir::Hashtag::Export)) {
    c.diag().Consume(NonExportedMember{
        .member = std::string{node->member_name()},
        .type   = s,
        .range  = node->range(),
    });

    // We can continue passed this error because we are confident about the
    // fields type, but we should not emit code for it.
    qt.MarkError();
  }

  return qt;
}

// Verifies access to a symbol in a different module. If there are multiple
// symbols of the same name, verify that they form a valid overload set.
type::QualType AccessModuleMember(Compiler &c, ast::Access const *node,
                                  type::QualType operand_qt) {
  if (not operand_qt.constant()) {
    c.diag().Consume(NonConstantModuleMemberAccess{
        .range = node->range(),
    });
    return type::QualType::Error();
  }

  ir::ModuleId mod_id = c.EvaluateModuleWithCache(node->operand());
  if (mod_id == ir::ModuleId::Invalid()) { return type::QualType::Error(); }

  // There is no way to refer to the current module, but a bug here could cause
  // a deadlock as this module waits for the notification that it's declarations
  // can be exported, so we would prefer to abort.
  auto const &mod = c.importer().get(mod_id).as<CompiledModule>();
  ASSERT(&mod != &c.context().module());

  // Note: for any declarations read across module boundaries, we set the
  // QualType of the imported declaration on the importing module context. This
  // makes it findable when it's called via an overload set as is type-checked
  // in VerifyCallee.
  auto ids = mod.scope().ExportedDeclarationIds(node->member_name());
  switch (ids.size()) {
    case 0: {
      c.diag().Consume(UndeclaredIdentifierInModule{
          .id    = node->member_name(),
          .range = node->range(),
      });
      return type::QualType::Error();
    } break;
    case 1: {
      type::QualType qt =
          mod.context(&c.context().module()).qual_types(ids[0])[0];

      if (mod.diagnostic_consumer().num_consumed() != 0) {
        c.context().module().set_dependent_module_with_errors();
      }

      if (not qt.ok()) {
        LOG("AccessModuleMember",
            "Found member in a different module that is missing a type. "
            "Suspected error generated from that module: %s",
            node->DebugString());
        return type::QualType::Error();
      } else {
        c.context().SetAllOverloads(node, ast::OverloadSet(ids));
        return c.context().set_qual_type(ids[0], qt)[0];
      }
    } break;
    default: {
      // TODO: these may also be an overload set of scopes
      type::Quals quals = type::Quals::Const();
      absl::flat_hash_set<type::Callable const *> member_types;
      auto const &ctx = mod.context(&c.context().module());

      if (mod.diagnostic_consumer().num_consumed() != 0) {
        c.context().module().set_dependent_module_with_errors();
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

        if (auto *callable = qt.type().if_as<type::Callable>()) {
          quals &= qt.quals();
          member_types.insert(callable);
          c.context().set_qual_type(id, qt);
        } else {
          LOG("AccessModuleMember",
              "Non-callable found in an overload set across module boundaries. "
              "An error should have already been generated by that module: %s",
              id->DebugString());
          return type::QualType::Error();
        }
      }

      c.context().SetAllOverloads(node, ast::OverloadSet(ids));
      return type::QualType(type::MakeOverloadSet(std::move(member_types)),
                            quals);
    } break;
  }
  UNREACHABLE();
}

}  // namespace

absl::Span<type::QualType const> Compiler::VerifyType(ast::Access const *node) {
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
            .expr_range   = node->operand()->range(),
            .member_range = node->member_range(),
            .member       = std::string{node->member_name()},
            .type         = TypeForDiagnostic(node->operand(), context()),
        });
        return context().set_qual_types(node, type::QualType::ErrorSpan());
      }
    } else if (auto *s = base_type.if_as<type::Struct>()) {
      return context().set_qual_type(node,
                                     AccessStructMember(*this, node, s, quals));
    } else {
      // TODO: Improve this error message. It's not just that the member is
      // missing but that we don't even think it's allowed to have a member on a
      // value of this type. Also, we're emitting the type of the operand even
      // if we want to follow multiple layers of pointers.
      diag().Consume(MissingMember{
          .expr_range   = node->operand()->range(),
          .member_range = node->member_range(),
          .member       = std::string{node->member_name()},
          .type         = TypeForDiagnostic(node->operand(), context()),
      });
      return context().set_qual_types(node, type::QualType::ErrorSpan());
    }
  }
}

}  // namespace compiler
