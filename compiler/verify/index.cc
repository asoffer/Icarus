#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/compiler.h"
#include "compiler/type_for_diagnostic.h"
#include "compiler/verify/common.h"
#include "type/array.h"
#include "type/primitive.h"
#include "type/provenance.h"
#include "type/qual_type.h"

namespace compiler {
namespace {

struct InvalidIndexType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "invalid-index-type";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Attempting to index a value of type `%s` with a "
                         "non-integral index. Indices must be integers, but "
                         "you provided an index of type `%s`.",
                         type, index_type),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  frontend::SourceView view;
  type::Type type;
  type::Type index_type;
};

struct IndexingArrayOutOfBounds {
  static constexpr std::string_view kCategory = "value-error";
  static constexpr std::string_view kName     = "indexing-array-out-of-bounds";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Array is indexed out of bounds. Array of type `%s` has size %u "
            "but you are attempting to access position %d.",
            type::Type(array), array->length(), index),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  frontend::SourceView view;
  type::Array const *array;
  ir::Integer index;
};

struct NegativeArrayIndex {
  static constexpr std::string_view kCategory = "value-error";
  static constexpr std::string_view kName     = "negative-array-index";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Array is indexed with a negative value."),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  frontend::SourceView view;
};

struct InvalidIndexing {
  static constexpr std::string_view kCategory = "value-error";
  static constexpr std::string_view kName     = "invalid-indexing";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Cannot index into a non-array, non-buffer type. Indexed type is "
            "a `%s`.",
            type),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  frontend::SourceView view;
  std::string type;
};

// Validate the index type, possibly emitting diagnostics
bool ValidIndexType(Compiler &c, ast::Index const *node, type::Type type,
                    type::QualType index_qt) {
  if (index_qt.ok()) {
    if (type::IsIntegral(index_qt.type())) { return true; }
    c.diag().Consume(InvalidIndexType{
        .view       = SourceViewFor(node),
        .type       = type,
        .index_type = index_qt.type(),
    });
  }
  return false;
}

type::QualType VerifySliceIndex(Compiler &c, ast::Index const *node,
                                type::Slice const *slice_type,
                                type::Quals quals, type::QualType index_qt) {
  // TODO: Emit errors when possible for out-of-bounds indices. Or probabl just
  // do this with properties/symbolic-execution.
  quals = (quals & index_qt.quals()) | type::Quals::Buf();
  type::QualType qt(slice_type->data_type(), quals);
  if (not ValidIndexType(c, node, slice_type, index_qt)) { qt.MarkError(); }
  return c.context().set_qual_type(node, qt)[0];
}

type::QualType VerifyArrayIndex(Compiler &c, ast::Index const *node,
                                type::Array const *array_type,
                                type::Quals quals, type::QualType index_qt) {
  if (index_qt.quals() <= ~type::Quals::Const()) {
    quals &= ~type::Quals::Const();
  }
  type::Type data_type = array_type->data_type();
  type::QualType qt(data_type, quals);

  if (not ValidIndexType(c, node, array_type, index_qt)) {
    qt.MarkError();
    return c.context().set_qual_type(node, qt)[0];
  }

  if (index_qt.constant()) {
    std::optional<ir::Integer> maybe_index_value =
        c.EvaluateOrDiagnoseAs<ir::Integer>(node->rhs());
    if (not maybe_index_value.has_value()) { return type::QualType::Error(); }
    ir::Integer index = *maybe_index_value;

    if (index < 0) {
      c.diag().Consume(NegativeArrayIndex{
          .view = SourceViewFor(node),
      });
      qt.MarkError();
    } else if (index >= array_type->length()) {
      c.diag().Consume(IndexingArrayOutOfBounds{
          .view  = SourceViewFor(node),
          .array = array_type,
          .index = index,
      });
      qt.MarkError();
    }
  }
  return c.context().set_qual_type(node, qt)[0];
}

type::QualType VerifyBufferPointerIndex(Compiler &c, ast::Index const *node,
                                        type::BufferPointer const *buf_ptr_type,
                                        type::Quals quals,
                                        type::QualType index_qt) {
  quals = (quals & index_qt.quals()) | type::Quals::Buf();
  type::QualType qt(buf_ptr_type->pointee(), quals);
  if (not ValidIndexType(c, node, buf_ptr_type, index_qt)) { qt.MarkError(); }
  return c.context().set_qual_type(node, qt)[0];
}

}  // namespace

absl::Span<type::QualType const> Compiler::VerifyType(ast::Index const *node) {
  auto lhs_qt   = VerifyType(node->lhs())[0];
  auto index_qt = VerifyType(node->rhs())[0];

  // We can recover from the index having errors but not from the data being
  // indexed.
  type::QualType qt;
  if (not lhs_qt.ok()) {
    qt = type::QualType::Error();
  } else if (auto const *slice_type = lhs_qt.type().if_as<type::Slice>()) {
    qt = VerifySliceIndex(*this, node, slice_type, lhs_qt.quals(), index_qt);
  } else if (auto const *array_type = lhs_qt.type().if_as<type::Array>()) {
    qt = VerifyArrayIndex(*this, node, array_type, lhs_qt.quals(), index_qt);
  } else if (auto const *buf_ptr_type =
                 lhs_qt.type().if_as<type::BufferPointer>()) {
    qt = VerifyBufferPointerIndex(*this, node, buf_ptr_type, lhs_qt.quals(),
                                  index_qt);
  } else {
    absl::flat_hash_set<type::Function const *> member_types;
    absl::flat_hash_set<ast::Declaration::Id const *> members;
    ast::OverloadSet os;

    auto get_ids = [&](Context const &ctx, ast::Declaration::Id const *id) {
      members.insert(id);
      member_types.insert(&ctx.qual_types(id)[0].type().as<type::Function>());
    };

    for (auto const &t : {lhs_qt.type(), index_qt.type()}) {
      // TODO: Checking defining_module only when this is a struct is wrong. We
      // should also handle pointers to structs, ec
      if (auto const *dm = type::Provenance(t)) {
        if (resources().module == dm) { continue; }
        dm->scope().ForEachDeclIdTowardsRoot(
            "__index__", [&](ast::Declaration::Id const *id) {
              if (id->declaration().hashtags.contains(ir::Hashtag::Export)) {
                get_ids(dm->as<CompiledModule>().context(), id);
              }
              return true;
            });
      }
    }
    node->scope()->ForEachDeclIdTowardsRoot(
        "__index__", [&](ast::Declaration::Id const *id) {
          get_ids(context(), id);
          return true;
        });

    for (auto const *id : members) { os.insert(id); }
    if (os.members().empty()) {
      diag().Consume(InvalidIndexing{
          .view = SourceViewFor(node),
          .type = TypeForDiagnostic(node->lhs(), context()),
      });
      qt = type::QualType::Error();
    } else {
      for (auto const *member : os.members()) {
        if (auto qts = context().maybe_qual_type(member); not qts.empty()) {
          ASSIGN_OR(continue, auto overload_qt, qts[0]);
          // Must be callable because we're looking at overloads for operators
          // which have previously been type-checked to ensure callability.
          auto &c = overload_qt.type().as<type::Function>();
          member_types.insert(&c);
        }
      }
    }

    context().SetViableOverloads(node, std::move(os));

    ASSERT(member_types.size() == 1u);

    qt = type::QualType((*member_types.begin())->return_types()[0],
                        type::Quals::Unqualified());
  }

  return context().set_qual_type(node, qt);
}

}  // namespace compiler
