#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/type_for_diagnostic.h"
#include "compiler/verify/common.h"
#include "compiler/verify/verify.h"
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
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
  std::string type;
  std::string index_type;
};

struct IndexingArrayOutOfBounds {
  static constexpr std::string_view kCategory = "value-error";
  static constexpr std::string_view kName     = "indexing-array-out-of-bounds";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Array is indexed out of bounds. Array of type `%s` has size %d "
            "but you are attempting to access position %d.",
            array, length, index),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
  std::string array;
  ir::Integer length;
  ir::Integer index;
};

struct NegativeArrayIndex {
  static constexpr std::string_view kCategory = "value-error";
  static constexpr std::string_view kName     = "negative-array-index";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Array is indexed with a negative value."),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
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
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
  std::string type;
};

// Validate the index type, possibly emitting diagnostics
bool ValidIndexType(CompilationDataReference data, ast::Index const *node,
                    type::Type type, type::QualType index_qt) {
  if (index_qt.ok()) {
    if (type::IsIntegral(index_qt.type())) { return true; }
    data.diag().Consume(InvalidIndexType{
        .view       = node->range(),
        .type       = TypeForDiagnostic(node->lhs(), data.context()),
        .index_type = TypeForDiagnostic(node->rhs(), data.context()),
    });
  }
  return false;
}

type::QualType VerifySliceIndex(CompilationDataReference data,
                                ast::Index const *node,
                                type::Slice const *slice_type,
                                type::Quals quals, type::QualType index_qt) {
  // TODO: Emit errors when possible for out-of-bounds indices. Or probabl just
  // do this with properties/symbolic-execution.
  quals = (quals & index_qt.quals()) | type::Quals::Buf();
  type::QualType qt(slice_type->data_type(), quals);
  if (not ValidIndexType(data, node, slice_type, index_qt)) { qt.MarkError(); }
  return data.context().set_qual_type(node, qt)[0];
}

type::QualType VerifyArrayIndex(CompilationDataReference data,
                                ast::Index const *node,
                                type::Array const *array_type,
                                type::Quals quals, type::QualType index_qt) {
  if (index_qt.quals() <= ~type::Quals::Const()) {
    quals &= ~type::Quals::Const();
  }
  type::Type data_type = array_type->data_type();
  type::QualType qt(data_type, quals);

  if (not ValidIndexType(data, node, array_type, index_qt)) {
    qt.MarkError();
    return data.context().set_qual_type(node, qt)[0];
  }

  if (index_qt.constant()) {
    std::optional<ir::Integer> maybe_index_value =
        data.EvaluateOrDiagnoseAs<ir::Integer>(node->rhs());
    if (not maybe_index_value.has_value()) { return type::QualType::Error(); }
    ir::Integer index = *maybe_index_value;

    if (index < 0) {
      data.diag().Consume(NegativeArrayIndex{
          .view = node->range(),
      });
      qt.MarkError();
    } else if (index >= array_type->length()) {
      data.diag().Consume(IndexingArrayOutOfBounds{
          .view   = node->range(),
          .array  = TypeForDiagnostic(node->lhs(), data.context()),
          .length = array_type->length(),
          .index  = index,
      });
      qt.MarkError();
    }
  }
  return data.context().set_qual_type(node, qt)[0];
}

type::QualType VerifyBufferPointerIndex(CompilationDataReference data,
                                        ast::Index const *node,
                                        type::BufferPointer const *buf_ptr_type,
                                        type::Quals quals,
                                        type::QualType index_qt) {
  quals = (quals & index_qt.quals()) | type::Quals::Buf();
  type::QualType qt(buf_ptr_type->pointee(), quals);
  if (not ValidIndexType(data, node, buf_ptr_type, index_qt)) {
    qt.MarkError();
  }
  return data.context().set_qual_type(node, qt)[0];
}

}  // namespace

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::Index const *node) {
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
    CallMetadata metadata(
        "__index__", node->scope(),
        ModulesFromTypeProvenance({lhs_qt.type(), index_qt.type()}));
    if (metadata.overloads().empty()) {
      diag().Consume(InvalidIndexing{
          .view = node->range(),
          .type = TypeForDiagnostic(node->lhs(), context()),
      });
      qt = type::QualType::Error();
    } else {
      absl::flat_hash_set<type::Function const *> member_types;
      CallMetadata::callee_locator_t resolved_call =
          static_cast<ast::Expression const *>(nullptr);
      for (auto overload : metadata.overloads()) {
        if (auto *info = overload.get_if<module::Module::SymbolInformation>()) {
          // Must be callable because we're looking at overloads for operators
          // which have previously been type-checked to ensure callability.
          member_types.insert(
              &info->qualified_type.type().as<type::Function>());
          resolved_call = overload;
        } else if (auto qts =
                       ModuleFor(overload.get<ast::Expression>())
                           ->as<CompiledModule>()
                           .context()
                           .maybe_qual_type(overload.get<ast::Expression>());
                   not qts.empty()) {
          ASSIGN_OR(continue, auto qt, qts[0]);
          // Must be callable because we're looking at overloads for operators
          // which have previously been type-checked to ensure callability.
          auto &c = qt.type().as<type::Function>();
          member_types.insert(&c);
          resolved_call = overload;
        }
      }

      context().SetCallMetadata(node, CallMetadata(resolved_call));

      ASSERT(member_types.size() == 1u);
      qt = type::QualType((*member_types.begin())->return_types()[0],
                          type::Quals::Unqualified());
    }
  }

  return context().set_qual_type(node, qt);
}

}  // namespace compiler
