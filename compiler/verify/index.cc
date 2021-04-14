#include "ast/ast.h"
#include "compiler/compiler.h"
#include "type/array.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace compiler {
namespace {

struct InvalidIndexType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "invalid-index-type";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Attempting to index a value of type `%s` with a "
                         "non-integral index. Indices must be integers, but "
                         "you provided an index of type `%s`.",
                         type, index_type),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
  type::Type type;
  type::Type index_type;
};

struct IndexingArrayOutOfBounds {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "indexing-array-out-of-bounds";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Array is indexed out of bounds. Array of type `%s` has size %u "
            "but you are attempting to access position %d.",
            type::Type(array), array->length(), index),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
  type::Array const *array;
  uint64_t index;
};

struct NegativeArrayIndex {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "negative-array-index";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Array is indexed with a negative value."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

struct InvalidIndexing {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "invalid-indexing";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Cannot index into a non-array, non-buffer type. Indexed type is "
            "a `%s`.",
            type),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
  type::Type type;
};

// Validate the index type, possibly emitting diagnostics
bool ValidIndexType(Compiler &c, ast::Index const *node, type::Type type,
                    type::QualType index_qt) {
  if (index_qt.ok()) {
    if (type::IsIntegral(index_qt.type())) { return true; }
    c.diag().Consume(InvalidIndexType{
        .range      = node->range(),
        .type       = type,
        .index_type = index_qt.type(),
    });
  }
  return false;
}

std::optional<uint64_t> IntegralToUint64(ir::Value const &value) {
  if (auto const *val = value.get_if<int8_t>()) {
    return *val >= 0 ? std::optional<uint64_t>(*val) : std::nullopt;
  } else if (auto const *val = value.get_if<int16_t>()) {
    return *val >= 0 ? std::optional<uint64_t>(*val) : std::nullopt;
  } else if (auto const *val = value.get_if<int32_t>()) {
    return *val >= 0 ? std::optional<uint64_t>(*val) : std::nullopt;
  } else if (auto const *val = value.get_if<int64_t>()) {
    return *val >= 0 ? std::optional<uint64_t>(*val) : std::nullopt;
  } else if (auto const *val = value.get_if<uint8_t>()) {
    return *val;
  } else if (auto const *val = value.get_if<uint16_t>()) {
    return *val;
  } else if (auto const *val = value.get_if<uint32_t>()) {
    return *val;
  } else if (auto const *val = value.get_if<uint64_t>()) {
    return *val;
  } else {
    UNREACHABLE(
        "All possibilities considered, given the precondition that the value "
        "holds an integral type: ",
        value);
  }
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
    ir::Value maybe_index_value = c.EvaluateOrDiagnose(
        type::Typed<ast::Expression const *>(node->rhs(), index_qt.type()));
    if (maybe_index_value.empty()) { return type::QualType::Error(); }

    std::optional<uint64_t> maybe_index = IntegralToUint64(maybe_index_value);
    if (not maybe_index.has_value()) {
      c.diag().Consume(NegativeArrayIndex{
          .range = node->range(),
      });
      qt.MarkError();
    } else if (*maybe_index >= array_type->length()) {
      c.diag().Consume(IndexingArrayOutOfBounds{
          .range = node->range(),
          .array = array_type,
          .index = *maybe_index,
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
    diag().Consume(InvalidIndexing{
        .range = node->range(),
        .type  = lhs_qt.type(),
    });
    qt = type::QualType::Error();
  }

  return context().set_qual_type(node, qt);
}

}  // namespace compiler
