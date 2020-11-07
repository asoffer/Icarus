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
                         type.to_string(), index_type.to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
  type::Type type;
  type::Type index_type;
};

struct NonConstantTupleIndex {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-constant-tuple-index";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Index into a tuple must be a compile-time constant."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

struct IndexingTupleOutOfBounds {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "indexing-tuple-out-of-bounds";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Tuple is indexed out of bounds. Tuple of type `%s` has size %u "
            "but you are attempting to access position %d.",
            tuple->to_string(), tuple->entries_.size(), index),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
  type::Tuple const *tuple;
  uint64_t index;
};

struct IndexingArrayOutOfBounds {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "indexing-array-out-of-bounds";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Array is indexed out of bounds. Array of type `%s` has size %u "
            "but you are attempting to access position %d.",
            array->to_string(), array->length(), index),
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

struct IndexingStringOutOfBounds {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "indexing-string-out-of-bounds";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "String-literal is indexed out of bounds. String has length %u but "
            "you are attempting to access position %u.",
            length, index),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
  uint64_t length;
  uint64_t index;
};

struct NegativeStringIndex {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "negative-string-index";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Byte-view is indexed with a negative value."),
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
            type.to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
  type::Type type;
};

// Validate the index type, possibly emitting diagnostics
bool ValidIndexType(Compiler *c, ast::Index const *node, type::Type type,
                    type::QualType index_qt) {
  if (index_qt.ok()) {
    if (type::IsIntegral(index_qt.type())) { return true; }
    c->diag().Consume(InvalidIndexType{
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

type::QualType VerifyByteViewIndex(Compiler *c, ast::Index const *node,
                                   type::Quals quals, type::QualType index_qt) {
  quals = (quals & index_qt.quals()) | type::Quals::Buf();
  type::QualType qt(type::Nat8, quals);
  if (not ValidIndexType(c, node, type::ByteView, index_qt)) {
    qt.MarkError();
    return c->context().set_qual_type(node, qt);
  }

  if (index_qt.constant()) {
    ir::Value maybe_index_value = c->EvaluateOrDiagnose(
        type::Typed<ast::Expression const *>(node->rhs(), index_qt.type()));

    if (quals >= type::Quals::Const()) {
      auto maybe_str = c->EvaluateOrDiagnoseAs<ir::String>(node->lhs());
      if (maybe_index_value.empty() or not maybe_str) {
        return type::QualType::Error();
      }

      std::optional<uint64_t> maybe_index = IntegralToUint64(maybe_index_value);
      if (not maybe_index.has_value()) {
        c->diag().Consume(NegativeStringIndex{
            .range = node->range(),
        });
        qt.MarkError();
      } else if (*maybe_index >= maybe_str->get().size()) {
        c->diag().Consume(IndexingStringOutOfBounds{
            .range  = node->range(),
            .length = maybe_str->get().size(),
            .index  = *maybe_index,
        });
        qt.MarkError();
      }
    }
  }

  return c->context().set_qual_type(node, qt);
}

type::QualType VerifyArrayIndex(Compiler *c, ast::Index const *node,
                                type::Array const *array_type,
                                type::Quals quals, type::QualType index_qt) {
  if (index_qt.quals() <= ~type::Quals::Const()) {
    quals &= ~type::Quals::Const();
  }
  type::Type data_type = array_type->data_type();
  type::QualType qt(data_type, quals);

  if (not ValidIndexType(c, node, array_type, index_qt)) {
    qt.MarkError();
    return c->context().set_qual_type(node, qt);
  }

  if (index_qt.constant()) {
    ir::Value maybe_index_value = c->EvaluateOrDiagnose(
        type::Typed<ast::Expression const *>(node->rhs(), index_qt.type()));
    if (maybe_index_value.empty()) { return type::QualType::Error(); }

    std::optional<uint64_t> maybe_index = IntegralToUint64(maybe_index_value);
    if (not maybe_index.has_value()) {
      c->diag().Consume(NegativeArrayIndex{
          .range = node->range(),
      });
      qt.MarkError();
    } else if (*maybe_index >= array_type->length()) {
      c->diag().Consume(IndexingArrayOutOfBounds{
          .range = node->range(),
          .array = array_type,
          .index = *maybe_index,
      });
      qt.MarkError();
    }
  }
  return c->context().set_qual_type(node, qt);
}

type::QualType VerifyBufferPointerIndex(Compiler *c, ast::Index const *node,
                                        type::BufferPointer const *buf_ptr_type,
                                        type::Quals quals,
                                        type::QualType index_qt) {
  quals = (quals & index_qt.quals()) | type::Quals::Buf();
  type::QualType qt(buf_ptr_type->pointee(), quals);
  if (not ValidIndexType(c, node, buf_ptr_type, index_qt)) { qt.MarkError(); }
  return c->context().set_qual_type(node, qt);
}

type::QualType VerifyTupleIndex(Compiler *c, ast::Index const *node,
                                type::Tuple const *tuple_type,
                                type::Quals quals, type::QualType index_qt) {
  if (not index_qt.constant()) {
    c->diag().Consume(NonConstantTupleIndex{
        .range = node->range(),
    });
    return type::QualType::Error();
  }

  if (not ValidIndexType(c, node, tuple_type, index_qt)) {
    return type::QualType::Error();
  }

  ir::Value maybe_value = c->EvaluateOrDiagnose(
      type::Typed<ast::Expression const *>(node->rhs(), index_qt.type()));
  if (maybe_value.empty()) { return type::QualType::Error(); }

  std::optional<uint64_t> maybe_index = IntegralToUint64(maybe_value);
  if (not maybe_index.has_value() or *maybe_index >= tuple_type->size()) {
    c->diag().Consume(IndexingTupleOutOfBounds{
        .range = node->range(),
        .tuple = tuple_type,
        .index = *maybe_index,
    });
    return type::QualType::Error();
  }

  quals = (quals & index_qt.quals()) | type::Quals::Ref();
  type::QualType qt(tuple_type->entries_[*maybe_index], quals);
  return c->context().set_qual_type(node, qt);
}

}  // namespace

type::QualType Compiler::VerifyType(ast::Index const *node) {
  auto lhs_qt   = VerifyType(node->lhs());
  auto index_qt = VerifyType(node->rhs());

  // We can recover from the index having errors but not from the data being
  // indexed.
  if (not lhs_qt.ok()) { return type::QualType::Error(); }

  if (lhs_qt.type() == type::ByteView) {
    return VerifyByteViewIndex(this, node, lhs_qt.quals(), index_qt);
  } else if (auto const *array_type = lhs_qt.type().if_as<type::Array>()) {
    return VerifyArrayIndex(this, node, array_type, lhs_qt.quals(), index_qt);
  } else if (auto const *buf_ptr_type =
                 lhs_qt.type().if_as<type::BufferPointer>()) {
    return VerifyBufferPointerIndex(this, node, buf_ptr_type, lhs_qt.quals(),
                                    index_qt);
  } else if (auto const *tuple_type = lhs_qt.type().if_as<type::Tuple>()) {
    return VerifyTupleIndex(this, node, tuple_type, lhs_qt.quals(), index_qt);
  } else {
    diag().Consume(InvalidIndexing{
        .range = node->range(),
        .type  = lhs_qt.type(),
    });
    return type::QualType::Error();
  }
}

}  // namespace compiler
