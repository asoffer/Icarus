#include "test/repl.h"

#include "ast/expression.h"
#include "compiler/context.h"
#include "frontend/parse.h"
#include "jasmin/execute.h"
#include "semantic_analysis/byte_code/byte_code.h"
#include "semantic_analysis/type_verification/verify.h"

namespace test {

Repl::ExecuteResult Repl::execute(std::string content) {
  source_content_.push_back(std::move(content));
  auto nodes              = frontend::Parse(source_content_.back(), consumer_);
  base::PtrSpan node_span = ast_module_.insert(nodes.begin(), nodes.end());
  if (consumer_.num_consumed() != 0) {
    return ExecuteResult(source_content_.back());
  }

  semantic_analysis::TypeVerifier tv(type_system_, context_, consumer_);
  for (auto const* node : node_span) { tv.schedule(node); }
  tv.complete();

  if (consumer_.num_consumed() != 0) {
    return ExecuteResult(source_content_.back());
  }

  auto const& expr = node_span.back()->as<ast::Expression>();
  semantic_analysis::IrFunction f =
      semantic_analysis::EmitByteCode(expr, context_, type_system_);
  return ExecuteResult(source_content_.back(), std::move(f));
}

Repl::TypeCheckResult Repl::type_check(std::string content) {
  source_content_.push_back(std::move(content));
  auto nodes              = frontend::Parse(source_content_.back(), consumer_);
  base::PtrSpan node_span = ast_module_.insert(nodes.begin(), nodes.end());
  if (consumer_.num_consumed() != 0) {
    return TypeCheckResult(source_content_.back(), {semantic_analysis::Error()},
                           consumer_.diagnostics(), *this);
  }

  semantic_analysis::TypeVerifier tv(type_system_, context_, consumer_);
  for (auto const* node : node_span) { tv.schedule(node); }
  tv.complete();

  return TypeCheckResult(
      source_content_.back(),
      context_.qualified_types(&node_span.back()->as<ast::Expression>()),
      consumer_.diagnostics(), *this);
}

void Repl::PrintQualifiedType(std::ostream& os,
                              semantic_analysis::QualifiedType qt) {
  std::string_view separator = "";
  semantic_analysis::Qualifiers qualifiers = qt.qualifiers();
  if (qualifiers >= semantic_analysis::Qualifiers::Constant()) {
    os << std::exchange(separator, "-") << "constant";
  }
  if (qualifiers >= semantic_analysis::Qualifiers::Buffer()) {
    os << std::exchange(separator, "-") << "buffer";
  } else if (qualifiers >= semantic_analysis::Qualifiers::Reference()) {
    os << std::exchange(separator, "-") << "reference";
  }
  if (qualifiers >= semantic_analysis::Qualifiers::Error()) {
    os << std::exchange(separator, "-") << "error";
  }
  os << '(';
  PrintType(os, qt.type());
  os << ')';
}

void Repl::PrintType(std::ostream& os, core::Type t) {
  if (auto p = t.get_if<semantic_analysis::PrimitiveType>(type_system_)) {
    static constexpr std::array kPrimitiveTypes{"bool",    "char",   "byte",
                                                "f32",     "f64",    "type",
                                                "integer", "module", "error"};
    os << kPrimitiveTypes[static_cast<uint8_t>(p->value())];
  } else if (auto p = t.get_if<core::PointerType>(type_system_)) {
    os << "*(";
    PrintType(os, p->pointee());
    os << ')';
  } else if (auto p =
                 t.get_if<semantic_analysis::BufferPointerType>(type_system_)) {
    os << "[*](";
    PrintType(os, p->pointee());
    os << ')';
  } else if (auto i = t.get_if<core::SizedIntegerType>(type_system_)) {
    os << (i->is_signed() ? 'i' : 'u') << i->bits();
    // TODO: Show alignment.
  } else {
    os << "???";
  }
}

}  // namespace test
