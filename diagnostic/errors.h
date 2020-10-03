#ifndef ICARUS_DIAGNOSTIC_ERRORS_H
#define ICARUS_DIAGNOSTIC_ERRORS_H

#include <experimental/source_location>
#include <string_view>
#include <vector>

#include "absl/strings/str_cat.h"
#include "core/fn_args.h"
#include "diagnostic/message.h"
#include "frontend/lex/lex.h"
#include "frontend/lex/numbers.h"
#include "frontend/source/file_name.h"
#include "frontend/source/range.h"
#include "frontend/source/source.h"
#include "ir/interpretter/evaluation_failure.h"
#include "type/qual_type.h"
#include "type/tuple.h"

namespace diagnostic {

struct MissingModule {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "missing-module";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(Text(
        "Could not find module named \"%s\" requested from %s", source.name(),
        requestor.empty() ? "command line"
                          : absl::StrCat("\"", requestor, "\".")));
  }

  frontend::CanonicalFileName source;
  std::string requestor;
};

struct Todo {
  static constexpr std::string_view kCategory = "todo";
  static constexpr std::string_view kName     = "todo";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(Text("TODO: Diagnostic emit from %s, line %u.",
                                  loc.file_name(), loc.line()));
  }

  std::experimental::source_location loc =
      std::experimental::source_location::current();
};

}  // namespace diagnostic

#endif  // ICARUS_DIAGNOSTIC_ERRORS_H
