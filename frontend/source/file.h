#ifndef ICARUS_FRONTEND_FILE_SOURCE_H
#define ICARUS_FRONTEND_FILE_SOURCE_H

#include <string>

#include "absl/status/statusor.h"
#include "diagnostic/message.h"
#include "frontend/source/file_name.h"

namespace frontend {

struct MissingModule {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "missing-module";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(diagnostic::Text(
        "Could not find module named \"%s\":\n%s", source.name(), reason));
  }

  CanonicalFileName source;
  std::string requestor;  // TODO: Set this correctly or remove it.
  std::string reason;
};

absl::StatusOr<SourceBuffer> SourceBufferFromFile(
    CanonicalFileName const &file_name);

}  // namespace frontend

#endif  // ICARUS_FRONTEND_FILE_SOURCE_H
