#ifndef ICARUS_FRONTEND_FILE_SOURCE_H
#define ICARUS_FRONTEND_FILE_SOURCE_H

#include <cstdio>
#include <utility>

#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "base/expected.h"
#include "diagnostic/message.h"
#include "frontend/source/file_name.h"
#include "frontend/source/source.h"
#include "frontend/source/string.h"

namespace frontend {

struct MissingModule {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "missing-module";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(diagnostic::Text(
        "Could not find module named \"%s\" requested from %s:\n%s",
        source.name(),
        requestor.empty() ? "command line"
                          : absl::StrCat("\"", requestor, "\""),
        reason));
  }

  CanonicalFileName source;
  std::string requestor;
  std::string reason;
};

struct FileSource : public Source {
  static absl::StatusOr<FileSource> Make(CanonicalFileName file_name);

  FileSource(FileSource const &) = delete;
  FileSource(FileSource &&f)     = default;

  SourceChunk ReadUntil(char delim) override { return src_.ReadUntil(delim); }

  std::string_view line(LineNum line_num) const override {
    return src_.line(line_num);
  }

  SourceBuffer &buffer() override { return src_.buffer(); }
  SourceBuffer const &buffer() const override { return src_.buffer(); }

  std::string FileName() const override { return std::string(name_.name()); }

 private:
  FileSource(CanonicalFileName name, std::string src)
      : name_(std::move(name)), src_(std::move(src)) {}

  // TODO: Temporarily while migrating to SourceBuffer, use StringSource
  // internally. Eventually we will remove this type altogether.
  CanonicalFileName name_;
  StringSource src_;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_FILE_SOURCE_H
