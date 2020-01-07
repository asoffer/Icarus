#ifndef ICARUS_FRONTEND_FILE_SOURCE_H
#define ICARUS_FRONTEND_FILE_SOURCE_H

#include <cstdio>
#include <utility>

#include "base/expected.h"
#include "base/strong_types.h"
#include "frontend/source/file_name.h"
#include "frontend/source/source.h"

namespace frontend {

struct FileSource : public Source {
  static base::expected<FileSource> Make(CanonicalFileName file_name);

  FileSource(FileSource const &) = delete;

  FileSource(FileSource &&f)
      : name_(std::move(f.name_)), f_(std::exchange(f.f_, nullptr)) {}

  ~FileSource() override { std::free(buf_); }

  SourceChunk ReadUntil(char delim) override {
    std::free(buf_);
    buf_              = nullptr;
    size_t n          = 0;
    ssize_t num_chars = getdelim(&buf_, &n, delim, f_.get());
    if (num_chars <= 0) { return {"", false}; }
    if (buf_[num_chars - 1] == delim) {
      return {std::string_view(buf_, num_chars - 1), true};
    } else {
      return {std::string_view(buf_, num_chars), true};
    }
  }

  std::vector<std::string> LoadLines() override {
    std::vector<std::string> lines{1};

    auto src = *FileSource::Make(name_);
    while (true) {
      auto chunk = src.ReadUntil('\n');
      if (chunk.view.empty() and not chunk.more_to_read) { return lines; }
      lines.emplace_back(chunk.view);
    }
  }

 private:
  FileSource(CanonicalFileName name, file_handle_t f)
      : name_(std::move(name)), f_(std::move(f)) {}

  CanonicalFileName name_;
  file_handle_t f_;
  char *buf_ = nullptr;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_FILE_SOURCE_H
