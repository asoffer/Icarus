#ifndef ICARUS_FRONTEND_FILE_SOURCE_H
#define ICARUS_FRONTEND_FILE_SOURCE_H

#include <cstdio>
#include <filesystem>
#include <iostream>

#include "base/expected.h"
#include "frontend/source/source.h"

namespace frontend {

struct FileSource : public Source {
  static base::expected<FileSource> Make(std::filesystem::path path);

  FileSource(FileSource const &) = delete;
  FileSource(FileSource &&f) : path_(std::move(f.path_)), f_(f.f_) {
    f.f_ = nullptr;
  }
  ~FileSource() override {
    std::free(buf_);
    if (f_) { std::fclose(f_); }
  }

  SourceChunk ReadUntil(char delim) override {
    std::free(buf_);
    buf_              = nullptr;
    size_t n          = 0;
    ssize_t num_chars = getdelim(&buf_, &n, delim, f_);
    if (num_chars <= 0) { return {"", false}; }
    if (buf_[num_chars - 1] == delim) {
      return {std::string_view(buf_, num_chars - 1), true};
    } else {
      return {std::string_view(buf_, num_chars), true};
    }
  }

  std::vector<std::string> LoadLines() override {
    std::vector<std::string> lines{1};

    auto src = *FileSource::Make(path());
    while (true) {
      auto chunk = src.ReadUntil('\n');
      if (chunk.view.empty() && !chunk.more_to_read) { return lines; }
      lines.emplace_back(chunk.view);
    }
  }

  std::filesystem::path path() const { return path_; }

 private:
  FileSource(std::filesystem::path path, FILE *f)
      : path_(std::move(path)), f_(f) {}

  std::filesystem::path path_;
  FILE *f_   = nullptr;
  char *buf_ = nullptr;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_FILE_SOURCE_H
