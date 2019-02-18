#ifndef ICARUS_FRONTEND_SOURCE_H
#define ICARUS_FRONTEND_SOURCE_H

#include <cstdio>
#include <filesystem>
#include <string>
#include <string_view>

#include "base/expected.h"
#include "base/log.h"

namespace frontend {

struct SrcChunk {
  std::string_view view = "";
  bool more_to_read     = true;
};

struct Src {
  virtual ~Src(){};

  // Data is guaranteed never to be cut in the middle of a token. It is
  // guaranteed to be valid until the next call to Read.
  virtual SrcChunk ReadChunk() = 0;
};

struct StringSrc : public Src {
  static constexpr uint16_t MaxChunkSize = 1024;

  ~StringSrc() override {}
  StringSrc(std::string src)
      : src_(std::move(src)), head_(src_.data()), size_left_(src_.size()){};

  SrcChunk ReadChunk() override {
    if (size_left_ >= MaxChunkSize) {
      std::string_view result(head_, MaxChunkSize);
      head_ += MaxChunkSize;
      size_left_ -= MaxChunkSize;
      return {result, true};
    } else if (size_left_ == 0) {
      return {"", false};
    } else {
      std::string_view result(head_, size_left_);
      head_ += size_left_;
      size_left_ = 0;
      return {result, false};
    }
  }

 private:
  std::string src_;
  char const *head_;
  size_t size_left_;
};

template <size_t MaxChunkSize>
struct FileSrc : public Src {
  FileSrc(std::filesystem::path path, FILE *f)
      : path_(std::move(path)), f_(f) {}
  FileSrc(FileSrc const&) = delete;
  FileSrc(FileSrc &&f) : path_(std::move(f.path_)), f_(f.f_) { f.f_ = nullptr; }
  ~FileSrc() override {
    if (f_) { std::fclose(f_); }
  }

  SrcChunk ReadChunk() override {
    // Note: Let string_view deduce the length of the buffer because we may not
    // have used the full `MaxChunkSize` and a null terminator may have been
    // written earlier.
    size_t num_read = std::fread(&buf_[0], sizeof(buf_[0]), MaxChunkSize - 1, f_);
    buf_[num_read] = '\0';
    return {std::string_view(&buf_[0], num_read), num_read == MaxChunkSize - 1};
  }

 private:
  template <size_t N>
  friend base::expected<FileSrc<N>> MakeFileSrc(std::filesystem::path path);
  std::filesystem::path path_;
  FILE *f_ = nullptr;
  char buf_[MaxChunkSize];
};

template <size_t MaxChunkSize = 1024>
base::expected<FileSrc<MaxChunkSize>> MakeFileSrc(std::filesystem::path path) {
  FILE *f = std::fopen(path.c_str(), "r");
  if (!f) {
    return base ::unexpected(std::string("Unable to open file \"") +
                             path.string() + "\"");
  }
  FileSrc<MaxChunkSize> src(std::move(path), f);
  return src;
}

}  // namespace frontend

#endif  // ICARUS_FRONTEND_SOURCE_H
