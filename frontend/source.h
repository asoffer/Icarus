#ifndef ICARUS_FRONTEND_SOURCE_H
#define ICARUS_FRONTEND_SOURCE_H

#include <iostream>
#include <cstdio>
#include <filesystem>
#include <string>
#include <string_view>

#include "base/expected.h"

namespace frontend {

struct SrcChunk {
  std::string_view view = "";
  bool more_to_read     = true;
};

struct Src {
  virtual ~Src(){};

  // Reads data from the source until the delimeter is found, dropping the
  // delimeter Sources may have a maximum number of characters they will read.  Typically 1k.
  //
  // Returned data contains a string_view which is guaranteed to be valid until
  // the next call to ReadUntil.
  virtual SrcChunk ReadUntil(char delim) = 0;

  virtual std::vector<std::string> LoadLines() = 0;
};

struct StringSrc : public Src {
  ~StringSrc() override {}
  StringSrc(std::string src) : src_(std::move(src)), view_(src_) {}

  SrcChunk ReadUntil(char delim) override {
    auto pos = view_.find_first_of(delim);
    if (pos == std::string_view::npos) { return {view_, false}; }
    auto result = view_.substr(0, pos);
    view_.remove_prefix(pos + 1);
    return {result, !view_.empty()};
  }

  std::vector<std::string> LoadLines() override {
    std::vector<std::string> lines{1};

    std::string_view all{src_};
    auto pos = all.find_first_of('\n');
    while (pos != std::string_view::npos) {
      lines.push_back(std::string{all.substr(0, pos)});
      all.remove_prefix(pos + 1);
      pos = all.find_first_of('\n');
    }
    lines.push_back(std::string{all});

    return lines;
  }

 private:
  std::string src_;
  std::string_view view_;
};

struct ReplSrc : public Src {
  SrcChunk ReadUntil(char delim) override {
    if (view_.empty()) {
      std::cout << "\n>> ";
      std::getline(std::cin, lines_.emplace_back());
      view_ = lines_.back();
    }

    auto pos = view_.find_first_of(delim);
    while (pos == std::string_view::npos) {
      std::cout << "\n.. ";
      // Compute how far `view_` was inside the line, because after we append to
      // the line we need to reset `view_` appropriately.
      int dist = view_.data() - lines_.back().data();
      std::string line;
      std::getline(std::cin, line);
      lines_.back().append(line);
      view_ = lines_.back();
      view_.remove_prefix(dist);
      pos = view_.find_first_of(delim);
    }

    auto result = view_.substr(0, pos);
    view_.remove_prefix(pos + 1);

    return {result, !view_.empty()};
  }

  std::vector<std::string> LoadLines() override { return lines_; }

  std::vector<std::string> lines_;
  std::string_view view_;
};

struct FileSrc : public Src {
  static base::expected<FileSrc> Make(std::filesystem::path path);

  FileSrc(FileSrc const&) = delete;
  FileSrc(FileSrc &&f) : path_(std::move(f.path_)), f_(f.f_) { f.f_ = nullptr; }
  ~FileSrc() override {
    std::free(buf_);
    if (f_) { std::fclose(f_); }
  }

  SrcChunk ReadUntil(char delim) override {
    std::free(buf_);
    buf_ = nullptr;
    size_t n = 0;
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

    auto src = *FileSrc::Make(path());
    while (true) {
      auto chunk = src.ReadUntil('\n');
      if (chunk.view.empty() && !chunk.more_to_read) { return lines; }
      lines.emplace_back(chunk.view);
    }
  }

  std::filesystem::path path() const { return path_; }

 private:
  FileSrc(std::filesystem::path path, FILE *f)
      : path_(std::move(path)), f_(f) {}

  std::filesystem::path path_;
  FILE *f_   = nullptr;
  char *buf_ = nullptr;
};


}  // namespace frontend

#endif  // ICARUS_FRONTEND_SOURCE_H
