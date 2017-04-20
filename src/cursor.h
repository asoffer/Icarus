#ifndef ICARUS_CURSOR_H
#define ICARUS_CURSOR_H

#include "base/types.h"
#include "base/debug.h"
#include "util/pstr.h"

#include <vector>
#include <string>
#include <fstream>

namespace AST {
  struct Statements;
} // namespace AST

struct SourceFile {
  SourceFile(const std::string &file_name = "")
      : name(file_name), ast(nullptr), ifs(name, std::ifstream::in) {}
  ~SourceFile() { ifs.close(); }

  std::string name;
  std::vector<pstr> lines;
  AST::Statements *ast;
  std::ifstream ifs;
};

struct Cursor {
  Cursor() : offset(0), line_num(0), source_file(nullptr) {}

  pstr line;
  size_t offset;
  size_t line_num;
  SourceFile *source_file;

  std::string file_name() const { return source_file->name; }

  // Get the character that the cursor is currently pointing to
  char &operator*(void)const { return *(line.ptr + offset); }

  void MoveToNextLine();

  static Cursor Behind(const Cursor &cursor, u64 dist);

  void SkipToEndOfLine() {
    while (**this != '\0') {
      ++offset;
    }
  }

  void BackUp() {
    // You can't back up to a previous line.
    ASSERT(offset > 0, "offset is negative.");
    --offset;
  }

  void Increment() {
    if (**this != '\0') {
      ++offset;
    } else {
      MoveToNextLine();
    }
  }
};

#endif // ICARUS_CURSOR_H
