#include "frontend/source/shared.h"

namespace frontend {
namespace {

struct SharedSourceType : Source {
  SourceChunk ReadUntil(char delim) override { UNREACHABLE(); }
  std::vector<std::string> LoadLines() const override { UNREACHABLE(); }
};

}  // namespace

frontend::Source const *SharedSource() {
  static auto *src = new SharedSourceType;
  return src;
}

}  // namespace frontend
