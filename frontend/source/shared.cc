#include "frontend/source/shared.h"
#include "base/no_destructor.h"

namespace frontend {
namespace {

struct SharedSourceType : Source {
  SourceChunk ReadUntil(char delim) override { UNREACHABLE(); }
  std::string_view line(size_t line_num) const override { UNREACHABLE(); }
  std::string FileName() const override { UNREACHABLE(); }
  SourceBuffer& buffer() override { UNREACHABLE(); }
  SourceBuffer const& buffer() const override { UNREACHABLE(); }
};

}  // namespace

frontend::Source const *SharedSource() {
  static base::NoDestructor<SharedSourceType> src;
  return &*src;
}

}  // namespace frontend
