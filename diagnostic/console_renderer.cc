#include "console_renderer.h"

namespace diagnostic {

namespace {
inline int NumDigits(frontend::LineNum line) {
  auto n = line.value;
  if (n == 0) { return 1; }
  int counter = 0;
  while (n != 0) {
    n /= 10;
    ++counter;
  }
  return counter;
}

static absl::flat_hash_map<frontend::Source *, std::vector<std::string>> lines;
std::string const &LoadLine(frontend::Source *src, frontend::LineNum line) {
  auto iter = lines.find(src);
  if (iter == lines.end()) {
    iter = lines.emplace(src, src->LoadLines()).first;
  }
  return iter->second[line.value];
}

}  // namespace

void ConsoleRenderer::Flush() {
  if (!has_data_) { return; }
  has_data_ = false;
  std::fputs("\n\n", out_);
  std::fflush(out_);
}

void ConsoleRenderer::WriteSourceQuote(SourceQuote const& quote) {
  int border_alignment = NumDigits(quote.lines.endpoints_.back() - 1) + 2;
  for (base::Interval<frontend::LineNum> line_range : quote.lines) {
    for (frontend::LineNum line = line_range.begin(); line != line_range.end();
         ++line) {
      std::fprintf(out_, "\033[97;1m%*d | \033[0m%s\n",  //
                   border_alignment, line.value,         //
                   LoadLine(quote.source, line).c_str());
    }
  }
}

}  // namespace diagnostic
