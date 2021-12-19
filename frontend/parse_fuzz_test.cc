#include <array>
#include <string>
#include <string_view>

#include "base/log.h"
#include "diagnostic/consumer/trivial.h"
#include "frontend/parse.h"
#include "frontend/source/buffer.h"

static constexpr auto kTokens = std::array{
    // Keywords
    std::string_view{"which"}, std::string_view{"import"},
    std::string_view{"flags"}, std::string_view{"enum"},
    std::string_view{"struct"}, std::string_view{"return"},
    std::string_view{"as"}, std::string_view{"copy"}, std::string_view{"move"},
    // Operators
    std::string_view{"@"}, std::string_view{","}, std::string_view{"[*]"},
    std::string_view{"$"}, std::string_view{"+="}, std::string_view{"+"},
    std::string_view{"--"}, std::string_view{"-="}, std::string_view{"->"},
    std::string_view{"-"}, std::string_view{"*="}, std::string_view{"*"},
    std::string_view{"%="}, std::string_view{"%"}, std::string_view{"&="},
    std::string_view{"&"}, std::string_view{"|="}, std::string_view{"|"},
    std::string_view{"^="}, std::string_view{"^"}, std::string_view{">="},
    std::string_view{">"}, std::string_view{"!"}, std::string_view{"::="},
    std::string_view{":?"}, std::string_view{"::"}, std::string_view{":="},
    std::string_view{".."}, std::string_view{":"}, std::string_view{"<<"},
    std::string_view{"<="}, std::string_view{"<"}, std::string_view{"!="},
    std::string_view{"=="}, std::string_view{"=>"}, std::string_view{"="},
    std::string_view{"'"}, std::string_view{"("}, std::string_view{")"},
    std::string_view{"["}, std::string_view{"]"}, std::string_view{"{"},
    std::string_view{"}"}, std::string_view{";"}, std::string_view{"."},
    // Types
    std::string_view{"bool"}, std::string_view{"i8"}, std::string_view{"i16"},
    std::string_view{"i32"}, std::string_view{"i64"}, std::string_view{"u8"},
    std::string_view{"u16"}, std::string_view{"u32"}, std::string_view{"u64"},
    std::string_view{"f32"}, std::string_view{"f64"}, std::string_view{"type"},
    std::string_view{"module"},
    // Other
    std::string_view{"block"}, std::string_view{"scope"},
    std::string_view{"#{export}"}, std::string_view{R"(\)"},
    std::string_view{R"(\\)"}, std::string_view{"\n"},
    // Values
    std::string_view{"true"}, std::string_view{"false"},
    std::string_view{"null"}, std::string_view{"1234"},
    std::string_view{"1234.5"}, std::string_view{R"("abc")"},
    std::string_view{"#.label"}};

extern "C" int LLVMFuzzerTestOneInput(uint8_t const* data, size_t length) {
  size_t size = 0;
  for (size_t i = 0; i < length; ++i) {
    uint8_t val = data[i];
    if (val >= kTokens.size()) { continue; }
    size += kTokens[val].size() + 1;
  }
  std::string string_source;

  for (size_t i = 0; i < length; ++i) {
    uint8_t val = data[i];
    if (val >= kTokens.size()) { continue; }
    string_source.append(kTokens[val]);
    string_source.append(" ");
  }

  LOG("", R"(
=============== SOURCE ===============
%s
======================================
)",
      string_source);

  frontend::SourceBuffer buffer(std::move(string_source));
  diagnostic::TrivialConsumer diag;
  frontend::Parse(buffer, diag);

  return 0;
}
