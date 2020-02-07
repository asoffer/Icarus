#include <array>
#include <string>
#include <string_view>

#include "diagnostic/consumer/trivial.h"
#include "frontend/parse.h"
#include "frontend/source/string.h"

static constexpr auto kTokens = std::array{
    // Keywords
    std::string_view{"which"}, std::string_view{"print"},
    std::string_view{"ensure"}, std::string_view{"needs"},
    std::string_view{"import"}, std::string_view{"flags"},
    std::string_view{"enum"}, std::string_view{"struct"},
    std::string_view{"return"}, std::string_view{"goto"},
    std::string_view{"jump"}, std::string_view{"switch"},
    std::string_view{"when"}, std::string_view{"as"}, std::string_view{"copy"},
    std::string_view{"move"},
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
    std::string_view{"bool"}, std::string_view{"int8"},
    std::string_view{"int16"}, std::string_view{"int32"},
    std::string_view{"int64"}, std::string_view{"nat8"},
    std::string_view{"nat16"}, std::string_view{"nat32"},
    std::string_view{"nat64"}, std::string_view{"float32"},
    std::string_view{"float64"}, std::string_view{"type"},
    std::string_view{"module"}, std::string_view{"byte_view"},
    // Other
    std::string_view{"block"}, std::string_view{"scope"},
    std::string_view{"#{export}"}, std::string_view{"\n"},
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
  string_source.reserve(size);

  for (size_t i = 0; i < length; ++i) {
    uint8_t val = data[i];
    if (val >= kTokens.size()) { continue; }
    string_source.append(kTokens[val]);
    string_source.append(" ");
  }

  std::cerr << "=============== SOURCE ===============\n"
            << string_source
            << "\n======================================\n";

      frontend::StringSource src(std::move(string_source));
  diagnostic::TrivialConsumer diag;
  frontend::Parse(&src, diag);

  return 0;
}
