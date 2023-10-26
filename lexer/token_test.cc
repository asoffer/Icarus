#include "lexer/token.h"

#include "common/identifier.h"
#include "nth/io/string_printer.h"
#include "nth/strings/format/universal.h"
#include "nth/strings/interpolate.h"
#include "nth/test/test.h"

namespace ic {
namespace {

std::string AsString(auto const& x) {
  std::string s;
  nth::string_printer p(s);
  nth::universal_formatter f({
      .depth    = 3,
      .fallback = "...",
  });
  nth::Interpolate<"{}">(p, f, x);
  return s;
}

NTH_TEST("token-kind/print") {
  NTH_EXPECT(AsString(Token::Kind::Identifier) == "tk.Identifier");
  NTH_EXPECT(AsString(Token::Kind::ColonColonEqual) == "tk.(::=)");
}

NTH_TEST("token/value") {
  NTH_EXPECT(Token::Identifier(5, Identifier(3)).kind() ==
             Token::Kind::Identifier);
  NTH_EXPECT(Token::Identifier(5, Identifier(3)).offset() == 5);

  NTH_EXPECT(
      Token::IntegerLiteral(5, Token::IntegerPayload::Immediate(3)).kind() ==
      Token::Kind::IntegerLiteral);
  NTH_EXPECT(
      Token::IntegerLiteral(5, Token::IntegerPayload::Immediate(3)).offset() ==
      5);

  NTH_EXPECT(Token::Symbol(Token::Kind::Colon, 4).kind() == Token::Kind::Colon);
  NTH_EXPECT(Token::Symbol(Token::Kind::Colon, 4).offset() == 4);
}

NTH_TEST("token/print") {
  NTH_EXPECT(AsString(Token::Identifier(5, Identifier(3))) ==
             "[tk.Identifier @5 #3]");
  NTH_EXPECT(
      AsString(Token::IntegerLiteral(5, Token::IntegerPayload::Index(3))) ==
      "[tk.IntegerLiteral @5 #3]");
  NTH_EXPECT(
      AsString(Token::IntegerLiteral(5, Token::IntegerPayload::Immediate(3))) ==
      "[tk.IntegerLiteral @5 !3]");
  NTH_EXPECT(AsString(Token::Symbol(Token::Kind::Colon, 3)) == "[tk.(:) @3]");
}

}  // namespace
}  // namespace ic
