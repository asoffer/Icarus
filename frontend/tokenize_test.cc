#include "test/test.h"

#include <memory>
#include <vector>

#include "frontend/source.h"
#include "frontend/tag.h"
#include "frontend/tokenize.h"

namespace frontend {
TaggedToken ConsumeWord(std::string_view* line);

namespace {
using ::matcher::Eq;
using ::matcher::IsEmpty;
using ::matcher::OrderedElementsAre;

TEST(ConsumeWord) {
  std::string_view line = "abc def";
  CHECK(ConsumeWord(&line) == TaggedToken(id, "abc"));

  line = "print xyz";
  CHECK(ConsumeWord(&line) == TaggedToken(op_l, "print"));
}

TEST(TokenizeEmpty) {
  auto src = StringSrc("");
  std::vector<TaggedToken> tks;
  CHECK(TokenizeLine(&src, &tks) == false);
  CHECK(tks, IsEmpty());
}

TEST(TokenizeWhitespace) {
  std::vector<TaggedToken> tks;

  auto src = StringSrc("    ");
  CHECK(TokenizeLine(&src, &tks) == false);
  CHECK(tks, IsEmpty());

  src = StringSrc("\t\t  \t");
  CHECK(TokenizeLine(&src, &tks) == false);
  CHECK(tks, IsEmpty());
}

TEST(TokenizeJustOneLine) {
  std::vector<TaggedToken> tks;

  auto src = StringSrc(R"(print abc
  def)");
  CHECK(TokenizeLine(&src, &tks) == true);
  CHECK(tks, OrderedElementsAre(Eq(TaggedToken{op_l, "print"}),
                                Eq(TaggedToken{id, "abc"})));
}

}  // namespace
}  // namespace frontend
