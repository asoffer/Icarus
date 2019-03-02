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

std::vector<TaggedToken> TokenVec(char const *str) {
  std::vector<TaggedToken> results;
  StringSrc src(str);
  Tokenizer tkr(&src);
  do { results.push_back(tkr.Next()); } while (results.back().tag != eof);
  return results;
}

TEST(TokenizeEmpty) {
  CHECK(TokenVec(""), OrderedElementsAre(Eq(TaggedToken(newline, "\n")),
                                         Eq(TaggedToken(eof, ""))));
}

TEST(TokenizeIdentifier) {
  CHECK(TokenVec(R"(print abc
  def gh--ij-k--__)"),
        OrderedElementsAre(Eq(TaggedToken(newline, "\n")),  //
                           Eq(TaggedToken(op_l, "print")),  //
                           Eq(TaggedToken(id, "abc")),      //
                           Eq(TaggedToken(newline, "\n")),  //
                           Eq(TaggedToken(id, "def")),
                           Eq(TaggedToken(id, "gh--ij-k--__")),
                           Eq(TaggedToken(eof, ""))));
}

TEST(TokenizeWhitespace) {
  CHECK(TokenVec("    \t\t  \r\n\t  \n  \r\r"),
        OrderedElementsAre(Eq(TaggedToken(newline, "\n")),  //
                           Eq(TaggedToken(newline, "\n")),
                           Eq(TaggedToken(newline, "\n")),
                           Eq(TaggedToken(eof, ""))));
}

TEST(Slash) {
  CHECK(TokenVec(R"(    /
  /   /)"),
        OrderedElementsAre(Eq(TaggedToken(newline, "\n")),  //
                           Eq(TaggedToken(op_b, Operator::Div)),
                           Eq(TaggedToken(newline, "\n")),
                           Eq(TaggedToken(op_b, Operator::Div)),
                           Eq(TaggedToken(op_b, Operator::Div)),
                           Eq(TaggedToken(eof, ""))));
}

TEST(SingleLineComment) {
  CHECK(TokenVec("foo // ignore all of this"),
        OrderedElementsAre(Eq(TaggedToken(newline, "\n")),  //
                           Eq(TaggedToken(id, "foo")),      //
                           Eq(TaggedToken(eof, ""))));

  CHECK(TokenVec(R"(foo // ignore all of this
  bar)"),
        OrderedElementsAre(Eq(TaggedToken(newline, "\n")),  //
                           Eq(TaggedToken(id, "foo")),      //
                           Eq(TaggedToken(newline, "\n")),  //
                           Eq(TaggedToken(id, "bar")),      //
                           Eq(TaggedToken(eof, ""))));

  CHECK(TokenVec(R"(foo // ignore all of this // and this too
  bar)"),
        OrderedElementsAre(Eq(TaggedToken(newline, "\n")),  //
                           Eq(TaggedToken(id, "foo")),      //
                           Eq(TaggedToken(newline, "\n")),  //
                           Eq(TaggedToken(id, "bar")),      //
                           Eq(TaggedToken(eof, ""))));

  CHECK(TokenVec(R"(foo // ignore all of this \\ bar)"),
        OrderedElementsAre(Eq(TaggedToken(newline, "\n")),     //
                           Eq(TaggedToken(id, "foo")),         //
                           Eq(TaggedToken(newline, R"(\\)")),  //
                           Eq(TaggedToken(id, "bar")),         //
                           Eq(TaggedToken(eof, ""))));
}

}  // namespace
}  // namespace frontend
