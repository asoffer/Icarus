#include "core/parser.h"

#include "gtest/gtest.h"

namespace core {

struct CorrectFunctionTemplate {
  using match_type = base::type_list<int, bool, char>;
  static bool Parse(absl::Span<Lexeme const>&, auto&, auto&, auto&) {
    return true;
  }
};

TEST(BasicParser, Test) {
  struct MissingMatchType {
    static bool Parse(absl::Span<Lexeme const>&);
  };
  EXPECT_FALSE(Parser<MissingMatchType>);

  struct MissingParse {
    using match_type = base::type_list<int>;
  };
  EXPECT_FALSE(Parser<MissingParse>);

  struct Mismatched {
    using match_type = base::type_list<int>;
    static bool Parse(absl::Span<Lexeme const>&, float&);
  };
  EXPECT_FALSE(Parser<Mismatched>);

  struct IncorrectReturn {
    using match_type = base::type_list<int>;
    void Parse(absl::Span<Lexeme const>&, int&);
  };
  EXPECT_FALSE(Parser<IncorrectReturn>);

  struct NonStatic {
    using match_type = base::type_list<int>;
    bool Parse(absl::Span<Lexeme const>&, int&);
  };
  EXPECT_FALSE(Parser<NonStatic>);

  struct CorrectVoid {
    using match_type = base::type_list<>;
    static bool Parse(absl::Span<Lexeme const>&);
  };
  EXPECT_TRUE(Parser<CorrectVoid>);

  struct CorrectOneMatch {
    using match_type = base::type_list<int>;
    static bool Parse(absl::Span<Lexeme const>&, int&);
  };
  EXPECT_TRUE(Parser<CorrectOneMatch>);

  struct CorrectMultipleMatches {
    using match_type = base::type_list<int, bool, char>;
    static bool Parse(absl::Span<Lexeme const>&, int&, bool&, char&);
  };
  EXPECT_TRUE(Parser<CorrectMultipleMatches>);

  EXPECT_TRUE(Parser<CorrectFunctionTemplate>);
}

struct NotAParser {};

struct SimpleParser {
  using match_type = base::type_list<int>;
  static bool Parse(absl::Span<Lexeme const>&, int&);
};

struct MissingParser {
  static int bind();
};

struct MissingBind {
  static constexpr auto parser = SimpleParser();
};

struct AdaptedParser {
  static constexpr auto parser = SimpleParser();
  static double bind(int);
};

struct MismatchedBindAndParser {
  static constexpr auto parser = SimpleParser();
  static double bind(std::string_view);
};;

TEST(ParserAdaptor, Test) {
  EXPECT_FALSE(Parser<MissingParser>);
  EXPECT_FALSE(Parser<MissingBind>);
  EXPECT_TRUE(Parser<AdaptedParser>);
  EXPECT_FALSE(Parser<MismatchedBindAndParser>);
}

TEST(MatchType, Test) {
  EXPECT_EQ(base::meta<MatchType<SimpleParser>>,
            base::meta<base::type_list<int>>);
  EXPECT_EQ(base::meta<MatchType<AdaptedParser>>,
            base::meta<base::type_list<double>>);
}

}  // namespace core
