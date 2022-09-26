#include <deque>
#include <optional>
#include <ostream>
#include <string_view>

#include "absl/strings/str_split.h"
#include "ast/module.h"
#include "diagnostic/consumer/tracking.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "jasmin/execute.h"
#include "semantic_analysis/byte_code/instruction_set.h"
#include "semantic_analysis/context.h"
#include "semantic_analysis/type_system.h"

namespace test {

struct Repl {
 private:
  struct ResultBase {
    ResultBase(std::string_view content) : content_(content) {}

   public:
    friend std::ostream& operator<<(std::ostream& os, ResultBase const& r) {
      os << "A code snippet\n";
      std::vector<std::string_view> lines =
          absl::StrSplit(r.content_, absl::ByChar('\n'));
      size_t indentation = std::numeric_limits<size_t>::max();
      for (std::string_view line : lines) {
        indentation = std::min(indentation, line.find_first_not_of(" \t"));
      }
      size_t length =
          std::max_element(lines.begin(), lines.end(),
                           [](std::string_view l, std::string_view r) {
                             return l.size() < r.size();
                           })
              ->size() -
          indentation;

      std::string boundary(length + 2, '-');
      os << "    +" << boundary << "+\n";
      for (std::string_view line : lines) {
        if (line.size() > indentation) { line.remove_prefix(indentation); }
        ASSERT(length >= line.size());
        os << "    | " << line << std::string(length - line.size(), ' ')
           << " |\n";
      }
      return os << "    +" << boundary << "+\n";
    }

    std::string_view content_;
  };

 public:
  struct ExecuteResult : ResultBase {
    semantic_analysis::IrFunction const* function() const {
      if (value_) { return &*value_; }
      return nullptr;
    }

   private:
    friend Repl;

    explicit ExecuteResult(std::string_view content)
        : ResultBase(content), value_(std::nullopt) {}
    explicit ExecuteResult(std::string_view content,
                           semantic_analysis::IrFunction f)
        : ResultBase(content), value_(std::move(f)) {}

    std::optional<semantic_analysis::IrFunction> value_;
  };

  struct TypeCheckResult : ResultBase {
    absl::Span<std::pair<std::string, std::string> const> diagnostics() const {
      return diagnostics_;
    }

    absl::Span<semantic_analysis::QualifiedType const> qualified_types() const {
      return qts_;
    }

    friend std::ostream& operator<<(std::ostream& os,
                                    TypeCheckResult const& r) {
      os << static_cast<ResultBase const&>(r);
      std::string_view separator =
          "    where the qualified type of the last expression is ";
      for (auto const& qt : r.qualified_types()) {
        os << std::exchange(separator, ", ");
        r.repl_.PrintQualifiedType(os, qt);
      }
      if (r.diagnostics().empty()) {
        return os << "\n    with no diagnostics.\n";
      }
      os << "\n    with diagnostics:\n";
      for (auto const& [category, name] : r.diagnostics()) {
        os << "      * [" << category << ": " << name << "]\n";
      }
      return os;
    }

   private:
    friend Repl;

    explicit TypeCheckResult(
        std::string_view content,
        absl::Span<semantic_analysis::QualifiedType const> qts,
        absl::Span<std::pair<std::string, std::string> const> diagnostics,
        Repl& repl)
        : ResultBase(content),
          qts_(qts.begin(), qts.end()),
          diagnostics_(diagnostics.begin(), diagnostics.end()),
          repl_(repl) {}

    std::vector<semantic_analysis::QualifiedType> qts_;
    std::vector<std::pair<std::string, std::string>> diagnostics_;
    Repl& repl_;
  };

  ExecuteResult execute(std::string source);
  TypeCheckResult type_check(std::string source);

  auto& type_system() { return type_system_; }

 private:
  void PrintQualifiedType(std::ostream& os,
                          semantic_analysis::QualifiedType qt);
  void PrintType(std::ostream& os, core::Type t);

  std::deque<std::string> source_content_;
  ast::Module ast_module_{nullptr};
  semantic_analysis::Context context_;
  semantic_analysis::TypeSystem type_system_;
  diagnostic::TrackingConsumer consumer_;
};

template <typename T>
struct EvaluatesToMatcher {
  explicit EvaluatesToMatcher(testing::Matcher<T> m) : m_(std::move(m)) {}

  using is_gtest_matcher = void;

  bool MatchAndExplain(Repl::ExecuteResult const& value,
                       testing::MatchResultListener* listener) const {
    auto const* f = value.function();
    if (not f) { return false; }
    T result;
    jasmin::Execute(*f, {}, result);
    return testing::ExplainMatchResult(m_, result, listener);
  }

  void DescribeTo(std::ostream* os) const {
    *os << "evaluates to a value that ";
    m_.DescribeTo(os);
  }

  void DescribeNegationTo(std::ostream* os) const {
    *os << "does not evaluate to a value that ";
    m_.DescribeTo(os);
  }

 private:
  testing::Matcher<T> m_;
};

template <typename T, typename Inner>
EvaluatesToMatcher<T> EvaluatesTo(Inner&& inner) {
  return EvaluatesToMatcher<T>(std::move(inner));
}

template <int&..., typename T>
auto EvaluatesTo(T&& value) {
  return EvaluatesTo<std::decay_t<T>>(testing::Eq(std::forward<T>(value)));
}

struct HasDiagnostics {
 private:
  using inner_matcher_type =
      std::vector<testing::Matcher<std::pair<std::string, std::string>>>;

 public:
  using is_gtest_matcher = void;
  template <typename... Ms>
  explicit HasDiagnostics(Ms&&... ms)
      : ms_(testing::UnorderedElementsAreArray(
            inner_matcher_type{std::forward<Ms>(ms)...})) {}

  bool MatchAndExplain(Repl::TypeCheckResult const& value,
                       testing::MatchResultListener* listener) const {
    return testing::ExplainMatchResult(ms_, value.diagnostics(), listener);
  }

  void DescribeTo(std::ostream* os) const {
    *os << "has diagnostics that ";
    static_cast<
        testing::Matcher<std::vector<std::pair<std::string, std::string>>>>(ms_)
        .DescribeTo(os);
  }

  void DescribeNegationTo(std::ostream* os) const {
    *os << "does not have diagnostics that ";
    static_cast<
        testing::Matcher<std::vector<std::pair<std::string, std::string>>>>(ms_)
        .DescribeTo(os);
  }

 private:
  using matcher_type = decltype(::testing::UnorderedElementsAreArray(
      std::declval<std::vector<
          testing::Matcher<std::pair<std::string, std::string>>> const&>()));

  matcher_type ms_;
};

struct HasQualTypes {
 private:
  using inner_matcher_type =
      std::vector<testing::Matcher<semantic_analysis::QualifiedType>>;

 public:
  using is_gtest_matcher = void;

  template <typename... Ms>
  explicit HasQualTypes(Ms&&... ms)
      : ms_(testing::ElementsAreArray(
            inner_matcher_type{std::forward<Ms>(ms)...})) {}

  bool MatchAndExplain(Repl::TypeCheckResult const& value,
                       testing::MatchResultListener* listener) const {
    return testing::ExplainMatchResult(ms_, value.qualified_types(), listener);
  }

  void DescribeTo(std::ostream* os) const {
    *os << "has qualified types that ";
    static_cast<
        testing::Matcher<std::vector<semantic_analysis::QualifiedType>>>(ms_)
        .DescribeTo(os);
  }

  void DescribeNegationTo(std::ostream* os) const {
    *os << "does not have qualified types that ";
    static_cast<
        testing::Matcher<std::vector<semantic_analysis::QualifiedType>>>(ms_)
        .DescribeTo(os);
  }

 private:
  using matcher_type =
      decltype(::testing::ElementsAreArray(std::declval<inner_matcher_type>()));

  matcher_type ms_;
};

}  // namespace test
