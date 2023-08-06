#include <algorithm>
#include <deque>
#include <optional>
#include <ostream>
#include <string_view>
#include <vector>

#include "absl/functional/function_ref.h"
#include "absl/strings/str_split.h"
#include "ast/module.h"
#include "compiler/compiler.h"
#include "diagnostic/consumer/tracking.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "jasmin/execute.h"
#include "module/module.h"
#include "module/resources.h"
#include "nth/io/string_printer.h"
#include "nth/strings/format/universal.h"
#include "semantic_analysis/context.h"
#include "semantic_analysis/type_system.h"
#include "serialization/foreign_symbol_map.h"
#include "vm/execute.h"
#include "vm/function.h"

namespace test {

inline diagnostic::TrackingConsumer tracking_consumer;

inline module::ModuleMap GlobalTestModuleMapHack;

inline module::Resources TestResources(
    module::ModuleMap& module_map = GlobalTestModuleMapHack) {
  return module::Resources(module::UniqueId("~test-module~"), module_map,
                           tracking_consumer);
}

struct Repl {
  explicit Repl(module::Resources resources = TestResources())
      : resources_(std::move(resources)),
        state_(table_, type_system(), arguments_) {
    consumer_ = &static_cast<diagnostic::TrackingConsumer&>(
        resources_.diagnostic_consumer());
  }

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

      os << "    \u256d";
      std::fill_n(std::ostream_iterator<std::string_view>(os), length + 2,
                  "\u2500");
      os << "\u256e\n";
      for (std::string_view line : lines) {
        if (line.size() > indentation) { line.remove_prefix(indentation); }
        NTH_ASSERT(length >= line.size());
        os << "    \u2502 " << line << std::string(length - line.size(), ' ')
           << " \u2502\n";
      }
      os << "    \u2570";
      std::fill_n(std::ostream_iterator<std::string_view>(os), length + 2,
                  "\u2500");

      return os << "\u256f\n";
    }

    std::string_view content_;
  };

 public:
  struct TypeCheckResult : ResultBase {
    std::span<std::pair<std::string, std::string> const> diagnostics() const {
      return diagnostics_;
    }

    std::span<semantic_analysis::QualifiedType const> qualified_types() const {
      return qts_;
    }

    friend std::ostream& operator<<(std::ostream& os,
                                    TypeCheckResult const& r) {
      os << static_cast<ResultBase const&>(r);
      std::string_view separator =
          "    where the qualified type of the last expression is ";
      for (auto const& qt : r.qualified_types()) {
        os << std::exchange(separator, ", ")
           << semantic_analysis::DebugQualifiedType(qt, r.repl_.type_system());
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
        std::span<semantic_analysis::QualifiedType const> qts,
        std::span<std::pair<std::string, std::string> const> diagnostics,
        Repl& repl)
        : ResultBase(content),
          qts_(qts.begin(), qts.end()),
          diagnostics_(diagnostics.begin(), diagnostics.end()),
          repl_(repl) {}

    std::vector<semantic_analysis::QualifiedType> qts_;
    std::vector<std::pair<std::string, std::string>> diagnostics_;
    Repl& repl_;
  };

  template <typename T>
  T execute(std::string source) {
    constexpr auto t = [] {
      if constexpr (nth::type<T> == nth::type<absl::int128>) {
        return nth::type<absl::int128*>;
      } else {
        return nth::type<T>;
      }
    }();
    using type = nth::type_t<t>;

    if (std::optional f = ExecutionFunction(std::move(source))) {
      if constexpr (FitsInRegister<type>()) {
        type result;
        vm::Execute(*f, state(), {}, result);
        if constexpr (t == nth::type<T>) {
          return result;
        } else {
          return *result;
        }
      } else {
        static_assert(t.dependent(false));
      }
    } else {
      for (auto const& [category, name] : consumer_->diagnostics()) {
        std::cerr << "* [" << category << ": " << name << "]\n";
      }
      std::cerr << "Failed to find an implementation function.\n";
      std::abort();
    }
  }

  TypeCheckResult type_check(std::string source);

  serialization::ForeignSymbolMap& foreign_symbol_map() {
    return module().foreign_symbol_map();
  }
  semantic_analysis::TypeSystem& type_system() {
    return module().type_system();
  }

  vm::ExecutionState& state() { return state_; }
  semantic_analysis::Context const& context() const { return context_; }
  ast::Module const& ast_module() const { return ast_module_; }
  module::Module& module() { return resources().primary_module(); }
  module::Resources& resources() { return resources_; }

  ast::Expression const& last_expression() const {
    base::PtrSpan stmts = ast_module().stmts();
    NTH_ASSERT(stmts.size() != 0);
    return stmts.back()->as<ast::Expression>();
  }

  vm::Function const* function(module::LocalFnId f) {
    return &module().function_table().function(f);
  }

 private:
  template <typename T>
  static constexpr bool FitsInRegister() {
    return true;  // TODO
  }
  void PrintQualifiedType(std::ostream& os,
                          semantic_analysis::QualifiedType qt);
  void PrintType(std::ostream& os, core::Type t);

  std::optional<vm::Function> ExecutionFunction(std::string&& source);

  module::Resources resources_;
  std::deque<std::string> source_content_;
  data_types::IntegerTable table_;
  ast::Module ast_module_;
  semantic_analysis::Context context_;
  diagnostic::TrackingConsumer* consumer_;
  vm::ArgumentSlice arguments_{nullptr, 0};
  vm::ExecutionState state_;
};

struct Snippet {
  explicit Snippet(std::string content,
                   module::Resources resources = TestResources());

  semantic_analysis::Context const& context() const { return context_; }
  ast::Module const& ast_module() const { return ast_module_; }
  module::Module& module() const { return resources().primary_module(); }
  module::Resources& resources() const { return resources_; }

  semantic_analysis::TypeSystem& type_system() const {
    return module().type_system();
  }

  friend std::ostream& operator<<(std::ostream& os, Snippet const& snippet) {
    os << "A code snippet\n";
    std::vector<std::string_view> lines =
        absl::StrSplit(snippet.content_, absl::ByChar('\n'));
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

    os << "    \u256d";
    std::fill_n(std::ostream_iterator<std::string_view>(os), length + 2,
                "\u2500");
    os << "\u256e\n";
    for (std::string_view line : lines) {
      if (line.size() > indentation) { line.remove_prefix(indentation); }
      NTH_ASSERT(length >= line.size());
      os << "    \u2502 " << line << std::string(length - line.size(), ' ')
         << " \u2502\n";
    }
    os << "    \u2570";
    std::fill_n(std::ostream_iterator<std::string_view>(os), length + 2,
                "\u2500");

    return os << "\u256f\n";
  }

 private:
  friend struct HasDiagnostics;
  friend struct HasQualTypes;
  template <typename>
  friend struct EvaluatesTo;

  vm::Function ExecutionFunction() const;

  // TODO: Doesn't need to be mutable?
  mutable module::Resources resources_;
  std::string content_;
  mutable data_types::IntegerTable table_;
  ast::Module ast_module_;
  mutable semantic_analysis::Context context_;
  diagnostic::TrackingConsumer& consumer_;
  mutable vm::ArgumentSlice arguments_{nullptr, 0};

  std::vector<semantic_analysis::QualifiedType> qualified_types_;
};

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

  bool MatchAndExplain(Snippet const& value,
                       testing::MatchResultListener* listener) const {
    return testing::ExplainMatchResult(ms_, value.consumer_.diagnostics(),
                                       listener);
  }

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

  bool MatchAndExplain(Snippet const& snippet,
                       testing::MatchResultListener* listener) const {
    return testing::ExplainMatchResult(ms_, snippet.qualified_types_, listener);
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

template <typename T>
struct EvaluatesTo {
 private:
  template <typename>
  static constexpr bool FitsInRegister() {
    return true;  // TODO
  }

 public:
  using is_gtest_matcher = void;

  explicit EvaluatesTo(T t) : value_(std::move(t)) {}

  bool MatchAndExplain(Snippet const& snippet,
                       testing::MatchResultListener* listener) const {
    constexpr auto t = [] {
      if constexpr (nth::type<T> == nth::type<absl::int128>) {
        return nth::type<absl::int128*>;
      } else {
        return nth::type<T>;
      }
    }();
    using type = nth::type_t<t>;

    auto f = snippet.ExecutionFunction();
    if constexpr (FitsInRegister<type>()) {
      type result;

      vm::ExecutionState state_(snippet.table_, snippet.type_system(),
                                snippet.arguments_);
      vm::Execute(f, state_, {}, result);
      if constexpr (t == nth::type<T>) {
        return testing::ExplainMatchResult(testing::Eq(value_), result,
                                           listener);
      } else {
        return testing::ExplainMatchResult(value_, *result, listener);
      }
    } else {
      static_assert(t.dependent(false));
    }
  }

  void DescribeTo(std::ostream* os) const {
    std::string s;
    nth::string_printer p(s);
    nth::universal_formatter f({.depth = 3, .fallback = "..."});
    nth::Interpolate<"{}">(p, f, value_);
    *os << "evaluates to " << s;
  }

  void DescribeNegationTo(std::ostream* os) const {
    std::string s;
    nth::string_printer p(s);
    nth::universal_formatter f({.depth = 3, .fallback = "..."});
    nth::Interpolate<"{}">(p, f, value_);
    *os << "does not evaluate to " << s;
  }

 private:
  T value_;
};

template <typename T>
EvaluatesTo(T const&) -> EvaluatesTo<T>;

}  // namespace test
