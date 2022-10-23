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
#include "semantic_analysis/compiler_state.h"
#include "semantic_analysis/context.h"
#include "semantic_analysis/foreign_function_map.h"
#include "semantic_analysis/instruction_set.h"
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

  template <typename T>
  T execute(std::string source) {
    if (std::optional f = ExecutionFunction(std::move(source))) {
      if constexpr (FitsInRegister<T>()) {
        T result;
        jasmin::Execute(*f, {}, result);
        return result;
      } else {
        alignas(T) std::byte result[sizeof(T)];
        semantic_analysis::IrFunction wrapper(0, 0);
        wrapper.append<jasmin::Push>(&result);
        wrapper.append<jasmin::Push>(&*f);
        wrapper.append<jasmin::Call>();
        wrapper.append<jasmin::Return>();
        jasmin::Execute(wrapper, {});
        return *reinterpret_cast<T const*>(&result);
      }
    } else {
      for (auto const& [category, name] : consumer_.diagnostics()) {
        std::cerr << "* [" << category << ": " << name << "]\n";
      }
      std::cerr << "Failed to find an implementation function.\n";
      std::abort();
    }
  }

  TypeCheckResult type_check(std::string source);

  semantic_analysis::ForeignFunctionMap& foreign_function_map() {
    return state_.foreign_function_map();
  }
  semantic_analysis::TypeSystem& type_system() { return state_.type_system(); }
  semantic_analysis::Context const& context() const { return context_; }
  ast::Module const& module() const { return ast_module_; }

  ast::Expression const& last_expression() const {
    base::PtrSpan stmts = module().stmts();
    ASSERT(stmts.size() != 0);
    return stmts.back()->as<ast::Expression>();
  }

  semantic_analysis::IrFunction const* function(ir::Fn f) {
    return state_.function(f);
  }

 private:
  template <typename T>
  static constexpr bool FitsInRegister() {
    return not std::is_same_v<T, ir::Integer>;
  }
  void PrintQualifiedType(std::ostream& os,
                          semantic_analysis::QualifiedType qt);
  void PrintType(std::ostream& os, core::Type t);

  std::optional<semantic_analysis::IrFunction> ExecutionFunction(
      std::string&& source);

  std::deque<std::string> source_content_;
  module::Module module_;
  ast::Module ast_module_;
  semantic_analysis::Context context_;
  semantic_analysis::CompilerState state_{module_};
  diagnostic::TrackingConsumer consumer_;
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
