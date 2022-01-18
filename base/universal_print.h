#ifndef ICARUS_BASE_UNIVERSAL_PRINT_H
#define ICARUS_BASE_UNIVERSAL_PRINT_H

#include <concepts>
#include <cstring>
#include <functional>
#include <iomanip>
#include <optional>
#include <sstream>
#include <string_view>
#include <variant>

#include "absl/status/statusor.h"
#include "base/meta.h"

namespace base {

template <typename P>
concept Printer = (std::invocable<P, int64_t> and
                   std::invocable<P, uint64_t> and std::invocable<P, double> and
                   std::invocable<P, void const*> and
                   std::invocable<P, std::string_view>);

// `UniversalPrint` accepts a `Printer` and a value of any type, and prints
// something with successive calls to the printer's call operator. This function
// will always compile correctly and aims to produce a representation of the
// value meaningful to humans. However, in the event that a value is provided
// for which there is no reasonable way we could determine how to print it, a
// representation will be printed that is not meaningful.
void UniversalPrint(Printer auto& printer, auto const& t) {
  using type = std::decay_t<decltype(t)>;
  if constexpr (std::invocable<std::decay_t<decltype(printer)>, type>) {
    printer(t);

  } else if constexpr (base::SatisfiesTupleProtocol<type>) {
    std::string_view separator = "";
    printer("(");
    std::apply(
        [&](auto const&... xs) {
          ((printer(std::exchange(separator, ", ")),
            ::base::UniversalPrint(printer, xs)),
           ...);
        },
        t);
    printer(")");

  } else if constexpr (meta<type>.template is_a<std::variant>()) {
    std::visit([&](auto const& x) { ::base::UniversalPrint(printer, x); }, t);

  } else if constexpr (meta<type> == meta<std::nullopt_t>) {
    printer("nullopt");

  } else if constexpr (meta<type>.template is_a<std::optional>()) {
    if (t) {
      ::base::UniversalPrint(printer, *t);
    } else {
      ::base::UniversalPrint(printer, std::nullopt);
    }

  } else if constexpr (meta<type>.template is_a<absl::StatusOr>()) {
    if (t.ok()) {
      ::base::UniversalPrint(*t);
    } else {
      ::base::UniversalPrint(t.status());
    }

  } else if constexpr (base::Container<type>) {
    std::string_view separator = "";
    printer("[");
    for (auto const& element : t) {
      printer(std::exchange(separator, ", "));
      ::base::UniversalPrint(printer, element);
    }
    printer("]");
  } else {
    static constexpr char kHexLookup[] = "0123456789abcdef";

    char buffer[sizeof(t)];
    std::memcpy(buffer, reinterpret_cast<void const*>(std::addressof(t)),
                sizeof(buffer));
    std::string_view separator = "";
    printer("(Unprintable value of type ");
    printer(typeid(type).name());
    printer(")[");
    for (size_t i = 0; i < sizeof(t); ++i) {
      printer(std::exchange(separator, " "));
      uint8_t value = static_cast<uint8_t>(buffer[i]);
      char byte[3]  = {kHexLookup[value >> 4], kHexLookup[value & 0x0f], '\0'};
      printer(byte);
    }
    printer("]");
  }
}

namespace internal_universal_print {

struct StringStreamPrinter {
  StringStreamPrinter() { ss_ << std::boolalpha; }

  void operator()(is_enum auto n) {
    using type = std::decay_t<decltype(n)>;
    ss_ << "(" << typeid(type).name() << ")"
        << static_cast<std::underlying_type_t<type>>(n);
  }
  void operator()(::base::Streamable auto&& x) { ss_ << x; }

  std::string str() && { return std::move(ss_).str(); }

 private:
  std::stringstream ss_;
};

}  // namespace internal_universal_print

std::string UniversalPrintToString(auto const& t) {
  ::base::internal_universal_print::StringStreamPrinter printer;
  ::base::UniversalPrint(printer, t);
  return std::move(printer).str();
}

}  // namespace base

#endif  //  ICARUS_BASE_UNIVERSAL_PRINT_H
