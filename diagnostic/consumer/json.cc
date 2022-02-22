#include "diagnostic/consumer/json.h"

namespace diagnostic {

void JsonConsumer::ConsumeImpl(std::string_view category, std::string_view name,
                               DiagnosticMessage &&diag) {
  nlohmann::json error{
      {"category", std::string(category)},
      {"name", std::string(name)},
  };
  auto &message = error["message"];
  diag.for_each_component([&](auto const &component) {
    constexpr auto type = base::meta<std::decay_t<decltype(component)>>;
    if constexpr (type == base::meta<Text>) {
      message.emplace_back(component);
    } else if constexpr (type == base::meta<List>) {
      message.emplace_back(component.items());
    } else if constexpr (type == base::meta<SourceQuote>) {
      auto highlights = component.highlights;
      std::sort(highlights.begin(), highlights.end(),
                [](auto const &l, auto const &r) {
                  return std::less<char const *>{}(l.range.data(),
                                                   r.range.data());
                });

      auto &lines = message.emplace_back();
      for (auto const &highlight : highlights) {
        auto &entry       = source_indexer_.EntryFor(highlight.range);
        auto [start, end] = entry.lines_containing(highlight.range);

        for (size_t line_number = start; line_number < end; ++line_number) {
          lines[absl::StrCat(line_number)] = entry.line(line_number);
        }
      }
    } else {
      static_assert(base::always_false(type));
    }
  });
  json_.push_back(std::move(error));
}

}  // namespace diagnostic
