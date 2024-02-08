#include "common/string_literal.h"

#include "absl/synchronization/mutex.h"

namespace ic {
namespace {

absl::Mutex mutex;
int generation = 0;
nth::flyweight_map<std::string, int> strings = {{"", 0}};

uint32_t Insert(std::string &&s) {
  absl::MutexLock lock(&mutex);
  auto iter    = strings.try_emplace(std::move(s), generation).first;
  iter->second = generation;
  return strings.index(iter);
}

uint32_t Insert(std::string const &s) {
  absl::MutexLock lock(&mutex);
  auto iter    = strings.try_emplace(s, generation).first;
  iter->second = generation;
  return strings.index(iter);
}

}  // namespace

StringLiteral::StringLiteral()
    : StrongIdentifierType<StringLiteral, uint32_t>(0) {}
StringLiteral::StringLiteral(std::string &&s)
    : StrongIdentifierType<StringLiteral, uint32_t>(Insert(std::move(s))) {}
StringLiteral::StringLiteral(std::string const &s)
    : StrongIdentifierType<StringLiteral, uint32_t>(Insert(s)) {}
StringLiteral::StringLiteral(std::string_view s)
    : StrongIdentifierType<StringLiteral, uint32_t>(Insert(std::string(s))) {}

std::string_view StringLiteral::str() const {
  return strings.from_index(index()).first;
}

void StringLiteral::CompleteGeneration() {
  absl::MutexLock lock(&mutex);
  ++generation;
}

std::vector<std::string_view> StringLiteral::LatestGeneration() {
  std::vector<std::string_view> result;
  absl::MutexLock lock(&mutex);
  for (auto const &[str, gen] : strings) {
    if (gen == generation) { result.push_back(str); }
  }
  return result;
}

}  // namespace ic
