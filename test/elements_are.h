#ifndef ICARUS_TEST_ELEMENTS_ARE_H
#define ICARUS_TEST_ELEMENTS_ARE_H

#include "base/stringify.h"
#include "test/catch.h"

namespace test {

template <typename Container>
struct ElementsAreMatcher : public Catch::MatcherBase<Container> {
  ElementsAreMatcher(Container&& container)
      : container_(std::move(container)) {}

  // Performs the test for this matcher
  virtual bool match(Container const& c) const override {
    if (container_.size() != c.size()) { return false; }
    for (auto const& [k, v] : container_) {
      auto iter = c.find(k);
      if (iter == c.end()) { return false; }
      if (iter->second != v) { return false; }
    }
    return true;
  }

  virtual std::string describe() const override {
    using base::stringify;
    return "is a container with the elements in some order " +
           stringify(container_);
  }

 private:
  Container container_;
};

// The builder function
template <typename K, typename V, typename... Args>
inline auto ElementsAre(std::pair<K, V> const& arg, Args&&... args) {
  return ElementsAreMatcher(
      absl::flat_hash_map<K, V>{std::move(arg), std::forward<Args>(args)...});
}

}  // namespace test

#endif  // ICARUS_TEST_ELEMENTS_ARE_H
