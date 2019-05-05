#ifndef ICARUS_TEST_ORDERED_ELEMENTS_ARE_H
#define ICARUS_TEST_ORDERED_ELEMENTS_ARE_H

#include "base/stringify.h"
#include "test/catch.h"

namespace test {

template <typename Container>
struct OrderedElementsAreMatcher : public Catch::MatcherBase<Container> {
  OrderedElementsAreMatcher(Container&& container)
      : container_(std::move(container)) {}

  virtual bool match(Container const& c) const override {
    if (container_.size() != c.size()) { return false; }
    using std::begin;
    using std::end;
    auto iter1 = begin(container_);
    auto iter2 = begin(c);
    while (true) {
      if (iter1 != end(container_)) {
        if (*iter1 != *iter2) { return false; }
        ++iter1;
        ++iter2;
      } else {
        return true;
      }
    }
  }

  virtual std::string describe() const override {
    using base::stringify;
    return "is a container with the elements in order " + stringify(container_);
  }

 private:
  Container container_;
};

// The builder function
template <typename T, typename... Args>
inline auto OrderedElementsAre(T const& arg, Args&&... args) {
  return OrderedElementsAreMatcher(
      std::vector<T>{arg, std::forward<Args>(args)...});
}

}  // namespace test

#endif  // ICARUS_TEST_ORDERED_ELEMENTS_ARE_H
