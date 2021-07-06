#include "base/serialize.h"

#include <type_traits>

#include "gtest/gtest.h"

namespace base {
namespace {

int depth = 0;

struct Serializer {
  explicit Serializer(std::string *output) : output_(output) {}

  void write(absl::Span<std::byte const> bytes) {
    output_->append(reinterpret_cast<char const *>(bytes.data()), bytes.size());
  }

 private:
  std::string *output_;
};

struct Deserializer {
  explicit Deserializer(std::string *input) : iter_(input->cbegin()) {}

  absl::Span<std::byte const> read(size_t num_bytes) {
    auto start = std::exchange(iter_, iter_ + num_bytes);
    return absl::Span<std::byte const>(
        reinterpret_cast<std::byte const *>(&*start), num_bytes);
  }

 private:
  std::string::const_iterator iter_;
};

template <typename T>
T RoundTrip(T const &value) {
  std::string buffer;
  Serializer s(&buffer);
  Serialize(s, value);

  Deserializer d(&buffer);
  T result;
  Deserialize(d, result);
  return result;
}

struct TriviallyCopyable {
  int a;
  bool b;
  double c;

  bool operator==(TriviallyCopyable const &) const = default;
};

struct Twice {
  explicit Twice(int n = 0) : n_(2 * n) {}

  friend void BaseSerialize(auto &serializer, Twice value) {
    Serialize(serializer, value.n_ / 2);
  }

  friend void BaseDeserialize(auto &deserializer, Twice &value) {
    int n;
    Deserialize(deserializer, n);
    value = Twice(n);
  }

  bool operator==(Twice const &) const = default;

 private:
  int n_;
};

TEST(Serialize, RoundTrip) {
  EXPECT_EQ(RoundTrip(3), 3);
  EXPECT_EQ(RoundTrip(3.14), 3.14);
  EXPECT_EQ(RoundTrip(TriviallyCopyable{.a = -4, .b = true, .c = 2.71828}),
            (TriviallyCopyable{.a = -4, .b = true, .c = 2.71828}));
  EXPECT_EQ(RoundTrip(Twice(3)), Twice(3));

  EXPECT_EQ(RoundTrip(std::vector<Twice>{}), std::vector<Twice>{});
  EXPECT_EQ(RoundTrip(std::vector{Twice(2), Twice(3), Twice(4)}),
            (std::vector{Twice(2), Twice(3), Twice(4)}));
  EXPECT_EQ(RoundTrip(std::vector{std::vector{Twice(2), Twice(3), Twice(4)}}),
            (std::vector{std::vector{Twice(2), Twice(3), Twice(4)}}));

  EXPECT_EQ(RoundTrip(std::pair(1, true)), std::pair(1, true));
  EXPECT_EQ(RoundTrip(std::tuple(1, true, 1.3)), std::tuple(1, true, 1.3));
}

}  // namespace
}  // namespace base
