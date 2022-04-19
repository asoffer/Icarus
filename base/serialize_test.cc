#include "base/serialize.h"

#include <memory>
#include <type_traits>

#include "absl/container/flat_hash_map.h"
#include "gtest/gtest.h"

namespace base {
namespace {

struct TreatedSpecially {
  explicit TreatedSpecially(char n = 0) : n_(n) {}

  TreatedSpecially(TreatedSpecially const &t) { n_ = t.n_; }
  TreatedSpecially &operator=(TreatedSpecially const &t) {
    n_ = t.n_;
    return *this;
  }

  char value() const { return n_; }

  bool operator==(TreatedSpecially const &) const = default;

 private:
  char n_;
};
static_assert(not std::is_trivially_copyable_v<TreatedSpecially>);

struct Serializer {
  explicit Serializer(std::string *output) : output_(output) {}

  void write_bytes(absl::Span<std::byte const> bytes) {
    output_->append(reinterpret_cast<char const *>(bytes.data()), bytes.size());
  }

  template <typename T>
  void write(T const &t) requires(std::is_trivially_copyable_v<T>) {
    auto const *p = reinterpret_cast<std::byte const *>(&t);
    write_bytes(absl::MakeConstSpan(p, p + sizeof(T)));
  }

  void write(TreatedSpecially t) { output_->push_back(t.value()); }

 private:
  std::string *output_;
};

struct Deserializer {
  explicit Deserializer(std::string *input)
      : iter_(input->cbegin()), end_(input->cend()) {}

  absl::Span<std::byte const> read_bytes(size_t num_bytes) {
    auto start = std::exchange(iter_, iter_ + num_bytes);
    return absl::Span<std::byte const>(
        reinterpret_cast<std::byte const *>(&*start), num_bytes);
  }

  template <typename T>
  bool read(T &t) requires(std::is_trivially_copyable_v<T>) {
    if (std::distance(iter_, end_) < sizeof(T)) { return false; }
    std::memcpy(&t, &*iter_, sizeof(T));
    iter_ += sizeof(T);
    return true;
  }

  bool read(TreatedSpecially &t) {
    t = TreatedSpecially(*reinterpret_cast<char const *>(read_bytes(1).data()));
    return true;
  }

 private:
  std::string::const_iterator iter_;
  std::string::const_iterator end_;
};

template <typename T>
T RoundTrip(T const &value) {
  std::string buffer;
  Serializer s(&buffer);
  Serialize(s, value);

  Deserializer d(&buffer);
  return Deserialize<T>(d);
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

  friend bool BaseDeserialize(auto &deserializer, Twice &value) {
    int n;
    bool b = Deserialize(deserializer, n);
    value  = Twice(n);
    return b;
  }

  bool operator==(Twice const &) const = default;

 private:
  int n_;
};

TEST(AssignableType, Works) {
  EXPECT_EQ(base::meta<internal_serialize::AssignableType<int>>,
            base::meta<int>);
  EXPECT_EQ(base::meta<internal_serialize::AssignableType<int const>>,
            base::meta<int>);
  EXPECT_EQ(base::meta<internal_serialize::AssignableType<TriviallyCopyable>>,
            base::meta<TriviallyCopyable>);
  EXPECT_EQ(base::meta<internal_serialize::AssignableType<TriviallyCopyable>>,
            base::meta<TriviallyCopyable>);

  struct S {
    S(S const &);
    S &operator=(S const &);
  };

  EXPECT_EQ((base::meta<
                internal_serialize::AssignableType<std::pair<S const, bool>>>),
            (base::meta<std::tuple<S, bool>>));

  EXPECT_EQ(
      (base::meta<
          internal_serialize::AssignableType<std::tuple<int, S const, bool>>>),
      (base::meta<std::tuple<int, S, bool>>));
}

TEST(RoundTrip, Primitives) {
  EXPECT_EQ(RoundTrip(3), 3);
  EXPECT_EQ(RoundTrip(3.14), 3.14);
}

TEST(RoundTrip, TriviallyCopyable) {
  EXPECT_EQ(RoundTrip(TriviallyCopyable{.a = -4, .b = true, .c = 2.71828}),
            (TriviallyCopyable{.a = -4, .b = true, .c = 2.71828}));

  std::string_view sv("abc");
  std::string_view sv_round_tripped = RoundTrip(sv);
  EXPECT_EQ(sv.data(), sv_round_tripped.data());
  EXPECT_EQ(sv.size(), sv_round_tripped.size());
}

TEST(RoundTrip, AdlHook) {
  EXPECT_EQ(RoundTrip(Twice(3)), Twice(3));
  EXPECT_EQ(RoundTrip(Twice(4)), Twice(4));
}

TEST(RoundTrip, Containers) {
  EXPECT_EQ(RoundTrip(std::vector<Twice>{}), std::vector<Twice>{});
  EXPECT_EQ(RoundTrip(std::vector{Twice(2), Twice(3), Twice(4)}),
            (std::vector{Twice(2), Twice(3), Twice(4)}));
  EXPECT_EQ(RoundTrip(std::vector{std::vector{Twice(2), Twice(3), Twice(4)}}),
            (std::vector{std::vector{Twice(2), Twice(3), Twice(4)}}));
  EXPECT_EQ(RoundTrip(std::string("abc")), "abc");
  EXPECT_EQ(RoundTrip(std::string("")), "");
  EXPECT_EQ(RoundTrip(absl::flat_hash_map<std::string, int>(
                {{"Aa", 1}, {"Bb", 4}, {"Cc", 9}})),
            (absl::flat_hash_map<std::string, int>(
                {{"Aa", 1}, {"Bb", 4}, {"Cc", 9}})));
}

TEST(RoundTrip, TupleProtocol) {
  EXPECT_EQ(RoundTrip(std::pair(1, true)), std::pair(1, true));
  EXPECT_EQ(RoundTrip(std::tuple(1, true, 1.3)), std::tuple(1, true, 1.3));
}

TEST(RoundTrip, SpecialTreatment) {
  EXPECT_EQ(RoundTrip(TreatedSpecially('x')), TreatedSpecially('x'));
}

struct SerializerWithDelay {
  explicit SerializerWithDelay(std::string *output) : output_(output) {}

  void write_bytes(absl::Span<std::byte const> bytes) {
    output_->append(reinterpret_cast<char const *>(bytes.data()), bytes.size());
  }

  struct DelayedToken {
    size_t index;
    std::string *output;
    void set(int n) { std::memcpy(output->data() + index, &n, sizeof(int)); }
  };
  template <std::same_as<int> T>
  DelayedToken delayed() {
    DelayedToken token{.index = output_->size(), .output = output_};
    output_->resize(output_->size() + sizeof(T));
    return token;
  }

  void write(int n) {
    auto const *p = reinterpret_cast<std::byte const *>(&n);
    write_bytes(absl::MakeConstSpan(p, p + sizeof(int)));
  }

 private:
  std::string *output_;
};

struct DeserializerWithDelay {
  explicit DeserializerWithDelay(std::string *input)
      : iter_(input->cbegin()), end_(input->cend()) {}

  absl::Span<std::byte const> read_bytes(size_t num_bytes) {
    auto start = std::exchange(iter_, iter_ + num_bytes);
    return absl::Span<std::byte const>(
        reinterpret_cast<std::byte const *>(&*start), num_bytes);
  }

  bool read(int &n) {
    if (std::distance(iter_, end_) < sizeof(int)) { return false; }
    std::memcpy(&n, &*iter_, sizeof(int));
    iter_ += sizeof(int);
    return true;
  }

 private:
  std::string::const_iterator iter_;
  std::string::const_iterator end_;
};

struct UsesDelayed {
  UsesDelayed() = default;
  explicit UsesDelayed(int x, int y) : x_(x), y_(y) {}

  bool operator==(UsesDelayed const &) const = default;

  friend void BaseSerialize(base::Serializer auto &s, UsesDelayed const &u) {
    auto token = s.template delayed<int>();
    base::Serialize(s, u.y_);
    token.set(u.x_);
  }

  friend bool BaseDeserialize(base::Deserializer auto &d, UsesDelayed &u) {
    // When serializing, we fill in `x` after `y` but space for them is reserved
    // in the correct order, so it should still be deserializable in this order.
    bool result = base::Deserialize(d, u.x_, u.y_);
    return result;
  }

 private:
  int x_;
  int y_;
};

TEST(RoundTrip, SupportsDelayedInt) {
  static_assert(base::SupportsDelayed<SerializerWithDelay, int>);
  std::string output;
  SerializerWithDelay s(&output);
  UsesDelayed in(3, 10);
  base::Serialize(s, in);

  DeserializerWithDelay d(&output);
  UsesDelayed out;
  base::Deserialize(d, out);
  EXPECT_EQ(in, out);
}

}  // namespace
}  // namespace base
