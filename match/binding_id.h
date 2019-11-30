#ifndef ICARUS_MATCH_BINDING_ID_H
#define ICARUS_MATCH_BINDING_ID_H

#include <string_view>

namespace match {

struct BindingId {
 public:
  explicit BindingId(std::string_view sv);

  BindingId()                                = delete;
  constexpr BindingId(BindingId const &)     = default;
  constexpr BindingId(BindingId &&) noexcept = default;
  constexpr BindingId &operator=(BindingId const &) = default;
  constexpr BindingId &operator=(BindingId &&) noexcept = default;

  friend bool operator==(BindingId lhs, BindingId rhs) {
    return lhs.name_.data() == rhs.name_.data();
  }
  friend bool operator!=(BindingId lhs, BindingId rhs) {
    return not(lhs == rhs);
  }

  constexpr std::string_view view() const { return name_; }

 private:
  std::string_view name_;
};

}  // namespace match

#endif
