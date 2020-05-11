#include "ir/value/value.h"

namespace ir {

MultiValue::~MultiValue() {
  if (not data_) { return; }
  for (size_t i = 0; i < size(); ++i) { (*this)[i].~Value(); }
}

MultiValue::MultiValue(absl::Span<Value const> values) {
  data_ =
      std::unique_ptr<char[]>(new char[sizeof(Value) * (values.size() + 1)]);
  *reinterpret_cast<size_t*>(data_.get()) = values.size();

  auto* val_ptr = reinterpret_cast<Value*>(data_.get()) + 1;
  for (Value const v : values) { *val_ptr++ = v; }
}

MultiValue::MultiValue(MultiValue const& v) : MultiValue(v.span()) {}
MultiValue& MultiValue::operator=(MultiValue const& v) {
  return *this = MultiValue(v.span());
}

absl::Span<Value> MultiValue::span() {
  return absl::MakeSpan(reinterpret_cast<Value*>(data_.get()) + 1, size());
}

absl::Span<Value const> MultiValue::span() const {
  return absl::MakeConstSpan(reinterpret_cast<Value const*>(data_.get()) + 1,
                             size());
}

Value const& MultiValue::operator[](size_t n) const { return span()[n]; }
Value& MultiValue::operator[](size_t n) { return span()[n]; }

size_t MultiValue::size() const {
  return *reinterpret_cast<size_t*>(data_.get());
}

bool operator==(MultiValue const& lhs, MultiValue const& rhs) {
  if (lhs.size() != rhs.size()) { return false; }
  for (size_t i = 0; i < lhs.size(); ++i) {
    if (lhs[i] != rhs[i]) { return false; }
  }
  return true;
}

}  // namespace ir
