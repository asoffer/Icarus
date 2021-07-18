#include "ir/value/result_buffer.h"

namespace ir {

void PartialResultBuffer::append(PartialResultBuffer const &value) {
  size_t size = buffer_.size();
  offsets_.reserve(offsets_.size() + value.offsets_.size());
  for (internal_result_buffer::Offset o : value.offsets_) {
    o.index += size;
    offsets_.push_back(o);
  }
  buffer_.write(buffer_.size(), value.buffer_);
}

void PartialResultBuffer::append(PartialResultRef value) {
  offsets_.push_back(internal_result_buffer::Offset{
      .index       = static_cast<uint32_t>(buffer_.size()),
      .is_register = value.is_register()});
  buffer_.write(buffer_.size(), value.view_.data(), value.view_.size());
}

void PartialResultBuffer::append(CompleteResultBuffer const &value) {
  size_t size = buffer_.size();
  offsets_.reserve(offsets_.size() + value.offsets_.size());
  for (uint32_t o : value.offsets_) {
    o += size;
    offsets_.push_back(
        internal_result_buffer::Offset{.index = o, .is_register = 0});
  }
  buffer_.write(buffer_.size(), value.buffer_);
}

void CompleteResultBuffer::append(CompleteResultRef value) {
  offsets_.push_back(buffer_.size());
  buffer_.write(buffer_.size(), value.view_.data(), value.view_.size());
}

void PartialResultBuffer::append() {
  offsets_.push_back(
      offsets_.empty()
          ? internal_result_buffer::Offset{.index = 0, .is_register = 0}
          : offsets_.back());
}

PartialResultRef PartialResultBuffer::operator[](size_t i) const {
  ASSERT(i < offsets_.size());
  return PartialResultRef(
      base::untyped_buffer_view(
          buffer_.raw(offsets_[i].index),
          (i + 1 == offsets_.size() ? buffer_.size() : offsets_[i + 1].index) -
              offsets_[i].index),
      is_register(i));
}

PartialResultRef PartialResultBuffer::back() const {
  return PartialResultRef(
      base::untyped_buffer_view(buffer_.raw(offsets_.back().index),
                                buffer_.size() - offsets_.back().index),
      is_register(offsets_.size() - 1));
}

void CompleteResultBuffer::append(CompleteResultBuffer const &value) {
  size_t size = buffer_.size();
  offsets_.reserve(offsets_.size() + value.offsets_.size());
  for (size_t o : value.offsets_) { offsets_.push_back(o + size); }
  buffer_.write(buffer_.size(), value.buffer_);
}

void CompleteResultBuffer::append() {
  offsets_.push_back(offsets_.empty() ? 0 : offsets_.back());
}

CompleteResultRef CompleteResultBuffer::operator[](size_t i) const {
  ASSERT(i < offsets_.size());
  return CompleteResultRef(base::untyped_buffer_view(
      buffer_.raw(offsets_[i]),
      (i + 1 == offsets_.size() ? buffer_.size() : offsets_[i + 1]) -
          offsets_[i]));
}

CompleteResultRef CompleteResultBuffer::back() const {
  return CompleteResultRef(base::untyped_buffer_view(
      buffer_.raw(offsets_.back()), buffer_.size() - offsets_.back()));
}

PartialResultBuffer::PartialResultBuffer(CompleteResultBuffer buffer)
    : buffer_(std::move(buffer.buffer_)) {
  for (uint32_t offset : buffer.offsets_) {
    offsets_.push_back(
        internal_result_buffer::Offset{.index = offset, .is_register = 0});
  }
}

addr_t CompleteResultBuffer::append_slot(size_t slot_size) {
  offsets_.push_back(buffer_.size());
  size_t size = buffer_.size();
  buffer_.append_bytes(slot_size);

  return buffer_.raw(size);
}

void CompleteResultBuffer::reserve_bytes(size_t num_entries, size_t num_bytes) {
  offsets_.reserve(num_entries);
  buffer_.reserve(num_bytes);
}

CompleteResultRef PartialResultRef::AsComplete() const {
  ASSERT(is_register() == false);
  return CompleteResultRef(view_);
}

void PartialResultBuffer::set_register(size_t i, Reg r) {
  ASSERT((*this)[i].is_register() == true);
  buffer_.set(offsets_[i].index, r);
}

}  // namespace ir
