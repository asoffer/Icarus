#include "ir/results.h"

namespace ir {

Results Results::FromRaw(void const* data, core::Bytes bytes) {
  Results results;
  // I don't care about alignment here because the buffer is maximally aligned
  // anyway.
  results.buf_.append_bytes(bytes.value(), 16);
  std::memcpy(results.buf_.raw(0), data, bytes.value());
  results.offset_.push_back(0);
  return results;
}

Results Results::FromUntypedBuffer(std::vector<int64_t> offsets,
                                   base::untyped_buffer buf) {
  Results results;
  for (int64_t& offset : offsets) { offset = -offset; }
  results.offset_ = std::move(offsets);
  results.buf_    = std::move(buf);
  return results;
}

Results Results::GetResult(size_t index) const {
  Results r;
  if (is_reg(index)) {
    r.offset_.push_back(offset_.at(index));
    return r;
  } else {
    r.offset_.push_back(offset_.at(0));
    size_t i = index + 1;
    for (; i < offset_.size(); ++i) {
      if (offset_.at(i) < 0) { break; }
    }
    size_t raw_start = -offset_.at(index);
    size_t raw_end   = (i < offset_.size()) ? -offset_.at(i) : buf_.size();
    size_t len       = raw_end - raw_start;
    r.buf_           = base::untyped_buffer::MakeFull(len);
    std::memcpy(r.buf_.raw(0), buf_.raw(raw_start), len);
    return r;
  }
}

std::string Results::to_string() const { 
 using base::stringify;
 return stringify(offset_) + buf_.to_string();
}

}  // namespace ir
