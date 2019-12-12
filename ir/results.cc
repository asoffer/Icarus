#include "ir/results.h"

#include "absl/strings/str_join.h"

namespace ir {

Results Results::FromRaw(void const* data, core::Bytes bytes) {
  Results results;
  results.buf_.append_bytes(bytes.value());
  std::memcpy(results.buf_.raw(0), data, bytes.value());
  results.offset_.push_back(0);
  results.is_reg_.push_back(false);
  return results;
}

Results Results::FromUntypedBuffer(std::vector<uint32_t> offsets,
                                   base::untyped_buffer buf) {
  Results results;
  results.is_reg_.resize(offsets.size());
  results.offset_ = std::move(offsets);
  results.buf_    = std::move(buf);
  return results;
}

Results Results::GetResult(size_t index) const {
  ASSERT(index <= size());
  Results r;
  r.is_reg_.push_back(is_reg(index));
  size_t raw_start = offset_[index];
  r.offset_.push_back(0);
  size_t raw_end = (index + 1 == size()) ? buf_.size() : offset_[index + 1];
  size_t len     = raw_end - raw_start;
  r.buf_         = base::untyped_buffer::MakeFull(len);
  std::memcpy(r.buf_.raw(0), buf_.raw(raw_start), len);
  return r;
}

std::string Results::to_string() const {
  size_t index = 0;
  return absl::StrCat(
      "[",
      absl::StrJoin(offset_, ", ",
                    [&](std::string* out, uint32_t offset) {
                      if (is_reg(index++)) {
                        out->append(stringify(buf_.get<Reg>(offset)));
                      } else {
                        return absl::StrAppend(out, "offset(", offset, ")");
                      }
                    }),
      "]");
}

}  // namespace ir
