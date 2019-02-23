#include "ir/results.h"

#include "ir/val.h"

namespace ir {

Results Results::FromVals(std::vector<Val> const& vals) {
  Results results;
  for (auto const& val : vals) {
    std::visit([&results](auto x) { results.append(x); }, val.value);
  }
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
#ifdef DBG
std::string Results::DebugString() const { 
 using base::stringify;
 return stringify(offset_) + buf_.DebugString();
}
#endif

}  // namespace ir
