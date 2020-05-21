#include <vector>

#include "type/qual_type.h"
#include "type/type.h"

namespace compiler::internal {

struct QualTypeIterator {
  explicit QualTypeIterator(std::vector<type::QualType>::iterator iter)
      : iter_(iter), index_(0) {}

  void operator++() {
    if (index_ + 1 == iter_->expansion_size()) {
      index_ = 0;
      ++iter_;
    } else {
      ++index_;
    }
  }

  type::QualType operator*() const {
    auto const &qt = *iter_;
    if (qt.expansion_size() == 1) { return qt; }
    return type::QualType(qt.expanded()[index_], qt.quals());
  }

  friend bool operator==(QualTypeIterator lhs, QualTypeIterator rhs) {
    return lhs.index_ == rhs.index_ and lhs.iter_ == rhs.iter_;
  }

 // private:
  std::vector<type::QualType>::iterator iter_;
  size_t index_ = 0;
};

}  // namespace compiler::internal
