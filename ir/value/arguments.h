#ifndef ICARUS_IR_VALUE_ARGUMENTS_H
#define ICARUS_IR_VALUE_ARGUMENTS_H

#include <cstddef>

#include "base/traverse.h"
#include "core/arguments.h"
#include "ir/value/result_buffer.h"

namespace ir {

struct Arguments {
  PartialResultRef operator[](size_t n) const { return buffer_[indices_[n]]; }
  PartialResultRef operator[](std::string_view s) const {
    return buffer_[indices_[s]];
  }


  template <typename T>
  void pos_insert(T&& value) {
    indices_.pos_emplace(buffer_.num_entries());
    buffer_.append(std::forward<T>(value));
  }

  template <typename T>
  void named_insert(std::string_view name, T&& value) {
    indices_.named_emplace(name, buffer_.num_entries());
    buffer_.append(std::forward<T>(value));
  }

  PartialResultBuffer& buffer() & { return buffer_; }

  template <typename Tr>
  friend void BaseTraverse(Tr& t, Arguments& arguments) {
    base::Traverse(t, arguments.buffer_);
  }

  size_t size() const { return indices_.size(); }

  struct const_iterator {
    auto operator++() {
      ++iter_;
      return *this;
    }
    auto operator++(int) {
      iter_++;
      return *this;
    }

    PartialResultRef operator*() { return ref_[*iter_]; }

    friend bool operator==(const_iterator const& lhs,
                           const_iterator const& rhs) {
      return lhs.iter_ == rhs.iter_;
    }

    friend bool operator!=(const_iterator const& lhs,
                           const_iterator const& rhs) {
      return not(lhs == rhs);
    }

   private:
    friend Arguments;
    const_iterator(core::Arguments<size_t>::const_iterator iter,
                   PartialResultBuffer const* ptr)
        : iter_(iter), ref_(*ASSERT_NOT_NULL(ptr)) {}
    core::Arguments<size_t>::const_iterator iter_;
    PartialResultBuffer const& ref_;
  };

  const_iterator begin() const {
    return const_iterator(indices_.begin(), &buffer_);
  }
  const_iterator end() const {
    return const_iterator(indices_.end(), &buffer_);
  }

 private:
  core::Arguments<size_t> indices_;
  PartialResultBuffer buffer_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_ARGUMENTS_H
