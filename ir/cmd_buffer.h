#ifndef ICARUS_IR_CMD_BUFFER_H
#define ICARUS_IR_CMD_BUFFER_H

#include <cstddef>

#include "base/untyped_buffer.h"
#include "core/arch.h"
#include "ir/reg.h"

namespace backend {
struct ExecContext;
}  // namespace backend

namespace ir {
struct BasicBlock;
struct Inliner;

struct CmdBuffer {
  friend struct Inliner;  // TODO remove
 public:
  template <typename CmdType>
  void append_index() {
    buf_.append(CmdType::index);
  }

  template <typename T>
  void append(T const& t) {
    buf_.append(t);
  }

  // Reserves space for a T and returns the offset at which the space is
  // reserved.
  template <typename T>
  size_t reserve() {
    return buf_.append_bytes(sizeof(T), alignof(T));
  }

  // TODO probably shouldn't exist.
  constexpr auto begin() { return buf_.begin(); }
  constexpr auto end() { return buf_.end(); }

  size_t size() const { return buf_.size(); }

  template <typename T>
  void set(size_t offset, T const& t) {
    buf_.set(offset, t);
  }

  BasicBlock const* Execute(std::vector<ir::Addr> const& ret_slots,
                            backend::ExecContext* ctx) const;

  std::string to_string() const;

  ICARUS_PRIVATE
  base::untyped_buffer buf_;
};

size_t GetOffset(CompiledFn const* fn, Reg r);
}  // namespace ir

#endif  // ICARUS_IR_CMD_BUFFER_H
