#ifndef ICARUS_IR_CMD_BUFFER_H
#define ICARUS_IR_CMD_BUFFER_H

#include <cstddef>

#include "base/untyped_buffer.h"
#include "core/arch.h"
#include "ir/inliner.h"

namespace backend {
struct ExecContext;
}  // namespace backend

namespace ir {
struct BasicBlock;

struct CmdBuffer {
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

  size_t size() const { return buf_.size(); }

  template <typename T>
  void set(size_t offset, T const& t) {
    buf_.set(offset, t);
  }

  BasicBlock const* Execute(std::vector<ir::Addr> const& ret_slots,
                            backend::ExecContext* ctx) const;

  void UpdateForInlining(Inliner const& inliner);

  std::string to_string() const;

  ICARUS_PRIVATE
  base::untyped_buffer buf_;
};

size_t GetOffset(CompiledFn const* fn, Reg r);
}  // namespace ir

#endif  // ICARUS_IR_CMD_BUFFER_H
