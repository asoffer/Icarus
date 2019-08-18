#ifndef ICARUS_IR_CMD_MISC_H
#define ICARUS_IR_CMD_MISC_H

#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "backend/exec.h"
#include "ir/cmd/util.h"
#include "ir/inliner.h"
#include "type/type.h"

namespace ir {
struct SemanticCmd {
  constexpr static cmd_index_t index = 33;

  enum class Kind : uint8_t { Init, Destroy, Move, Copy };

  static std::optional<BlockIndex> Execute(base::untyped_buffer::iterator *iter,
                                           std::vector<Addr> const &ret_slots,
                                           backend::ExecContext *ctx);

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);

  static void UpdateForInlining(base::untyped_buffer::iterator *iter,
                                Inliner const &inliner);
};

struct LoadSymbolCmd {
  constexpr static cmd_index_t index = 34;

  static std::optional<BlockIndex> Execute(base::untyped_buffer::iterator *iter,
                                           std::vector<Addr> const &ret_slots,
                                           backend::ExecContext *ctx);

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);

  static void UpdateForInlining(base::untyped_buffer::iterator *iter,
                                Inliner const &inliner);
};

struct TypeInfoCmd {
  constexpr static cmd_index_t index = 35;

  static std::optional<BlockIndex> Execute(base::untyped_buffer::iterator *iter,
                                           std::vector<Addr> const &ret_slots,
                                           backend::ExecContext *ctx);

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);

  static void UpdateForInlining(base::untyped_buffer::iterator *iter,
                                Inliner const &inliner);
};

TypedRegister<core::Alignment> Align(RegOr<type::Type const *> r);
TypedRegister<core::Bytes> Bytes(RegOr<type::Type const *> r);

void Init(type::Type const *t, Reg r);
void Destroy(type::Type const *t, Reg r);
void Move(type::Type const *t, Reg from, RegOr<Addr> to);
void Copy(type::Type const *t, Reg from, RegOr<Addr> to);

type::Typed<Reg> LoadSymbol(std::string_view name, type::Type const *type);

}  // namespace ir

#endif  // ICARUS_IR_CMD_MISC_H
