#ifndef ICARUS_IR_INSTRUCTION_PHI_H
#define ICARUS_IR_INSTRUCTION_PHI_H

#include <string>
#include <utility>
#include <vector>

#include "absl/strings/str_cat.h"
#include "base/extend.h"
#include "ir/blocks/basic.h"
#include "ir/byte_code_writer.h"
#include "ir/instruction/inliner.h"
#include "ir/instruction/op_codes.h"
#include "ir/value/reg_or.h"

namespace ir {

// TODO consider changing these to something like 'basic block arguments'
template <typename T>
struct PhiInstruction {
  using type = T;

  PhiInstruction() = default;
  PhiInstruction(std::vector<BasicBlock const*> blocks,
                 std::vector<RegOr<T>> values)
      : blocks(std::move(blocks)), values(std::move(values)) {}
  ~PhiInstruction() {}

  void add(BasicBlock const* block, RegOr<T> value) {
    blocks.push_back(block);
    values.push_back(value);
  }

  std::string to_string() const {
    using base::stringify;
    std::string s =
        absl::StrCat(stringify(result), " = phi ", internal::TypeToString<T>());
    for (size_t i = 0; i < blocks.size(); ++i) {
      absl::StrAppend(&s, "\n      ", stringify(blocks[i]), ": ",
                      stringify(values[i]));
    }
    return s;
  }

  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write<uint16_t>(values.size());
    for (auto block : blocks) { writer->Write(block); }
    internal::WriteBits<uint16_t, RegOr<T>>(
        writer, values, [](RegOr<T> const& r) { return r.is_reg(); });

    absl::c_for_each(values, [&](RegOr<T> const& x) {
      x.apply([&](auto v) { writer->Write(v); });
    });

    writer->Write(result);
  }

  void Inline(InstructionInliner const& inliner) {
    inliner.Inline(values);
    inliner.Inline(result);
  }

  std::vector<BasicBlock const*> blocks;
  std::vector<RegOr<T>> values;
  Reg result;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_PHI_H
