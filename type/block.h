#ifndef ICARUS_TYPE_BLOCK_H
#define ICARUS_TYPE_BLOCK_H

#include "core/parameters.h"
#include "type/callable.h"
#include "type/qual_type.h"
#include "type/type.h"

namespace type {

struct Block : ReturningType {
  // TODO: Support actual returns.
  Block(core::Parameters<QualType> params)
      : ReturningType(IndexOf<Block>(),
                      LegacyType::Flags{.is_default_initializable = 0,
                                        .is_copyable              = 1,
                                        .is_movable               = 1,
                                        .has_destructor           = 0},
                      std::move(params), {}, false) {}

  bool is_big() const override { return false; }
  void ShowValue(std::ostream &, ir::CompleteResultRef const &) const override;

  void WriteTo(std::string *buf) const override;
  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  Completeness completeness() const override { return Completeness::Complete; }
};

Block const *Blk(core::Parameters<QualType> params);

}  // namespace type

#endif  // ICARUS_TYPE_BLOCK_H
