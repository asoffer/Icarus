#ifndef ICARUS_IR_EXECUTE_H
#define ICARUS_IR_EXECUTE_H

#include "ir/module.h"
#include "ir/module.pb.h"
#include "nth/base/attributes.h"

namespace ic {

struct Deserializer {
  bool Deserialize(ModuleProto const& proto, Module& module);

  void set_builtin_module(Module& builtin NTH_ATTRIBUTE(lifetimebound)) {
    builtin_module_ = &builtin;
  }

 private:
  Module const* builtin_module_;
};

}  // namespace ic

#endif  // ICARUS_IR_EXECUTE_H
