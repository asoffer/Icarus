"""
Python script for eliding frames from GDB backtraces that do not provide
valuable information.

This script elides any ast- or type-visitor frames, C++ standard library
internals and some frames surrounding std::function's call operator.

Enable this script by adding the following line to yout `.gdbinit` file:

```
source ~/.gdb/icarus.py
```

"""

import gdb
import re

def should_display(f):
    if type(f.function()) == int:
        # Integers represent "non-virtual thunks", which are typically offsets
        # for vtables of types with multiple inheritance.
        return False

    for pattern in [
            r"^ast::internal_node",
            r"^ast::Node::visit",
            r"^compiler::Compiler::VerifyType\(ast::Node const",
            r"^compiler::Compiler::EmitToBuffer\(ast::Node const",
            r"^type::Visitor<",
            r"^type::[^:]*::Accept",
            r"^std::__invoke",
            r"^std::_[A-Z]",
            r"^std::function",
            r"^std::invoke",
        ]:
        if re.match(pattern, f.function()) is not None:
            return False
    return True


class IcarusFilter:
    def __init__(self):
        self.name = 'IcarusFilter'
        self.enabled = True
        self.priority = 0

        gdb.current_progspace().frame_filters[self.name] = self

    def filter(self, frame_iter):
        return filter(should_display, frame_iter)

IcarusFilter()
