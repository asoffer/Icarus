#include "init/argon.h"

#include <iostream>

namespace argon {
int (*ArgonExecute)();
std::function<void(char const *)> ArgonHandleOther;

namespace internal {
std::unordered_map<std::string, ::argon::internal::Flag *> all_args;
}  // namespace internal

int Parse(int argc, char *argv[]) {
  argon::Usage();

  for (int arg_num = 1; arg_num < argc; ++arg_num) {
    auto arg = argv[arg_num];
    if (arg[0] == '-') {
      if (arg[1] == '-') {
        char *ptr = arg + 2;
        while (*ptr != '=' && *ptr != '\0') { ++ptr; }
        bool was_eq = (*ptr == '=');
        *ptr = '\0';
        ptr++;  // points to the argument

        auto iter = ::argon::internal::all_args.find(arg + 2);
        if (iter == ::argon::internal::all_args.end()) {
          std::cerr << "Invalid argument \"" << argv[arg_num]
                    << "\" encountered (failed to find matching name).";
        } else {
          if (!iter->second->parse_and_apply_(ptr)) {
            // TODO
          }
        }
        if (was_eq) { *ptr = '='; }
      } else {
        std::cerr << "Invalid argument \"" << argv[arg_num]
                  << "\" encountered (starts with exactly one dash).\n";
      }
    } else {
      ArgonHandleOther(arg);
    }
  }

  return ArgonExecute();
}
}  // namespace argon
