#include "init/cli.h"

#include <iostream>

#include "base/debug.h"

namespace cli {
std::function<int()> execute;
std::function<void(char const *)> HandleOther;

namespace internal {
std::vector<std::unique_ptr<::cli::internal::Handler>> owned_handlers;
absl::flat_hash_map<std::string, ::cli::internal::Handler *> all_handlers;
}  // namespace internal

int ParseAndRun(int argc, char *argv[]) {
  cli::Usage();

  bool errors = false;
  for (int arg_num = 1; arg_num < argc; ++arg_num) {
    auto arg = argv[arg_num];
    if (arg[0] == '-') {
      if (arg[1] == '-') {
        char *ptr = arg + 2;
        while (*ptr != '=' and *ptr != '\0') { ++ptr; }
        bool was_eq   = (*ptr == '=');
        char *old_ptr = ptr;
        *old_ptr      = '\0';
        ptr           = was_eq ? ptr + 1 : nullptr;

        if (auto iter = ::cli::internal::all_handlers.find(arg + 2);
            iter == ::cli::internal::all_handlers.end()) {
          std::cerr << "Invalid argument \"" << argv[arg_num]
                    << "\" encountered (failed to find matching name).\n";
          errors = true;
        } else {
          switch (ASSERT_NOT_NULL(iter->second->parse_and_apply_)(ptr)) {
            case ::cli::internal::Result::Ok: break;
            case ::cli::internal::Result::ParseError:
              if (was_eq) { *old_ptr = '='; }
              std::cerr << "Failed to parse argument \"" << arg << "\".\n";
              errors = true;
              break;
            case ::cli::internal::Result::AlreadyCalled:
              std::cerr << "Flag \"" << arg
                        << "\" was called more than once.\n";
              errors = true;
              break;
          }
        }
        if (was_eq) { *old_ptr = '='; }
      } else {
        std::cerr << "Invalid argument \"" << argv[arg_num]
                  << "\" encountered (starts with exactly one dash).\n";
      }
    } else {
      HandleOther(arg);
    }
  }

  for (auto &[name, handler] : ::cli::internal::all_handlers) {
    if (handler->call_once_ and not handler->called_) {
      handler->parse_and_apply_("");
    }
  }

  if (errors) { return -1; }
  if (execute == nullptr) {
    std::cerr << "No function chosen to execute.\n";
    return -1;
  }
  return execute();
}

int ShowUsage() {
  std::fputs("Usage:\n", stderr);
  absl::flat_hash_map<::cli::internal::Handler *, std::vector<std::string>>
      handlers;

  // TODO any sort of reasonable sorting?

  constexpr int terminal_width = 80;
  int max_name_length          = 0;
  for (const auto &[name, handler] : ::cli::internal::all_handlers) {
    handlers[handler].push_back(name);
    max_name_length = std::max<int>(max_name_length, name.size());
  }

  for (const auto &[handler, names] : handlers) {
    std::string const &msg = handler->msg_;
    size_t msg_index       = 0;

    for (const auto &name : names) {
      if (msg.size() == msg_index) {
        std::fprintf(stderr, "\n  --%s", name.c_str());
      } else {
        int space_for_description = terminal_width - 8 - name.size();
        size_t chunk_size         = 0;
        if (space_for_description < static_cast<int>(msg.size() - msg_index)) {
          chunk_size = space_for_description;
          while (chunk_size > 0 and msg.at(msg_index + chunk_size) != ' ') {
            --chunk_size;
          }
          if (chunk_size == 0) {
            // TODO
            chunk_size = space_for_description;
          }

        } else {
          chunk_size = msg.size() - msg_index;
        }
        std::string chunk(msg.data() + msg_index, chunk_size);
        std::fprintf(stderr, "\n  --%-*s    %s", max_name_length, name.c_str(),
                     chunk.c_str());
        msg_index += chunk.size();
        while (msg_index < msg.size() and msg.at(msg_index) == ' ') {
          ++msg_index;
        }
      }
    }
    std::fputs("\n", stderr);
  }

  return 0;
}

}  // namespace cli
