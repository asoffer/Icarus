#ifndef ICARUS_INIT_CLI_H
#define ICARUS_INIT_CLI_H

#include <cstring>
#include <functional>
#include <iostream>
#include <memory>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "base/util.h"

namespace cli {
namespace internal {
struct Handler;
enum class Result { Ok, ParseError, AlreadyCalled };

extern std::vector<std::unique_ptr<::cli::internal::Handler>> owned_handlers;
extern absl::flat_hash_map<std::string, ::cli::internal::Handler *> all_handlers;

struct Handler {
  template <typename... Args>
  Handler(Args &&... args) : call_once_(true) {
    (all_handlers.emplace(std::forward<Args>(args), this), ...);
  }

  template <typename Fn>
  void operator<<(Fn &&fn) {
    if constexpr (std::is_invocable_v<Fn, bool>) {
      parse_and_apply_ = [ this, f = std::forward<Fn>(fn) ](char const *arg) {
        bool called_already = called_;
        called_             = true;
        if (call_once_ && called_already) {
          return ::cli::internal::Result::AlreadyCalled;
        }

        if (arg == nullptr || strcmp("true", arg) == 0) {
          f(true);
          return ::cli::internal::Result::Ok;
        } else if (strcmp("false", arg) == 0) {
          f(false);
          return ::cli::internal::Result::Ok;
        } else if (strcmp("", arg) == 0) {
          f();
          return ::cli::internal::Result::Ok;
        } else {
          return ::cli::internal::Result::ParseError;
        }
      };
    } else if constexpr (std::is_invocable_v<Fn>) {
      call_once_       = false;
      parse_and_apply_ = [ this, f = std::forward<Fn>(fn) ](char const *) {
        bool called_already = called_;
        called_             = true;
        if (call_once_ && called_already) {
          return ::cli::internal::Result::AlreadyCalled;
        }

        f();
        return ::cli::internal::Result::Ok;
      };
    } else if constexpr (std::is_invocable_v<Fn, char const *>) {
      call_once_       = false;
      parse_and_apply_ = [ this, f = std::forward<Fn>(fn) ](char const * cstr) {
        bool called_already = called_;
        called_             = true;
        if (call_once_ && called_already) {
          return ::cli::internal::Result::AlreadyCalled;
        }

        f(cstr);
        return ::cli::internal::Result::Ok;
      };

    } else {
      static_assert(base::always_false<Fn>());
    }
  }

  Handler &operator<<(const char *msg) {
    msg_ = msg;
    return *this;
  }

  Handler &operator<<(std::string msg) {
    msg_ = std::move(msg);
    return *this;
  }

  std::string msg_;
  std::function<::cli::internal::Result(char const *)> parse_and_apply_;
  bool call_once_ = false;
  bool called_    = false;
};

struct Flag : public Handler {
  template <typename... Args>
  Flag(Args &&... args) : Handler(std::forward<Args>(args)...) {}

  bool called_ = false;
};
}  // namespace internal

template <typename... Args>
::cli::internal::Handler &Flag(Args &&... args) {
  return *::cli::internal::owned_handlers.emplace_back(
      std::make_unique<::cli::internal::Handler>(std::forward<Args>(args)...));
}

extern std::function<int()> execute;
extern std::function<void(char const *)> HandleOther;
int ParseAndRun(int argc, char *argv[]);
void Usage();
int ShowUsage();
}  // namespace cli

#endif  // ICARUS_INIT_CLI_H
