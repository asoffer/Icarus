#include <cstring>
#include <functional>
#include <string>
#include <unordered_map>
#include <utility>

#define ARGON_FLAG(...)                                                        \
  []() -> ::argon::internal::Flag & {                                          \
    static ::argon::internal::Flag flag(__VA_ARGS__);                          \
    return flag;                                                               \
  }()

namespace argon {
namespace internal {
struct Flag;
extern std::unordered_map<std::string, ::argon::internal::Flag *> all_args;

struct Flag {
  template <typename... Args>
  Flag(Args &&... args) {
    (all_args.emplace(std::forward<Args>(args), this), ...);
  }

  Flag &operator<<(std::string msg) {
    msg_ = std::move(msg);
    return *this;
  }

  template <typename Fn,
            typename = decltype(std::declval<Fn>()(std::declval<bool>()))>
  Flag &operator<<(Fn &&fn) {
    parse_and_apply_ = [f = std::forward<Fn>(fn)](const char *arg) {
      if (strcmp("true", arg) == 0) {
        f(true);
        return true;
      } else if (strcmp("false", arg) == 0) {
        f(false);
        return true;
      } else {
        return false;
      }
    };
    return *this;
  }

  std::string msg_;
  std::function<bool(char const *)> parse_and_apply_;
};
}  // namespace internal

extern int (*ArgonExecute)();
extern std::function<void(char const *)> ArgonHandleOther;
int Parse(int argc, char *argv[]);
void Usage();
}  // namespace argon
