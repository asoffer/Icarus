#include <cxxabi.h>
#include <execinfo.h>
#include <csignal>
#include <string>

namespace init {
void InstallSignalHandlers() {
#ifdef DBG
  std::signal(SIGABRT, +[](int) {
    constexpr unsigned int max_frames = 40;
    fprintf(stderr, "stack trace:\n");
    void *addrlist[max_frames + 1];
    int addrlen = backtrace(addrlist, sizeof(addrlist) / sizeof(void *));
    if (addrlen == 0) {
      fprintf(stderr, "  \n");
    } else {
      char **symbollist = backtrace_symbols(addrlist, addrlen);
      for (int i = 4; i < addrlen; i++) {
        std::string symbol = symbollist[i];
        auto start_iter    = symbol.find('(');
        auto end_iter      = symbol.find(')');
        std::string mangled =
            symbol.substr(start_iter + 1, end_iter - start_iter - 1);
        end_iter = mangled.find('+');
        mangled =
            end_iter >= mangled.size() ? mangled : mangled.substr(0, end_iter);
        char demangled[1024];
        size_t demangled_size = 1024;

        int status;
        abi::__cxa_demangle(mangled.c_str(), &demangled[0], &demangled_size,
                            &status);
        if (status != 0) {
          fprintf(stderr, "#%2d| %s\n", i - 3, symbol.c_str());
        } else {
          if (demangled_size > 70) {
            auto s = std::string(&demangled[0], 70) + " ...";
            fprintf(stderr, "#%2d| %s\n", i - 3, s.c_str());
          } else {
            fprintf(stderr, "#%2d| %s\n", i - 3, demangled);
          }
        }
      }
      free(symbollist);
    }
  });
#endif
}
}  // namespace init
