#ifndef ICARUS_UTIL_TIMER_H
#define ICARUS_UTIL_TIMER_H

#include <chrono>
#include <numeric>
#include <vector>
#include <cstdio>

#include "../base/types.h"

namespace debug {
extern bool timer;
} // namespace debug

struct Timer {
  Timer() = default;

  std::chrono::time_point<std::chrono::high_resolution_clock> start, end;

  std::vector<const char *> msgs;
  std::vector<std::chrono::duration<double>> times;

  ~Timer() {
    if (debug::timer) {
      auto total = std::accumulate(times.begin(), times.end(),
                                   std::chrono::duration<double>());
      for (size_t i = 0; i < msgs.size(); ++i) {
        char percent_buffer[10];
        sprintf(percent_buffer, "(%.2f%%)",
                100 * times[i].count() / total.count());
        fprintf(
            stderr, "%25s:%15luns %9s\n", msgs[i],
            static_cast<u64>(
                std::chrono::duration_cast<std::chrono::nanoseconds>(times[i])
                    .count()),
            percent_buffer);
      }

      fprintf(stderr, "%25s:%15luns\n", "Total",
              static_cast<u64>(
                  std::chrono::duration_cast<std::chrono::nanoseconds>(total)
                      .count()));
    }
  }
};

// Abusing a for-loop to get scoping to look correct
#define RUN(timer, msg)                                                        \
  for (bool TIME_FLAG = true;                                                  \
       timer.start = std::chrono::high_resolution_clock::now(), TIME_FLAG;     \
       timer.end = std::chrono::high_resolution_clock::now(),                  \
            timer.msgs.push_back(msg),                                         \
            timer.times.push_back(timer.end - timer.start), TIME_FLAG = false)

#endif // ICARUS_UTIL_TIMER_H
