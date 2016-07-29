#ifndef ICARUS_UTIL_TIMER_H
#define ICARUS_UTIL_TIMER_H

#include <mach/mach.h>
#include <mach/mach_time.h>

struct Timer {
  Timer() : start(0), end(0){};

  size_t start, end;

  std::vector<const char *> msgs;
  std::vector<size_t> times;

  ~Timer(){
    if (debug::timer) {
      size_t total = 0;
      for (auto time : times) { total += time; }

      for (size_t i = 0; i < msgs.size(); ++i) {
        char percent_buffer[10];
        sprintf(percent_buffer, "(%.2f%%)", ((double)(100 * times[i])) / total);
        fprintf(stderr, "%25s:%15luns %9s\n", msgs[i], times[i],
                percent_buffer);
      }

      fprintf(stderr, "%25s:%15luns\n", "Total", total);
    }
  }
};

// Abusing a for-loop to get scoping to look correct
#define RUN(timer, msg)                                                        \
  for (bool TIME_FLAG = true;                                                  \
      timer.start  = mach_absolute_time(), TIME_FLAG;                          \
      timer.end    = mach_absolute_time(),                                     \
      timer.msgs.push_back(msg),                                               \
      timer.times.push_back(timer.end - timer.start),                          \
      TIME_FLAG = false)

#endif // ICARUS_UTIL_TIMER_H
