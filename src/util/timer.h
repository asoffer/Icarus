#ifndef ICARUS_TIMER_H
#define ICARUS_TIMER_H

struct Timer {
  Timer(){};

  std::vector<const char *> msgs;
  std::vector<size_t> times;

  ~Timer() {
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

// Abusing a for-loop to do timings correctly.
#define RUN(timer, msg)                                                        \
  for (bool TIME_FLAG = true;                                                  \
      start_time  = mach_absolute_time(), TIME_FLAG;                           \
      end_time    = mach_absolute_time(),                                      \
      timer.msgs.push_back(msg),                                               \
      timer.times.push_back(end_time - start_time),                            \
      TIME_FLAG = false)
#endif // ICARUS_TIMER_H
