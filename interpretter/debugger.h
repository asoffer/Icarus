#ifndef ICARUS_INTERPRETTER_DEBUGGER_H
#define ICARUS_INTERPRETTER_DEBUGGER_H

namespace interpretter {

struct Debugger {
  void pause() { paused_ = true; };
  bool paused() const { return paused_; };

 private:
  bool paused_ = false;
};

}  // namespace interpretter

#endif  // ICARUS_INTERPRETTER_DEBUGGER_H
