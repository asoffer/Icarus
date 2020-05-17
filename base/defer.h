#ifndef ICARUS_BASE_DEFER_H
#define ICARUS_BASE_DEFER_H

namespace base {

// `defer` is a class-template accepting a no-argument invokable which runs the
// invokable in its destructor. `defer` is intended to be used with
// Class Template Argument Deduction. One should avoid specifying the template
// arguments for the callable if possible.
template <typename Fn>
struct defer {
  defer(Fn &&fn) : fn_(static_cast<Fn &&>(fn)) {}
  ~defer() { static_cast<Fn &&>(fn_)(); }

 private:
  Fn fn_;
};

template <typename Fn>
defer(Fn &&)->defer<Fn>;

}  // namespace base

#endif  // ICARUS_BASE_DEFER_H
