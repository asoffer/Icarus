#ifndef ICARUS_BASE_LAZY_CONVERT_H
#define ICARUS_BASE_LAZY_CONVERT_H

namespace base {
template <typename Fn>
struct lazy_convert {
  using result_type = decltype(std::declval<Fn>()());
  lazy_convert(Fn fn) : fn_(std::move(fn)) {}

  /* implicit */ operator result_type() && { return std::move(fn_)(); }

 private:
  Fn fn_;
};

}  // namespace base

#endif  // ICARUS_BASE_LAZY_CONVERT_H
