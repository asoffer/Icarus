namespace base {
namespace internal {
struct hasher {
  size_t value = 0x9e3779b97f681513ull;
};
template <typename T>
hasher &operator<<(hasher &h, const T &arg) {
  h.value ^= std::hash<T>{}(arg) + 0x9e3779b9 + (h.value << 6) + (h.value >> 2);
  return h;
}
}  // namespace internal

template <typename... Args>
size_t hash_args(const Args &... args) {
  internal::hasher h;
  return (h << ... << args).value;
}
}  // namespace base
