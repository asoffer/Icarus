#ifndef ICARUS_DEPENDENCY_TYPES_H
#define ICARUS_DEPENDENCY_TYPES_H

namespace AST {
  struct Node;
}  // namespace AST

namespace Dependency {
  enum Flag : char {
    unseen    = 0x00,
    type_seen = 0x01,
    val_seen  = 0x04,
    tv_seen   = 0x05,
    type_done = 0x02,
    val_done  = 0x08,
    tv_done   = 0x0a
  };

  // This is terribly wasteful due to poor alignment.
  // Maybe a bottleneck for large programs but probably not.
  // In any event, for your own pride you should pack these neater.
  struct PtrWithTorV {
    PtrWithTorV() = delete;
    PtrWithTorV(AST::Node *ptr, bool torv) : ptr_(ptr), torv_(torv) {}
    AST::Node *ptr_;
    bool torv_; // true => type, false => value
  };

}  // namespace Dependency

namespace std {
  template<> struct less<Dependency::PtrWithTorV> {
    bool operator()(const Dependency::PtrWithTorV& lhs, const Dependency::PtrWithTorV& rhs) const {
      if (lhs.ptr_ != rhs.ptr_) return lhs.ptr_ < rhs.ptr_;
      if (lhs.torv_ != rhs.torv_) return lhs.torv_ < rhs.torv_;
      return false;
    }
  };
}  // namespace std


#endif  // ICARUS_DEPENDENCY_TYPES_H
