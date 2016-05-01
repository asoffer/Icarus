#ifndef ICARUS_DEPENDENCY_FUNCTIONS_H
#define ICARUS_DEPENDENCY_FUNCTIONS_H

namespace Dependency {
// This is terribly wasteful due to poor alignment.
// Maybe a bottleneck for large programs but probably not.
// In any event, for your own pride you should pack these neater.
struct PtrWithTorV {
  PtrWithTorV() = delete;
  PtrWithTorV(AST::Node *ptr, bool torv) : ptr_(ptr), torv_(torv) {}
  AST::Node *ptr_;
  bool torv_; // true => type, false => value
};

extern void type_type(AST::Node *depender, AST::Node *dependee);
extern void type_value(AST::Node *depender, AST::Node *dependee);
extern void value_type(AST::Node *depender, AST::Node *dependee);
extern void value_value(AST::Node *depender, AST::Node *dependee);

extern void traverse_from(PtrWithTorV);

extern void record(AST::Node *node);
extern void mark_as_done(AST::Node *e);
extern void add_to_table(AST::Node *depender);
extern void assign_order();
extern void rebuild_already_seen();
extern void write_graphviz();
} // namespace Dependency

#endif // ICARUS_DEPENDENCY_FUNCTIONS_H
