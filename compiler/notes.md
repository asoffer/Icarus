## Language design decisions:
* Should `(import)` be overloadable? If you have to import a definition of
  `import` this may lose it's value so this this kind of depends on how much of
  a user-defined prelude you to be possible.
* How do named returns work? They shouldn't be visible/usable by the caller, but
  they're interesting in the callee. They give a very nice explanation of when
  we can apply NRVO. But we need ReturnStmt to support calling them by name. And
  then probably also YieldStmt
* Determine if we want to support default return values.
* Determine if we want named return values to be specifiable at the return site as in
  ```
  f ::= () -> (x: i64, y: bool) {
    return y = true, x = 3
  }
  ```
* For most casts, reference qualifiers are stripped, but for casts to the same
  type we could keep them. Would this be a good idea? Currently we're stripping
  them unconditionally
* Figure out if we want to allow something like
  ```
  (x: $x = 0) => x
  ```
  The problem here is the initialization from `0` when `x` isn't constrained.
  I think once we have interfaces We can require that `$x` is constrained to be
  initializable from `0`.
* For specified values on flags, should those be bit positions or the actual flag values?
* Should yields be allowed to have named arguments? It kinda makes sense because
  you're calling a jump handler which has parameters with names, but also if you
  yield a value out of a scope, the name goes to some intermediate thing that users don't
  really see so it feels weird.
