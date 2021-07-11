open Anchovy
let rec rem_anchovy = x =>
  switch x {
  | Bottom => Bottom
  | @implicit_arity Topping(Anchovy, p) => rem_anchovy(p)
  | @implicit_arity Topping(Tuna, p) => @implicit_arity Topping(Tuna, rem_anchovy(p))
  | @implicit_arity Topping(Lox, p) => @implicit_arity Topping(Lox, rem_anchovy(p))
  }
