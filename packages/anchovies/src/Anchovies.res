/* 1 */
type rec pizza<'a> =
  | Bottom
  | Topping(('a, pizza<'a>))

/* 3 */
type fish =
  | Anchovy
  | Lox
  | Tuna

/* 11 */
let rec rem_anchovy' = x =>
  switch x {
  | Bottom => Bottom
  | Topping((Anchovy, p)) => rem_anchovy'(p)
  | Topping((Tuna, p)) => Topping((Tuna, rem_anchovy'(p)))
  | Topping((Lox, p)) => Topping((Lox, rem_anchovy'(p)))
  }

let rec rem_anchovy2 = x =>
  switch x {
  | Bottom => Bottom
  | Topping((Anchovy, p)) => rem_anchovy2(p)
  | Topping((Tuna, p)) => Topping((Tuna, rem_anchovy2(p)))
  | Topping((Lox, p)) => Topping((Lox, rem_anchovy2(p)))
  }

let rec rem_anchovy3 = x =>
  switch x {
  | Bottom => Bottom
  | @implicit_arity Topping(Anchovy, p) => rem_anchovy3(p)
  | @implicit_arity Topping(Tuna, p) => @implicit_arity Topping(Tuna, rem_anchovy3(p))
  | @implicit_arity Topping(Lox, p) => @implicit_arity Topping(Lox, rem_anchovy3(p))
  }

let rec rem_anchovy4' = x =>
  switch x {
  | Bottom => Bottom
  | @implicit_arity Topping(Anchovy, p) => rem_anchovy4'(p)
  | @implicit_arity Topping(Tuna, p) => @implicit_arity Topping(Tuna, rem_anchovy4'(p))
  | @implicit_arity Topping(Lox, p) => @implicit_arity Topping(Lox, rem_anchovy4'(p))
  }
