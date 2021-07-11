type rec pizza<'a> =
  | Bottom
  | Topping(('a, pizza<'a>))

/* 3 */
type fish =
  | Anchovy
  | Lox
  | Tuna
