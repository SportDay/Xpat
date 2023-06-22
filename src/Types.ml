type game = Freecell | Seahaven | Midnight | Baker
type mode =
  | Check of string (* filename of a solution file to check *)
  | Search of string (* filename where to write the solution *)
type settings = {mutable depot: int FArray.t; mutable colonnes: int List.t FArray.t; mutable temp: int FArray.t; numCardCollone: int}
type state = {score: int; plat: settings; history: (int * int) list}
type config = { mutable game : game; mutable seed: int; mutable mode: mode }
