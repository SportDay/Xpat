open Types

val depotLegal : int -> int
val addTestDepot : settings -> unit
val move : int -> int -> settings -> bool
val prepareMove : int -> int -> settings -> bool
val valid_line : string -> bool
val coupLegal : int -> int -> settings -> config -> bool
