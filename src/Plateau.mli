open Types
val config : config
type tmpList = { mutable list : int list; }
val plateau : settings ref
val printDepot : int FArray.t -> unit
val printCollones : int list FArray.t -> unit
val printRegistre : int FArray.t -> unit
val printSettings : settings -> unit
val kingTmp : tmpList ref
val fifoTmp : tmpList ref
val initGame : game -> unit
val calcScore : settings -> int