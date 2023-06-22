
(** Cards *)

type rank = int (* 1 to 13, valet=11, dame=12, roi=13 *)
type suit = Trefle | Pique | Coeur | Carreau
type card = rank * suit

(** The order of suits is the one of Xpat2

   List.map int_of_suit [Trefle;Pique;Coeur;Carreau] = [0;1;2;3]
*)

type suitnum = int (* 0..3 *)

let num_of_suit = function
  | Trefle -> 0
  | Pique -> 1
  | Coeur -> 2
  | Carreau -> 3

let suit_of_num = function
  | 0 -> Trefle
  | 1 -> Pique
  | 2 -> Coeur
  | 3 -> Carreau
  | _ -> assert false

(** From 0..51 to cards and back (the Xpat2 way) *)

type cardnum = int (* 0..51 *)

let of_num n = (n lsr 2)+1, suit_of_num (n land 3)
let to_num (rk,s) = num_of_suit s + (rk-1) lsl 2

(** Display of a card *)

let suit_to_string = function
  | Trefle -> "Tr"
  | Pique -> "Pi"
  | Coeur -> "Co"
  | Carreau -> "Ca"

let suit_to_full_string = function
    | Trefle -> "Trefle"
    | Pique -> "Pique"
    | Coeur -> "Coeur"
    | Carreau -> "Carreau"

let rank_to_string = function
  | 13 -> "Ro"
  | 12 -> "Da"
  | 11 -> "Va"
  | n -> string_of_int n

let to_string (rk,s) = rank_to_string rk ^ suit_to_string s

let red_card (num : cardnum) = match of_num num with
  | _, Coeur | _, Carreau -> true
  | _ -> false

let real_card_num (num : cardnum) = (num lsr 2)+1

let get_suit (num : cardnum) = suit_of_num (num land 3)
