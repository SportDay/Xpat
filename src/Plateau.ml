open Types
open Card
type tmpList = {mutable list: int List.t;}
let plateau = ref {depot = FArray.empty; colonnes = FArray.make 1 []; temp = FArray.empty; numCardCollone = 0}
let config = { game = Midnight; seed = 1; mode = Search "" }

(* Affiche le depot *)
let printDepot (depot: int FArray.t) =
  let rec aux i t =
    if i > t then ()
    else
      begin
        Printf.printf "%s: Nombre de carte: %d\n" (Card.suit_to_full_string (Card.suit_of_num i)) (FArray.get depot i);
        aux (i+1) t
      end
  in
  aux 0 (FArray.length depot - 1)

(* Affiche tout les elements des colones *)
let printCollones (collone: int List.t FArray.t) =
    let rec printCollones = function
      | [] -> ()
      | x::xs -> Printf.printf "%s " (Card.to_string (Card.of_num x)); printCollones xs in
    for i = 0 to FArray.length collone - 1 do
        Printf.printf "Colonne %d: " (i+1);
        Printf.printf "Longueur %d: " (List.length (FArray.get collone i));
        printCollones (List.rev (FArray.get collone i));
        print_newline ()
    done

(* Affiche tout les elements du registere*)
let printRegistre (registre: int FArray.t) =
  let rec aux i t =
    if i > t then ()
    else
    let card = FArray.get registre i in
    if card = -100 then
        print_string("Pas de registre\n")
    else
        if card = -99 then
            begin
                Printf.printf "Registre %d: Vide\n" (i);
                aux (i+1) t
            end
        else
          begin
            Printf.printf "Registre %d: Carte: %s\n" (i) (Card.to_string (Card.of_num card));
            aux (i+1) t
          end
  in
  aux 0 (FArray.length registre - 1)

(* Affiche tout le jeu *)
let printSettings tmp =
    print_string "\n--- Voici les paramètres du jeu ---\n";
    Printf.printf "Type: %s\n" (match config.game with
        | Freecell -> "Freecell"
        | Seahaven -> "Seahaven Towers"
        | Midnight -> "Midnight Oil"
        | Baker -> "Baker's Dozen");
    print_string "--- Depot ---\n";
    printDepot tmp.depot;
    print_string "--- Collones ---\n";
    printCollones tmp.colonnes;
    print_string "--- Registre Temporaire ---\n";
    printRegistre tmp.temp;
    print_string "\n"

let kingTmp = ref {list = []}
let fifoTmp = ref {list = []}


(* Initalisation du plateau en fonction du type de jeu *)
let initGame game =
   let _ = match game with
            | Freecell -> plateau := {depot = FArray.make 4 0; colonnes = FArray.make 8 []; temp = FArray.make 4 (-99); numCardCollone = 7}
            | Seahaven -> plateau := {depot = FArray.make 4 0; colonnes = FArray.make 10 []; temp = FArray.make 4 (-99); numCardCollone = 5}
            | Midnight -> plateau := {depot = FArray.make 4 0; colonnes = FArray.make 18 []; temp = FArray.empty; numCardCollone = 3}
            | Baker -> plateau := {depot = FArray.make 4 0; colonnes = FArray.make 13 []; temp = FArray.empty; numCardCollone = 4}
         in
   let cardInCollone = !plateau.numCardCollone in
   let permut = XpatRandom.shuffle config.seed in
   let rec fill (numCollone : int)(numCard : int)(permut : int list)(list : int List.t FArray.t)(maxCard : int) =
        match permut with
        | [] -> !plateau.colonnes <- list
        | x::xs -> if numCollone >= FArray.length !plateau.colonnes then
                       let rec addCardTemp (permut : int list)(temp : int FArray.t)(pos : int)(length:int) =
                          match permut with
                          | [] -> !plateau.temp <- temp; !plateau.colonnes <- list
                          | x::t -> if pos > length then
                                        failwith "Error: trop de cartes dans le jeu."
                                    else
                                        let ho = FArray.set temp pos x in addCardTemp t ho (pos+1) length
                          in
                          addCardTemp permut !plateau.temp 0 (FArray.length !plateau.temp - 1)
                   else
                       if numCard < maxCard then
                           let newlist = if game = Baker then(
                                            if (Card.real_card_num x) = 13 then(
                                                !kingTmp.list <- x::!kingTmp.list;
                                                FArray.set list numCollone (x::(FArray.get list numCollone))
                                            )else(
                                                !fifoTmp.list <- x::!fifoTmp.list;
                                                FArray.set list numCollone (x::(FArray.get list numCollone))
                                                )
                                        )else
                                            FArray.set list numCollone (x::(FArray.get list numCollone))

                            in
                           fill numCollone (numCard+1) xs newlist maxCard
                       else
                           let maxCard = match game with
                               | Freecell -> if maxCard mod 2 = 0 then maxCard+1 else maxCard-1
                               | _ -> cardInCollone in
                           if game = Baker then(
                                   let newlist = ((!fifoTmp.list)@(!kingTmp.list)) in
                                   let tmp = FArray.set list numCollone newlist in
                                      kingTmp := {list = []};
                                        fifoTmp := {list = []};
                                   fill (numCollone+1) 0 permut tmp maxCard
                            )else
                                fill (numCollone+1) 0 permut list maxCard
    in
    fill 0 0 permut !plateau.colonnes cardInCollone


(* Fonction qui calcule le score *)
let calcScore(plat: settings) =
  let rec aux i t score =
    if i > t then score
    else
      begin
        aux (i+1) t (score + (FArray.get plat.depot i))
      end
  in
  aux 0 (FArray.length plat.depot - 1) 0