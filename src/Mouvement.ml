open Card
open Plateau
open Types


(* Renvoie true si on peut deplacer la carte vers le depot *)
let depotLegal(card : int) =
    let pos = Card.num_of_suit (Card.suit_of_num (card land 3)) in
     if FArray.length !plateau.depot = 0 then 4096
     else if pos >= FArray.length !plateau.depot then 4096
     else if (FArray.get !plateau.depot pos) = (card lsr 2) then pos
     else 4096


(* Teste sur touts les carte du register et des colones le mouvement vers le depot *)
let rec addTestDepot plat =
    let rec testInColonne (pos : int)(rep : bool) =
        if pos >= FArray.length plat.colonnes then
           rep
        else
           let lt = FArray.get plat.colonnes pos in
               if (List.length lt) = 0 then
                    testInColonne (pos+1) rep
               else
           let cardTmp = List.hd lt in
           let tmpList = List.tl lt in
           let posDepot = depotLegal cardTmp in
               if posDepot <> 4096 then (
                   Printf.printf "\nCarte %s ajoutée au depot\n" (Card.to_string (Card.of_num cardTmp));
                   !plateau.colonnes <- FArray.set plat.colonnes pos tmpList;
                   !plateau.depot <- FArray.set plat.depot posDepot ((cardTmp lsr 2)+1);
                   testInColonne (pos+1) true
                   )
               else
                   testInColonne (pos+1) rep
        in
    let rec testInTemp (pos : int)(rep : bool) =
        if pos >= FArray.length plat.temp then
              rep
          else
              let card = FArray.get plat.temp pos in
              let posDepot = depotLegal card in
                    if posDepot <> 4096 then begin
                            Printf.printf "\nCarte %s ajoutée au depot\n" (Card.to_string (Card.of_num card));
                            !plateau.temp <- FArray.set plat.temp pos (-99);
                            !plateau.depot <- FArray.set plat.depot posDepot ((card lsr 2)+1);
                            testInTemp (pos+1) true end
                    else
                        testInTemp (pos+1) rep
        in
        if (testInColonne 0 false) then
         begin
         addTestDepot !plateau;
         if (testInTemp 0 false) then
            addTestDepot !plateau
         else
           ()
         end
        else
         if (testInTemp 0 false) then
             begin
             addTestDepot !plateau;
             if (testInColonne 0 false) then
                addTestDepot !plateau
             else
               ()
             end
         else
             ()

(* Renvoie la position si la carte existe dans les colones et 4096 sinon *)
let findCard_Col(card : int)(plat:settings) =
    if FArray.length plat.colonnes = 0 then
        4096
    else
        let rec find_card_aux (card : int)(pos : int) =
            if pos >= FArray.length plat.colonnes then
                4096
            else
                let lt = FArray.get plat.colonnes pos in
                if (List.length lt) = 0 then
                    find_card_aux card (pos+1)
                else
                let cardTmp = List.hd lt in
                if card = cardTmp then
                    pos
                else
                    find_card_aux card (pos+1)
        in
        find_card_aux card 0

(* Renvoie la position si la carte existe dans le registe et 4096 sinon *)
let findCard_Reg (card : int)(plat:settings) =
        if FArray.length plat.temp = 0 then
            4096
        else
            let rec find_card_aux (card : int)(pos : int) =
                if pos >= FArray.length plat.temp then
                    4096
                else
                    if card = FArray.get plat.temp pos then
                        pos
                    else
                        find_card_aux card (pos+1)
            in
            find_card_aux card 0

(* Renvoie la position de la colone vide ou 4096 sinon *)
let findEmptyCol(plat:settings) =
    if FArray.length plat.colonnes = 0 then
        4096
    else
        let rec find_card_aux (list : int List.t FArray.t)(pos : int) =
            if pos >= FArray.length list then
                4096
            else
                if List.length (FArray.get list pos) = 0 then
                    pos
                else
                    find_card_aux list (pos+1) in
        find_card_aux plat.colonnes 0

(* Determine le type et la position de la carte *)
let findTypeCard(card : int)(plat:settings) =
    if card == 1024 then
       (2, findEmptyCol plat)
    else
        if card == 2048 then
            (1, findCard_Reg (-99) plat)
        else
            let pos = findCard_Col card plat in
                if pos = 4096 then
                    (1, findCard_Reg card plat)
                else
                    (0, pos)

(* Determine si la carte 1 à la bonne couleur pour aller sur la carte 2*)
let check_color c1 c2 =
  match config.game with
  | Freecell -> let red1 = Card.red_card c1 in
                let red2 = Card.red_card c2 in
                if red1 = red2 then false else true
  | Seahaven | Midnight -> if (Card.get_suit c1) = (Card.get_suit c2) then true else false
  | Baker -> true

(* Determine si le coup est legal *)
let coupLegal (card1: int) (card2: int) (plat:settings) (config: config) =
    let (type1, pos1) = findTypeCard card1 plat in
    let (type2, pos2) = findTypeCard card2 plat in
    if (pos1 <> 4096) && (pos2 <> 4096) then
        if type1 = 0 && type2 = 1 then true
            else
        match config.game with
            | Freecell -> if List.length (FArray.get plat.colonnes pos2) = 0 then true
             else
                let plusPetit = if (Card.real_card_num card1) < (Card.real_card_num card2) then true else false in
                if not(check_color card1 card2) then false
                else if plusPetit then true
                else false
            | Seahaven -> if (List.length (FArray.get plat.colonnes pos2) = 0) && ((Card.real_card_num card1) = 13) then true
             else if check_color card1 card2 then if (Card.real_card_num card1) < (Card.real_card_num card2) then true else false
             else false
            | Midnight -> if (List.length (FArray.get plat.colonnes pos2) = 0) then false
             else if check_color card1 card2 then if (Card.real_card_num card1) < (Card.real_card_num card2) then true else false
             else false
            | Baker ->  if (List.length (FArray.get plat.colonnes pos2) = 0) then false
             else if (Card.real_card_num card1) < (Card.real_card_num card2) then true else false
    else false

(* 1024 = vide *)
(* 2048 = registre *)
(* Renvoie true si le mouvement de la card1 sur la card2 a reussit et false si non *)
let move (card1: int)(card2: int) (plat:settings) =
    let (type1, pos1) = findTypeCard card1 plat in
    let (type2, pos2) = findTypeCard card2 plat in
        if coupLegal card1 card2 plat config then
            match (type1,type2) with
            (* Si on veut déplacer une carte de la colonne vers une colonne (vide ou non)*)
            | (0,0) | (0,2) -> let card1 = List.hd (FArray.get !plateau.colonnes pos1) in
                                 let tmpColonne1 = List.tl (FArray.get !plateau.colonnes pos1) in
                                 let colonnes = FArray.set plat.colonnes pos1 tmpColonne1 in
                                 let tmpColonne2 = card1::(FArray.get colonnes pos2) in
                                 !plateau.colonnes <- FArray.set colonnes pos2 tmpColonne2;
                                 true
            (* Si on veut déplacer une carte de registre vers une colonne *)
            | (1,0) -> let tmpColonne2 = card1::(FArray.get plat.colonnes pos2) in
                       !plateau.colonnes <- FArray.set plat.colonnes pos2 tmpColonne2;
                       !plateau.temp <- FArray.set plat.temp pos1 (-99);
                       true
            (* Si on veut déplacer une carte de la colonne vers le registre *)
            | (0,1) -> let card1 = List.hd (FArray.get plat.colonnes pos1) in
                       let tmpColonne1 = List.tl (FArray.get plat.colonnes pos1) in
                       let tmpRegistre = FArray.set plat.temp pos2 card1 in
                       !plateau.colonnes <- FArray.set plat.colonnes pos1 tmpColonne1;
                       !plateau.temp <- tmpRegistre;
                       true
            (* Si on veut déplacer une carte de registre vers une collone vide *)
            | (1,2) -> let tmpColonne2 = card1::(FArray.get plat.colonnes pos2) in
                      let newTemp = FArray.set plat.temp pos1 (-99) in
                      !plateau.colonnes <- FArray.set plat.colonnes pos2 tmpColonne2;
                      !plateau.temp <- newTemp;
                      true
           | _ -> false
    else false



(* Effectue le mouvement de card1 sur card2 et la mise aux deports automatique *)
let prepareMove (card1: int)(card2: int)(plat:settings) =
    let _ = addTestDepot plat in
    let temp = move card1 card2 plat in
    let _ = addTestDepot plat in
    temp

(* A partir d'un string effectue le mouvement  *)
let valid_line line =
  match String.split_on_char ' ' line with
  | [c1; c2] -> if c2 = "V" then (Printf.printf "Move %s -> Vide\n" (Card.to_string (Card.of_num (int_of_string c1))); prepareMove (int_of_string c1) 1024 !plateau)
                else if c2 = "T" then (Printf.printf "Move %s -> Registre\n" (Card.to_string (Card.of_num (int_of_string c1))); prepareMove (int_of_string c1) 2048 !plateau)
                else (Printf.printf "Move %s -> %s \n" (Card.to_string (Card.of_num (int_of_string c1))) (Card.to_string (Card.of_num (int_of_string c2))); prepareMove (int_of_string c1) (int_of_string c2) !plateau)
  | _ -> false