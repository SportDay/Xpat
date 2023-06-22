open XpatLib
open Card
open Plateau
open Types
open Mouvement

let getgame = function
  | "FreeCell"|"fc" -> Freecell
  | "Seahaven"|"st" -> Seahaven
  | "MidnightOil"|"mo" -> Midnight
  | "BakersDozen"|"bd" -> Baker
  | _ -> raise Not_found

let split_on_dot name =
  match String.split_on_char '.' name with
  | [string1;string2] -> (string1,string2)
  | _ -> raise Not_found

let set_game_seed name =
  try
    let (sname,snum) = split_on_dot name in
    Plateau.config.game <- getgame sname;
    config.seed <- int_of_string snum
  with _ -> failwith ("Error: <game>.<number> expected, with <game> in "^
                      "FreeCell Seahaven MidnightOil BakersDozen")

let checkWin plat =
    let rec testDepot (pos : int) =
       if pos >= FArray.length plat.depot then
             true
       else
         let num = FArray.get plat.depot pos in
         if num <> 13 then
             false
         else
             testDepot (pos + 1) in
             testDepot 0

let check_file filename =
  let file = open_in filename in
  let rec read nb =
    try
        let line = input_line file in
        if not (valid_line line) then nb
        else read (nb + 1)
    with End_of_file -> close_in file; nb
  in let nb = read 1 in
  if checkWin !plateau then (let str = "SUCCES" in print_string str; exit 0)
  else (let str = "ECHEC " ^ string_of_int nb in print_string str; exit nb)

let rec compare_reg r1 r2 i =
  if i = FArray.length r1 then 0
  else
    let e1 = FArray.get r1 i in
    if FArray.exists (fun x -> x = e1) r2 then compare_reg r1 r2 (i + 1) else 1

let compare_state s1 s2 = 
  if s1.score = s2.score then
    if compare_reg s1.plat.temp s2.plat.temp 0 = 0 then
      if Stdlib.compare s1.plat.depot s2.plat.depot = 0 then
        if Stdlib.compare s1.plat.colonnes s2.plat.colonnes = 0 then 0
        else 1
      else 1
    else 1
  else 1

module States = Set.Make (struct type t = state let compare = compare_state end)


let startState = {score = 0; plat = !plateau; history = []}

let getNewState(stat: state)(move: (int * int)) = {score = (calcScore stat.plat); plat = stat.plat; history = (stat.history @ [move])}

let solver(hist:(int * int) list) =
  let rec solver_aux(plat: settings) (c1:int) (c2:int) (num:int) (hist:(int * int) list) =
    if num = 0 then hist
    else
        let c1 = if c1 >= FArray.length plat.colonnes then 0 else c1 in
        let rec solve_col(plat: settings) (c2:int) (hist:(int * int) list) =
                let c2 = if c2 == c1 then (c2+1) else c2 in
                if c2 >= FArray.length plat.colonnes then plat, hist else
                let card1 = try List.hd (FArray.get plat.colonnes c1) with
                                _ -> 4096 in
                let card2 = try List.hd (FArray.get plat.colonnes c2) with
                                _ -> 4096 in
                if card1 <> 4096 && card2 <> 4096 then
                  if prepareMove card1 card2 plat then
                    solve_col plat (c2+1) (hist @ [(card1, card2)])
                  else
                      if prepareMove card2 card1 plat then
                        solve_col plat (c2+1) (hist @ [(card2, card1)])
                      else
                        if prepareMove card1 2048 plat then
                            solve_col plat (c2+1) (hist @ [(card1, 2048)])
                          else
                           if prepareMove card2 2048 plat then
                            solve_col plat (c2+1) (hist @ [(card2, 2048)])
                           else
                            if prepareMove card1 1024 plat then
                                solve_col plat (c2+1) (hist @ [(card1, 1024)])
                               else
                                if prepareMove card2 1024 plat then
                                    solve_col plat (c2+1) (hist @ [(card2, 1024)])
                                   else
                                    solve_col plat (c2+1) hist
                else
                    if card1 = 4096 then
                        if prepareMove card2 1024 plat then
                            solve_col plat (c2+1) (hist @ [(card2, 1024)])
                           else
                            solve_col plat (c2+1) hist
                    else if card2 = 4096 then
                        if prepareMove card1 1024 plat then
                            solve_col plat (c2+1) (hist @ [(card1, 1024)])
                        else
                            solve_col plat (c2+1) hist
                    else
                        solve_col plat (c2+1) hist
             in let plat, hist = solve_col plat c2 hist in
             solver_aux plat (c1+1) 0 (num - 1) hist
        in solver_aux !plateau 0 0 2000 hist


let search_sol filename =
  let history = solver [] in
  let file = open_out filename in
  let rec write history =
    match history with
    | [] -> ()
    | (a, b) :: t -> (if b = 1024 then Printf.fprintf file "%d V\n" a
                      else if b = 2048 then Printf.fprintf file "%d T\n" a
                      else Printf.fprintf file "%d %d\n" a b);
                     write t
  in write history;
  close_out file




let rec print_tuples =
  function
  | [] -> Printf.printf "Vide\n"
  | (a, b) :: rest ->
    Printf.printf "[%d, %d]; " a b;
    print_tuples rest


let main () =
  Arg.parse
    [("-check", String (fun filename -> config.mode <- Check filename),
        "<filename>:\tValidate a solution file");
     ("-search", String (fun filename -> config.mode <- Search filename),
        "<filename>:\tSearch a solution and write it to a solution file")]
    set_game_seed (* pour les arguments seuls, sans option devant *)
    "XpatSolver <game>.<number> : search solution for Xpat2 game <number>";

  initGame config.game;
  printSettings !plateau;

  let _ =
      match config.mode with
      | Check name -> check_file name
      | Search name -> search_sol name
    in
  printSettings !plateau

let _ = if not !Sys.interactive then main () else ()
