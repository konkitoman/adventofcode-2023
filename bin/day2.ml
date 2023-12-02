type game't = {
  game: int;
  r: int;
  g: int;
  b: int;
};;

type color = Red | Green | Blue;;

exception Cannot_Parse;;
exception Cannot_Parse_Num;;
exception Cannot_Parse_Color;;

let rec parse_num = fun str start -> match str
  with c :: res -> (match c 
    with '0' -> parse_num res (start * 10)
    | '1' -> parse_num res ((start * 10) + 1)
    | '2' -> parse_num res ((start * 10) + 2) 
    | '3' -> parse_num res ((start * 10) + 3) 
    | '4' -> parse_num res ((start * 10) + 4) 
    | '5' -> parse_num res ((start * 10) + 5)
    | '6' -> parse_num res ((start * 10) + 6) 
    | '7' -> parse_num res ((start * 10) + 7) 
    | '8' -> parse_num res ((start * 10) + 8) 
    | '9' -> parse_num res ((start * 10) + 9)
    | _ -> start, res)
  | _ -> raise Cannot_Parse_Num
;;

let parse_color = fun str -> match str
  with 'r' :: 'e' :: 'd' :: res -> Red, res
  | 'g' :: 'r' :: 'e' :: 'e' :: 'n' :: res -> Green, res
  | 'b' :: 'l' :: 'u' :: 'e' :: res -> Blue, res
  | _ -> raise Cannot_Parse_Color

let rec parse_colors = fun str r g b -> match str
  with ' ' :: res -> parse_colors res r g b
  | ',' :: res -> parse_colors res r g b
  | c :: res -> (match c
    with '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> 
      let num, res = parse_num str 0 in
      let color, res = parse_color res in
      (match color 
      with Red -> parse_colors res (max r num) g b
      | Green -> parse_colors res r (max g num) b
      | Blue -> parse_colors res r g (max b num))
    | '\n' -> res, r, g, b
    | _ -> parse_colors res r g b
    )
  | [] -> raise End_of_file


let parse = fun str -> match str
  with 'G' :: 'a' :: 'm' :: 'e' :: ' ' :: res -> 
    let num, res = parse_num res 0 in
    let res, r, g, b = parse_colors res 0 0 0 in
    res, {game=num; r; g; b}
  | [] -> raise End_of_file
  | _ -> raise Cannot_Parse 
  ;;

let rec get_games = fun str l-> 
  try let res, game = parse str in 
  get_games res (game :: l)
  with End_of_file -> str, l

let filter = fun games r g b -> List.filter (fun game -> game.r <= r && game.g <= g && game.b <= b ) games

let rec sum = fun list start fn -> match list
  with e :: res -> sum res (start + fn e) fn
  | [] -> start

let games_sum = fun games -> sum games 0 (fun g -> g.game)

let run = fun games r g b -> 
  (Format.printf "Max r: %i, g: %i, b: %i\n" r g b);
  let games = filter games r g b in
  let sum = games_sum games in
  Format.printf "Sum: %i\n" sum

let rec read_all = fun file str -> try read_all file (input_char file :: str) with End_of_file -> List.rev str

let read_file = fun filename -> read_all (open_in filename) []

let file_data = read_file "input2.txt";;
let _,games = get_games file_data [];;

let () = run games 12 13 14



let games_sum'5 = fun games -> sum games 0 (fun g -> g.r * g.g * g.b)

let run'5 = fun games -> 
  let sum = games_sum'5 games in
  Format.printf "Sum'5: %i\n" sum

let _,games = get_games file_data [];;

let () = run'5 games
