let sample = "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"

module IntMap = Map.Make(Int);;

let list_of_string s = List.init (String.length s) (String.get s);;

let rec parse_num' l i num len = 
  let c = (List.nth l i) in (
    match c with '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> 
      parse_num' l (i + 1) ((int_of_char c) - 48 + (num * 10)) (len + 1) 
    | _ -> num, len) 
;;

let rec find_num l i = 
  let c = (List.nth l i) in (
    match c with '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> i
    | _ -> find_num l (i + 1)) 

let parse_num l i = parse_num' l i 0 0;;

let rec sum list start fn = match list
  with e :: res -> sum res (start + fn e) fn
  | [] -> start

let rec read_all file l = try read_all file (input_char file :: l) with End_of_file -> List.rev l
let read_file filename = read_all (open_in filename) []

module Part1 = struct
  let rec get_points' l i map = 
    try
      let c = (List.nth l i) in (
        match c with '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' | '\n' -> get_points' l (i + 1) map
        | _ -> get_points' l (i + 1) (IntMap.add i c map))
    with Failure _ -> map
  ;;

  let get_points l = get_points' l 0 IntMap.empty;;

  let rec row_size' l i = let c = (List.nth l i) in if c = '\n' then (i + 1) else row_size' l (i + 1)
  let row_size l = row_size' l 0;;

  exception Found of char;;

  let get_neibor points row_size i l =
    let rec l_add l start en = if start = en then l else l_add (start :: l) (start + 1) en in
    let li = l_add [] ((i - 1) - row_size) ((i + l + 1) - row_size) in
    let li = (i-1) :: (i + l) :: li in
    let li = l_add li ((i - 1) + row_size) ((i + l + 1) + row_size) in
    let li = List.filter_map (fun a -> try let c = IntMap.find a points in Some c with Not_found -> None) li in 
    try Some (List.nth li 0) with Failure _ -> None
  ;;


  let rec get_nums' l i points row_size ret =
    let num, len = parse_num l i in
    let neibor = get_neibor points row_size i len in
    let ret = (num, neibor) :: ret in
    try get_nums' l (find_num l (i + len)) points row_size ret with _ -> ret
  ;;
  let get_nums l i points row_size = get_nums' l i points row_size [];;


  let run data = 
    let row_size = row_size data in
    (Format.printf "Line: %i\n" row_size);
    let points = get_points data in
    IntMap.iter (fun key value -> Format.printf "Point: %i: '%c'\n" key value) points;
    let nums = get_nums data 0 points row_size in
    List.iter (fun (num, neibor) -> match neibor with Some n -> Format.printf "Num: %i, Neibor: %c \n" num n | None -> Format.printf "Num: %i\n" num) nums;
    let nums = List.filter_map (fun (num, neibor) -> match neibor with Some _ -> Some num | None -> None) nums in
    let sum = sum nums 0 (fun n -> n) in
    Format.printf "Sum: %i\n" sum
  ;;
end

let () = Part1.run (read_file "input3.txt")
