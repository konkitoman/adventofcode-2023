let rec get_num = fun str -> 
  match str with 
    | ch :: res -> (match ch with 
      '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> (int_of_char ch) - 48
      | _ -> get_num res)
    | _ -> 0
  

let rec adventofcode1 = fun file start -> 
  try
    let line = input_line file in 
    let chars = List.init (String.length line) (String.get line) in
    let first = get_num chars in
    let last = get_num (List.rev chars) in
    let num = (first * 10) + last in
    adventofcode1 file (num + start)
  with _ -> start

let res = adventofcode1 (open_in "input1.txt") 0
let () = print_endline (string_of_int res)



let rec get_num'5 = fun str -> 
  match str with 
      'z' :: 'e' :: 'r' :: 'o'        :: _ -> 0
    | 'o' :: 'n' :: 'e'               :: _ -> 1
    | 't' :: 'w' :: 'o'               :: _ -> 2
    | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ -> 3
    | 'f' :: 'o' :: 'u' :: 'r'        :: _ -> 4
    | 'f' :: 'i' :: 'v' :: 'e'        :: _ -> 5
    | 's' :: 'i' :: 'x'               :: _ -> 6
    | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ -> 7
    | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ -> 8
    | 'n' :: 'i' :: 'n' :: 'e'        :: _ -> 9
    | ch :: res -> (match ch with 
      '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> (int_of_char ch) - 48
      | _ -> get_num'5 res)
    | _ -> 0

let rec get_num_rev'5 = fun str -> 
  match str with 
      'o' :: 'r' :: 'e' :: 'z'        :: _ -> 0
    | 'e' :: 'n' :: 'o'               :: _ -> 1
    | 'o' :: 'w' :: 't'               :: _ -> 2
    | 'e' :: 'e' :: 'r' :: 'h' :: 't' :: _ -> 3
    | 'r' :: 'u' :: 'o' :: 'f'        :: _ -> 4
    | 'e' :: 'v' :: 'i' :: 'f'        :: _ -> 5
    | 'x' :: 'i' :: 's'               :: _ -> 6
    | 'n' :: 'e' :: 'v' :: 'e' :: 's' :: _ -> 7
    | 't' :: 'h' :: 'g' :: 'i' :: 'e' :: _ -> 8
    | 'e' :: 'n' :: 'i' :: 'n'        :: _ -> 9
    | ch :: res -> (match ch with 
      '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> (int_of_char ch) - 48
      | _ -> get_num_rev'5 res)
    | _ -> 0

let rec adventofcode1'5 = fun file start -> 
  try
    let line = input_line file in 
    let chars = List.init (String.length line) (String.get line) in
    let first = get_num'5 chars in
    let last = get_num_rev'5 (List.rev chars) in
    let num = (first * 10) + last in
    adventofcode1'5 file (num + start)
  with _ -> start

let res = adventofcode1'5 (open_in "input1.txt") 0
let () = print_endline (string_of_int res)
