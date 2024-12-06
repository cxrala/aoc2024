let parse filename =
    let read_lines name =
        let ic = open_in name in
        let try_read () =
            try Some (input_line ic) with End_of_file -> None in
                let rec loop acc = match try_read () with
                   | Some s -> loop (s :: acc)
                   | None -> close_in ic; List.rev acc in
        loop [] in
    List.map (fun s -> List.map int_of_string  @@ String.split_on_char ' ' s) @@ read_lines filename

let bounded x y n =
    let z = x - y in
    let diff = if z > 0 then z else -z in
    diff >= 1 && diff <= 3

    
let () =
    let input_list = parse "input" in

    (*i know the function name is cursed but i am not at work so you cannot criticise me*)
    let strictly_monotonically_bounded list =
        let rec smb is_increasing = function
            | [] | [_] -> true
            | x::y::xs when x = y -> false
            | x::y::xs when (x < y = is_increasing) && bounded x y 3 -> smb is_increasing (y::xs)
            | _ -> false
        in match list with
            | [] | [_] -> true
            | x::y::_ -> smb (x < y) list in
    let count_smbs = List.map strictly_monotonically_bounded input_list in
    let map_bool_int = function
        | false -> 0
        | true -> 1
    in Printf.printf "Part 1: %d" (List.fold_left (fun acc x -> acc + map_bool_int x) 0 count_smbs)

