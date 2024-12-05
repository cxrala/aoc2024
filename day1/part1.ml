let read_lines name : string list =
    let ic = open_in name in
    let try_read () =
        try Some (input_line ic) with End_of_file -> None in
    let rec loop acc = match try_read () with
        | Some s -> loop (s :: acc)
        | None -> close_in ic; List.rev acc in
    loop []

let abs_diff x y =
    let z = x - y in
    if z > 0 then z else -z 

let sort_file filename = List.sort compare @@ List.map int_of_string @@ read_lines filename

let () =
    let sorted_left = sort_file "left.txt" in
    let sorted_right = sort_file "right.txt" in
    let rec solve1 list acc = match list with
        | [] -> acc
        | (a, b) :: t -> solve1 t (acc + abs_diff a b) in
    Printf.printf "Part 1: %d" @@ solve1 (List.combine sorted_left sorted_right) 0;
    let runlength = 
        let rec consec list acc = match list with 
            | [] -> (acc, [])
            | [x] -> (acc + 1, [])
            | x1::x2::xs when x1 = x2 -> consec (x1::xs) (acc + 1)
            | x::y::xs -> (acc + 1, y::xs) in
        let rec runlength = function 
            | [] -> []
            | x::_ as xs ->
                let (count, rem) = consec xs 0 in
                (x, count) :: runlength rem in
        runlength in
    let solve2 left rl_list =
        let rec solve2 left rl_list acc = match left, rl_list with
            | [], _ | _, [] -> acc
            | x1::xs, (x2, n)::ys when x1 = x2 -> solve2 xs ys (acc + x1 * n)
            | x::xs, ((y, _)::_ as ys) when x < y -> solve2 xs ys acc
            | xs, y::ys -> solve2 xs ys acc 
        in solve2 left rl_list 0 in
    Printf.printf "Part 2: %d" @@ solve2 sorted_left (runlength sorted_right)
