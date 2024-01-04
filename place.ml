type piece =
	|N |I |O |E |X |V |P |D |L |Z |S |T
;;


let first_spot board l =
	let flag = ref false in
	let x = ref 0 in
	let y = ref 0 in
	for i = 0 to Array.length board - 1 do
		for j = 0 to Array.length board.(i) - 1 do
			if !flag = false && board.(i).(j) = N && (not (List.mem (i, j) l)) && ((i < 5) || (j >= (i - 4))) then begin
				x := i;
				y := j;
				flag := true
			end
		done
	done;
	!x, !y
;;


let put_piece board today_l p :bool =
	let aux board x y piece l tab :bool =
		let flag = ref true in
		Array.iter 
			(fun (x2, y2) ->
				try 
					(if board.(x+x2).(y+y2) = N &&
					((x+x2 < 5) || (y+y2 >= (x+x2 - 4))) &&
					(not (List.mem (x+x2, y+y2) l))
					then board.(x+x2).(y+y2) <- piece else flag := false)
				with Invalid_argument "index out of bounds" -> flag := false)
			tab;
		!flag
	in
	let x, y = first_spot board today_l in
	match p with
	|I, 0 -> aux board x y (fst p) today_l [|(0,0); (1,0); (2,0); (3,0); (4,0)|]
	|I, 1 -> aux board x y (fst p) today_l [|(0,0); (1,1); (2,2); (3,3); (4,4)|]
	|I, 2 -> aux board x y (fst p) today_l [|(0,0); (0,1); (0,2); (0,3); (0,4)|]

	|O, 0 -> aux board x y (fst p) today_l [|(0,0); (1,0); (1,1); (2,0); (2,1); (2,2)|]
	|O, 1 -> aux board x y (fst p) today_l [|(0,0); (0,1); (0,2); (1,1); (1,2); (2,2)|]

	|E, 0 -> aux board x y (fst p) today_l [|(0,0); (0,1); (1,0); (1,1); (2,0); (2,1)|]
	|E, 1 -> aux board x y (fst p) today_l [|(0,0); (1,0); (1,1); (2,1); (2,2); (3,2)|]
	|E, 2 -> aux board x y (fst p) today_l [|(0,0); (0,1); (0,2); (1,1); (1,2); (1,3)|]
	|E, 3 -> aux board x y (fst p) today_l [|(0,0); (1,0); (1,1); (2,0); (2,1); (3,1)|]
	|E, 4 -> aux board x y (fst p) today_l [|(0,0); (0,1); (1,1); (1,2); (2,2); (2,3)|]
	|E, 5 -> aux board x y (fst p) today_l [|(0,0); (0,1); (0,2); (1,0); (1,1); (1,2)|]

	|X, 0 -> aux board x y (fst p) today_l [|(0,0); (0,1); (1,1); (2,1); (2,2)|]
	|X, 1 -> aux board x y (fst p) today_l [|(0,0); (1,0); (1,1); (1,2); (2,2)|]
	|X, 2 -> aux board x y (fst p) today_l [|(0,0); (1,-1); (1,0); (1,1); (2,0)|]

	|V, 0 -> aux board x y (fst p) today_l [|(0,0); (0,1); (0,2); (1,0); (2,0)|]
	|V, 1 -> aux board x y (fst p) today_l [|(0,0); (1,0); (2,0); (3,1); (4,2)|]
	|V, 2 -> aux board x y (fst p) today_l [|(0,0); (1,1); (2,2); (2,3); (2,4)|]
	|V, 3 -> aux board x y (fst p) today_l [|(0,0); (1,0); (2,-2); (2,-1); (2,0)|]
	|V, 4 -> aux board x y (fst p) today_l [|(0,0); (1,1); (2,2); (3,2); (4,2)|]
	|V, 5 -> aux board x y (fst p) today_l [|(0,0); (0,1); (0,2); (1,3); (2,4)|]

	|P, 0 -> aux board x y (fst p) today_l [|(0,0); (0,1); (1,0); (1,1); (2,0)|]
	|P, 1 -> aux board x y (fst p) today_l [|(0,0); (1,0); (1,1); (2,1); (3,2)|]
	|P, 2 -> aux board x y (fst p) today_l [|(0,0); (0,1); (1,1); (1,2); (1,3)|]
	|P, 3 -> aux board x y (fst p) today_l [|(0,0); (1,-1); (1,0); (2,-1); (2,0)|]
	|P, 4 -> aux board x y (fst p) today_l [|(0,0); (1,1); (2,1); (2,2); (3,2)|]
	|P, 5 -> aux board x y (fst p) today_l [|(0,0); (0,1); (0,2); (1,2); (1,3)|]
	|P, 6 -> aux board x y (fst p) today_l [|(0,0); (1,0); (1,1); (2,1); (3,1)|]
	|P, 7 -> aux board x y (fst p) today_l [|(0,0); (0,1); (1,1); (1,2); (2,3)|]
	|P, 8 -> aux board x y (fst p) today_l [|(0,0); (0,1); (0,2); (1,0); (1,1)|]
	|P, 9 -> aux board x y (fst p) today_l [|(0,0); (1,0); (2,0); (2,1); (3,1)|]
	|P, 10 -> aux board x y (fst p) today_l [|(0,0); (1,1); (1,2); (2,2); (2,3)|]
	|P, 11 -> aux board x y (fst p) today_l [|(0,0); (0,1); (1,-1); (1,0); (1,1)|]

	|D, 0 -> aux board x y (fst p) today_l [|(0,0); (1,0); (1,1); (2,0); (2,1)|]
	|D, 1 -> aux board x y (fst p) today_l [|(0,0); (0,1); (1,1); (1,2); (2,2)|]
	|D, 2 -> aux board x y (fst p) today_l [|(0,0); (0,1); (1,0); (1,1); (1,2)|]
	|D, 3 -> aux board x y (fst p) today_l [|(0,0); (0,1); (1,0); (1,1); (2,1)|]
	|D, 4 -> aux board x y (fst p) today_l [|(0,0); (1,0); (1,1); (2,1); (2,2)|]
	|D, 5 -> aux board x y (fst p) today_l [|(0,0); (0,1); (0,2); (1,1); (1,2)|]

	|L, 0 -> aux board x y (fst p) today_l [|(0,0); (0,1); (1,0); (2,0); (3,0)|]
	|L, 1 -> aux board x y (fst p) today_l [|(0,0); (1,0); (2,1); (3,2); (4,3)|]
	|L, 2 -> aux board x y (fst p) today_l [|(0,0); (1,1); (1,2); (1,3); (1,4)|]
	|L, 3 -> aux board x y (fst p) today_l [|(0,0); (1,0); (2,0); (3,-1); (3,0)|]
	|L, 4 -> aux board x y (fst p) today_l [|(0,0); (1,1); (2,2); (3,3); (4,3)|]
	|L, 5 -> aux board x y (fst p) today_l [|(0,0); (0,1); (0,2); (0,3); (1,4)|]
	|L, 6 -> aux board x y (fst p) today_l [|(0,0); (1,1); (2,1); (3,1); (4,1)|]
	|L, 7 -> aux board x y (fst p) today_l [|(0,0); (0,1); (1,2); (2,3); (3,4)|]
	|L, 8 -> aux board x y (fst p) today_l [|(0,0); (0,1); (0,2); (0,3); (1,0)|]
	|L, 9 -> aux board x y (fst p) today_l [|(0,0); (1,0); (2,0); (3,0); (4,1)|]
	|L, 10 -> aux board x y (fst p) today_l [|(0,0); (1,1); (2,2); (3,3); (3,4)|]
	|L, 11 -> aux board x y (fst p) today_l [|(0,0); (1,-3); (1,-2); (1,-1); (1,0)|]

	|Z, 0 -> aux board x y (fst p) today_l [|(0,0); (1,0); (2,1); (3,1); (4,1)|]
	|Z, 1 -> aux board x y (fst p) today_l [|(0,0); (1,1); (1,2); (2,3); (3,4)|]
	|Z, 2 -> aux board x y (fst p) today_l [|(0,0); (0,1); (0,2); (1,-1); (1,0)|]
	|Z, 3 -> aux board x y (fst p) today_l [|(0,0); (1,0); (2,0); (3,1); (4,1)|]
	|Z, 4 -> aux board x y (fst p) today_l [|(0,0); (1,1); (2,2); (2,3); (3,4)|]
	|Z, 5 -> aux board x y (fst p) today_l [|(0,0); (0,1); (1,-2); (1,-1); (1,0)|]
	|Z, 6 -> aux board x y (fst p) today_l [|(0,0); (1,-1); (1,0); (2,-1); (3,-1)|]
	|Z, 7 -> aux board x y (fst p) today_l [|(0,0); (1,1); (2,1); (3,2); (4,3)|]
	|Z, 8 -> aux board x y (fst p) today_l [|(0,0); (0,1); (1,2); (1,3); (1,4)|]
	|Z, 9 -> aux board x y (fst p) today_l [|(0,0); (1,0); (2,-1); (2,0); (3,-1)|]
	|Z, 10 -> aux board x y (fst p) today_l [|(0,0); (1,1); (2,2); (3,2); (4,3)|]
	|Z, 11 -> aux board x y (fst p) today_l [|(0,0); (0,1); (0,2); (1,3); (1,4)|]

	|S, 0 -> aux board x y (fst p) today_l [|(0,0); (1,0); (2,1); (3,2); (4,2)|]
	|S, 1 -> aux board x y (fst p) today_l [|(0,0); (1,1); (1,2); (1,3); (2,4)|]
	|S, 2 -> aux board x y (fst p) today_l [|(0,0); (0,1); (1,0); (2,-1); (2,0)|]
	|S, 3 -> aux board x y (fst p) today_l [|(0,0); (1,1); (2,1); (3,1); (4,2)|]
	|S, 4 -> aux board x y (fst p) today_l [|(0,0); (0,1); (1,2); (2,3); (2,4)|]
	|S, 5 -> aux board x y (fst p) today_l [|(0,0); (1,-2); (1,-1); (1,0); (2,-2)|]

	|T, 0 -> aux board x y (fst p) today_l [|(0,0); (1,-1); (1,0); (2,-1); (2,0); (3,-1)|]
	|T, 1 -> aux board x y (fst p) today_l [|(0,0); (1,1); (2,1); (2,2); (3,2); (4,3)|]
	|T, 2 -> aux board x y (fst p) today_l [|(0,0); (0,1); (0,2); (1,2); (1,3); (1,4)|]
	|T, 3 -> aux board x y (fst p) today_l [|(0,0); (1,0); (2,0); (2,1); (3,1); (4,1)|]
	|T, 4 -> aux board x y (fst p) today_l [|(0,0); (1,1); (1,2); (2,2); (2,3); (3,4)|]
	|T, 5 -> aux board x y (fst p) today_l [|(0,0); (0,1); (0,2); (1,-1); (1,0); (1,1)|]

	|_, _ -> aux board x y (fst p) today_l [|(0,0); (1,0); (1,-1)|]
;;
