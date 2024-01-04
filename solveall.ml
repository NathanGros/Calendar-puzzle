open Raylib
open Place;;

(*typedef*)
type piece = Place.piece
	(*|N |I |O |E |X |V |P |D |L |Z |S |T*)
;;

type piece_state = (*piece et rotation*)
	piece * int
;;

type board_state =
	piece_state list
;;

let foi n =
	float_of_int n
;;

let iof n =
	int_of_float n
;;

(*global values*)
init_window 0 0 "Calendar";;
let w = get_screen_width ();;
let h = get_screen_height ();;
let valid_l = ref [];;
let sol_nb = ref 1;;
let cursor_x = ref 100;;
let button_pressed = ref false;;
let not_gen = ref true;;
let pannel_x = iof (foi w /. 3.5);;

(*MODIFY THIS*)
let today_l = [(2, 2); (3, 4); (4, 0)];;




(*raylib functions*)

let draw_board l =
	(*draw_board*)
	let center_x = foi (w/2 + pannel_x/2) in
	let center_y = foi (h/2 + 15) in
	draw_poly (Vector2.create center_x center_y) 6 ((foi h) /. 2.) 30. (Color.create 184 186 187 255);
	(*make board matrix from l*)
	let board_matrix = Array.init 9 (fun i -> if i<4 then Array.make (5+i) N else Array.make 9 N) in
	for i = 0 to List.length l - 1 do
		let _ = Place.put_piece board_matrix today_l (List.nth l ((List.length l) - i - 1)) in ()
	done;
	(*draw tiles*)
	let ext_rad = (foi h /. 19.) in
	let int_rad = sqrt (3. /. 4. *. ext_rad *. ext_rad) in
	for i = 0 to Array.length board_matrix - 1 do
		for j = 0 to Array.length board_matrix.(i) - 1 do
			if board_matrix.(i).(j) <> N then begin
				let small_center_y = (center_y -. 6. *. ext_rad) +. 3. /. 2. *. ext_rad *. (foi i) in
				let small_center_x =
					match i mod 2 with
					|0 -> (center_x -. 4. *. int_rad) +. 2. *. int_rad *. (foi (j - i/2))
					|_ -> (center_x -. 4. *. int_rad) +. 2. *. int_rad *. (foi (j - i/2)) -. int_rad
				in
				let col =
					match board_matrix.(i).(j) with
					|D -> Color.create 35 195 255 255
					|E -> Color.create 134 255 140 255
					|I -> Color.create 231 255 96 255
					|L -> Color.create 197 107 255 255
					|O -> Color.create 255 180 90 255
					|P -> Color.create 181 115 66 255
					|S -> Color.create 87 142 239 255
					|T -> Color.create 255 81 81 255
					|V -> Color.create 255 103 241 255
					|X -> Color.create 119 253 255 255
					|Z -> Color.create 67 171 109 255
					|_ -> Color.create 184 186 187 255
				in
				draw_poly (Vector2.create small_center_x small_center_y) 6 ext_rad 0. col
			end
		done
	done;
	begin_drawing ();
	end_drawing ();
;;


let draw_cursor () =
	draw_rectangle 80 (h/2 - 20) (pannel_x - 160) 40 (Color.create 212 215 216 255);
	let len = List.length !valid_l in
	match len with
	|0 -> draw_rectangle 87 (h/2 - 13) 26 26 (Color.create 255 255 255 255)
	|_ ->	draw_rectangle (87 + (pannel_x - 200) * !sol_nb / len) (h/2 - 13) 26 26 (Color.create 255 255 255 255)
;;


let draw_nb () =
	draw_rectangle 100 (h/2 - 95) (pannel_x - 160 - 192) 56 (Color.create 212 215 216 255);
	draw_text (string_of_int !sol_nb) 110 (h/2 - 90) 50 (Color.create 255 255 255 255);
;;


let draw_buttons () =
	let button_top_y = h/2 - 95 in
	let minus_button_x = (pannel_x - 160 - 192) + 120 in
	draw_rectangle minus_button_x button_top_y 56 56 (Color.create 212 215 216 255);
	draw_rectangle (minus_button_x + 10) (button_top_y + 24) 36 8 (Color.create 255 255 255 255);
	let plus_button_x = (pannel_x - 160 - 192) + 120 + 76 in
	draw_rectangle plus_button_x button_top_y 56 56 (Color.create 212 215 216 255);
	draw_rectangle (plus_button_x + 10) (button_top_y + 24) 36 8 (Color.create 255 255 255 255);
	draw_rectangle (plus_button_x + 24) (button_top_y + 10) 8 36 (Color.create 255 255 255 255);
;;


let draw_all l =
	draw_cursor ();
	draw_nb ();
	draw_buttons ();
	draw_board l
;;


let run_window () =
	let x = get_mouse_x () in
	let y = get_mouse_y () in
	let button_minus = Rectangle.create (foi (pannel_x - 160 - 192 + 120)) (foi (h/2 - 95)) 56. 56. in
	let button_plus = Rectangle.create (foi (pannel_x - 160 - 192 + 120 + 76)) (foi (h/2 - 95)) 56. 56. in
	if not (is_mouse_button_down Left) then button_pressed := false;
	if is_mouse_button_pressed Left then begin
		if check_collision_point_rec (Vector2.create (foi x) (foi y)) button_minus then (button_pressed := true; sol_nb := !sol_nb - 1);
		if check_collision_point_rec (Vector2.create (foi x) (foi y)) button_plus then (button_pressed := true; sol_nb := !sol_nb + 1);
		if !sol_nb < 1 then sol_nb := 1;
		if !sol_nb > List.length !valid_l then sol_nb := List.length !valid_l;
		clear_background (Color.create 212 215 216 255);
		draw_rectangle 0 0 pannel_x h (Color.create 184 186 187 255);
		draw_buttons ();
		draw_nb ();
		draw_cursor ();
		draw_board (List.nth !valid_l (!sol_nb - 1))
	end
	else if is_mouse_button_down Left && not !button_pressed then begin
		(*cursor values*)
		if x < 100 then
			(sol_nb := 1; cursor_x := 100)
		;
		if x > 100 && x < (pannel_x - 100) then
			(sol_nb := 1 + iof (foi (List.length !valid_l * (x - 100)) /. foi (pannel_x - 200)); cursor_x := x)
		;
		if x > (pannel_x - 100) then
			(sol_nb := List.length !valid_l; cursor_x := (pannel_x - 100))
		;
		clear_background (Color.create 212 215 216 255);
		draw_rectangle 0 0 pannel_x h (Color.create 184 186 187 255);
		draw_buttons ();
		(*cursor drawing*)
		draw_rectangle 80 (h/2 - 20) (pannel_x - 160) 40 (Color.create 212 215 216 255);
		draw_rectangle (!cursor_x - 13) (h/2 - 13) 26 26 (Color.create 255 255 255 255);

		draw_nb ();
		draw_board (List.nth !valid_l (!sol_nb - 1))
	end
	;
	begin_drawing ();
	end_drawing ()
;;






(*backtracking functions*)
let string_of_piece p =
	match p with
	|N -> " " |I -> "I" |O -> "O" |E -> "E" |X -> "X" |V -> "V" |P -> "P" |D -> "D" |L -> "L" |Z -> "Z" |S -> "S" |T -> "T"
;;


let remove_piece board p =
	for i = 0 to Array.length board - 1 do
		for j = 0 to Array.length board.(i) - 1 do
			if board.(i).(j) = p then board.(i).(j) <- N
		done
	done
;;


let not_in_list l =
	Array.of_list (List.filter (fun i -> not (List.mem i (fst (List.split l)) )) [D;E;I;L;O;P;S;T;V;X;Z])
;;


let nb_orient p =
	match p with
	|I -> 3 |O -> 2 |E -> 6 |X -> 3 |V -> 6 |P -> 12 |D -> 6 |L -> 12 |Z -> 12 |S -> 6 |T -> 6 |_ -> failwith "error"
;;


let int_of_piece p =
	match p with
	|D -> 0 |E -> 1 |I -> 2 |L -> 3 |O -> 4 |P -> 5 |S -> 6 |T -> 7 |V -> 8 |X -> 9 |Z -> 10 |_ -> failwith "error"
;;


let rec backtrack board (used_l : board_state) =
	let avail = not_in_list used_l in
	if avail = [||] then	begin
		valid_l := used_l::!valid_l;
		sol_nb := !sol_nb + 1;
		clear_background (Color.create 212 215 216 255);
		draw_rectangle 0 0 pannel_x h (Color.create 184 186 187 255);
		(*draw cursor*)
		let int_piece = int_of_piece (fst (List.nth used_l (List.length used_l - 1))) in
		let rot_piece = snd (List.nth used_l (List.length used_l - 1)) in
		let total_piece_frac = ref rot_piece in
		let t = [|D;E;I;L;O;P;S;T;V;X;Z|] in
		for i = 0 to 10 do
			if i < int_piece then total_piece_frac := !total_piece_frac + (nb_orient t.(i))
		done;
		let cursor_len = iof (foi (pannel_x - 160 - 54) /. 74. *. (foi !total_piece_frac)) in
		draw_rectangle 80 (h/2 - 20) (pannel_x - 160) 40 (Color.create 212 215 216 255);
		draw_rectangle 87 (h/2 - 13) 26 26 (Color.create 255 255 255 255);
		draw_rectangle 113 (h/2 - 13) cursor_len 26 (Color.create 255 255 255 255);

		draw_nb ();
		draw_buttons ();
		draw_board used_l
	end;
	for i = 0 to Array.length avail - 1 do
		let n = nb_orient avail.(i) in
		for j = 0 to n - 1 do
			let p = (avail.(i), j) in
			let valid_piece = ref true in
			if not (Place.put_piece board today_l p) then
			 (valid_piece := false; remove_piece board (fst p))
			;
			if !valid_piece then	backtrack board (p::used_l);
			remove_piece board (fst p)
		done
	done
;;


let rec loop () =
	match window_should_close () with
	| true ->
		print_string "\x1B[H";
		close_window ()
	| false ->
		if !not_gen then begin
			not_gen := false;
			let board = Array.init 9 (fun i -> if i<4 then Array.make (5+i) N else Array.make 9 N) in
			backtrack board [];
			sol_nb := List.length !valid_l;
			draw_nb ();
			draw_cursor ();
			draw_board (List.nth !valid_l (List.length !valid_l - 1))
		end;
		run_window ();
		loop ()
;;


let _ =
	print_string "\x1B[2J";

	(*background*)
	clear_background (Color.create 212 215 216 255);
	draw_rectangle 0 0 pannel_x h (Color.create 184 186 187 255);

	draw_all [];

	set_target_fps 60;
	loop ()
;;
