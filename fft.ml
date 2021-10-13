let print_array arr=
	let n = Array.length arr in
	for i = 0 to n-1 do 
		Printf.printf "%f x^%d " arr.(i) i;
	done;
	Printf.printf "\n";
	;;


let () =

	Printf.printf "Enter degree of 1st polynomial\n";
	let n1 = read_int() in

	Printf.printf "enter coeffcients in increasing order of degree\n";
	let a = Array.make (n1+1) 0. in 
	for i = 0 to n1 do
		let x = read_int () in
		a.(i) <- Float.of_int x;
	done;

	Printf.printf "\nEnter degree of 2st polynomial\n";
	let n2 = read_int() in

	Printf.printf "enter coeffcients in increasing order of degree\n";
	let b = Array.make (n2+1) 0. in 
	for i = 0 to n2 do
		let x = read_int () in
		b.(i) <- Float.of_int x;
	done;

	print_array a;
	print_array b;
	
	