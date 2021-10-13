open Complex
let print_array arr=
	let n = Array.length arr in
	for i = 0 to n-1 do 
		Printf.printf "%d x^%d + " (int_of_float arr.(i)) i;
	done;
	Printf.printf "\n";
	;;
	
let print p1 = 
	let n = Array.length p1 in
	for i = 0 to n-1 do
        Printf.printf "%f + %f  i\n" p1.(i).re p1.(i).im
    done;
	Printf.printf "\n"
	;;

let print_real p1 = 
	let n = Array.length p1 in
	for i = 0 to n-1 do
        Printf.printf "%.0f x^%d + " p1.(i).re i;
    done;
	Printf.printf "\n"
	;;

let print_co p1 = 
	let n = Array.length p1 in
	for i = 0 to n-1 do
        Printf.printf " %.0f ," p1.(i).re;
    done;
	Printf.printf "]\n"
	;;


let rec fft p sign =
	let n = Array.length p in

	if n = 1 then 
		begin
			p			
		end
	else 
	begin
		
		let n2 = n/2 in

		let pe = Array.make n2 {re = 0.0; im = 0.0} in
		let po = Array.make n2 {re = 0.0; im = 0.0} in

		for i = 0 to n-1 do
			if i mod 2 = 0 then 
				pe.(i/2) <- p.(i)
			else 
				po.(i/2) <- p.(i)
		done;

		let ye = fft pe sign in
		let yo = fft po sign in

		let ang = 2.0 *. 3.14159265359 /. float n *. float (if sign then 1 else -1) in

		let w = ref {re = 1.0; im = 0.0} in
		let wn = { re = cos ang ; im = sin ang } in

		let i = ref 0 in

		while !i * 2 < n do
			
			p.(!i) <- add ye.(!i) (mul !w  yo.(!i));
			p.(!i+ n/2) <- sub ye.(!i) (mul !w  yo.(!i));

			if not sign then	
				begin
					p.(!i) <- div p.(!i) {re = 2.0; im = 0.0};
					p.(!i+ n/2) <- div p.(!i + n/2)  {re= 2.0; im = 0.0};
				end
			else
				Printf.printf "";

			w := mul !w  wn;
			i := !i + 1;
			
		done;
		p
	end
	;;

let mult x y = 
	let n = Array.length x in

	let vx = fft x true in
	let vy = fft y true in

	for i = 0 to n-1 do
		vx.(i) <- mul vx.(i) vy.(i);
	done;

	let pro = fft vx false in 

	pro
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

	Printf.printf "1st polynomial\n";
	print_array a;
	Printf.printf "\n2st polynomial\n";
	print_array b;

	let k = Array.length a + Array.length b in
	let n = ref 1 in

	while !n < k do
		n := !n*2
	done;

	let x = Array.make !n {re = 0.0; im = 0.0} in
	let y = Array.make !n {re = 0.0; im = 0.0} in

	for i = 0 to (Array.length a) - 1 do
		x.(i) <- {re = a.(i); im = 0.0}
	done;

	for i = 0 to (Array.length b) - 1 do
		y.(i) <- {re = b.(i); im = 0.0}
	done;	

	let res = mult x y in 
	Printf.printf "\nProduct after multiplication using FFT is =>\n";
	print_real res;
	Printf.printf "\ncoeefcient representation\n[";
	print_co res;
	Printf.printf "\n";
	;;