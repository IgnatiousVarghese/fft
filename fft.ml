let pi = 4. *. atan 1.

module Complex = struct
  type t = {
    re : float;
    im : float;
  }
  let re t = t.re
  let im t = t.im
  
  let zero = { re = 0.; im = 0. }
  let one = { re = 1.; im = 0. }
  let real re = { re; im = 0. }

  let add { re = re1; im = im1; } { re = re2; im = im2; } =
    { re = re1 +. re2;
      im = im1 +. im2;
    }

  let sub { re = re1; im = im1; } { re = re2; im = im2; } =
    { re = re1 -. re2;
      im = im1 -. im2;
    }

  let mult { re = re1; im = im1; } { re = re2; im = im2; } =
    { re = re1 *. re2 -. im1 *. im2;
      im = im1 *. re2 +. im2 *. re1;
    }

  let exp_im theta =
    { re = cos theta; im = sin theta; }
end

(* Only works if len(cs) is a power of 2. *)
let rec fft cs sign =
  let n = Array.length cs in
  if n = 1 then cs
  else
    let cs1 = Array.init (n/2) (fun i -> cs.(2*i)) in
    let cs2 = Array.init (n/2) (fun i -> cs.(2*i+1)) in
    let es = fft cs1 sign in
    let ds = fft cs2 sign in
    let ww = Complex.exp_im(float sign *. 2.0 *. pi /. float n) in
    let res = Array.create n Complex.zero in
    let rec loop idx w =
      if idx = n/2 then ()
      else begin
        let es = es.(idx) in
        let ds = Complex.mult w ds.(idx) in
        res.(idx) <- Complex.add es ds;
        res.(idx + n/2) <- Complex.sub es ds;
        loop (idx + 1) (Complex.mult w ww)
      end
    in
    loop 0 Complex.one;
    res

let pow2 n =
  let rec loop acc =
    if acc < n then loop (2*acc)
    else acc
  in
  loop 1

let rec remove_leading_zeros = function
  | [ 0 ] -> [ 0 ]
  | 0 :: q -> remove_leading_zeros q
  | l -> l

let mult a b =
  let a = Array.of_list (List.rev a) in
  let b = Array.of_list (List.rev b) in
  let max_len = max (Array.length a) (Array.length b) in
  let n = 2 * pow2 max_len in
  let a =
    Array.init n (fun i -> if i < Array.length a then Complex.real (float a.(i)) else Complex.zero)
  in
  let b =
    Array.init n (fun i -> if i < Array.length b then Complex.real (float b.(i)) else Complex.zero)
  in
  let a_fft = fft a 1 in
  let b_fft = fft b 1 in
  let c_fft = Array.init n (fun i -> Complex.mult a_fft.(i) b_fft.(i)) in
  let c = fft c_fft (-1) in
  let c = Array.map (fun x -> int_of_float (0.5 +. Complex.re x /. float n)) c in
  let res, carry =
    Array.fold_left
      (fun (acc, carry) c ->
        let tmp = carry + c in
        tmp mod 10 :: acc, tmp / 10)
      ([], 0)
      c
  in
  remove_leading_zeros (if carry <> 0 then carry :: res else res)

let () =
  let x = mult [ 1; 2; 3; 4; 5 ] [ 6; 7; 8; 9; 0; 1 ] in
  List.iter (fun x -> Printf.printf "%d" x) x;
  Printf.printf "\n"