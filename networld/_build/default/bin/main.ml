open Free

module Fiber = struct
	type 'p promise = int
	type ('a, 'p) t =
    | Recv of ('p promise -> ('a, 'p) t)
    | Await of ('p promise) * ('p -> ('a, 'p) t)
    | Pure of 'a
  
  let bind m f = match m with
    | Recv k -> Recv (fun p -> f p)
    | Await (p, k) -> Await (p, f)
    | Pure a -> f a
      (* | Await  *)
  let (let*) m k = bind m k
end

open Fiber

let run main =
  let kqueue = ref [] in 
  let rec loop f = match f with
    | Recv k -> 
        loop (k 10)
    | Await (p, k) -> 
        kqueue := !kqueue @ [fun a -> loop (k 5)]; 999
    | Pure a -> a
  in
  kqueue := !kqueue @ [fun a -> loop main];
  let rec scheduler () = match !kqueue with
    | [] -> ()
    | tasks -> 
        let task = List.hd tasks in
        kqueue := List.tl !kqueue;
        let _ = task () in scheduler ()
  in
  scheduler ()

let main () = 
  let* p = Recv (fun x -> Pure x) in
  let* r = Await (p, fun x -> Pure x) in
  Pure (Printf.printf "%d\n" r; 0)
  (* bind (Recv (fun x -> Pure x)) (fun p -> Await (p, fun x -> Pure (Printf.printf "await %d\n" x; x))) *)

let _ = run @@ main ()