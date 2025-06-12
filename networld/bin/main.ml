open Free

module Fiber = struct
	type 'a t =
    (* | Spawn of ((unit -> unit) * ('a, 'p) t) *)
    | Recv of (int list -> 'a t)
    | Set of (int * (int -> 'a t))
    | Pure of 'a
  
  (* let bind m f = match m with
    (* | Spawn (t, _) -> Spawn (t, f) *)
    | Recv k -> Recv (fun p -> f p)
    | Set (i, _) -> Set (i, f)
    | Pure a -> f a *)
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