open Free

module Fiber = struct
  type 'a t =
    | Spawn of ((unit -> unit) * (unit -> 'a t))
    | Recv of (string -> 'a t)
    | Set of (int * (int -> 'a t))
    | Pure of 'a

  let rec bind m f = match m with
    | Spawn (a, k) -> Spawn (a, fun b -> bind (k b) f)
    | Recv k -> Recv (fun a -> bind (k a) f)
    | Set (i, k) -> Set (i, fun a -> bind (k a) f)
    | Pure a -> f a

  let (let*) m k = bind m k
end

open Fiber

let run main = 
  let q = Queue.create () in
  let rec loop f = match f with
    | Spawn (f, k) -> 
        Queue.push (fun () -> f ()) q;
        loop (k ())
    | Set (i, k) -> loop (k i)
    | Recv k -> loop (k "hello")
        (* Queue.push (fun () -> loop (k "hello")) q; *)
    | Pure a -> ()
  in 
  Queue.push (fun () -> loop main) q;
  let rec scheduler () =
    if not (Queue.is_empty q) then begin
      Queue.pop q ();
      scheduler ()
    end
  in
  scheduler ()

let main () = 
  let* _ = Spawn (fun () -> Printf.printf "spawn\n%!"; , fun x -> Pure x) in
  let* i = (Set (4, fun x -> Pure x)) in
  let* rx = Recv (fun x -> Pure x) in
  let* j =  Set (1, fun x -> Printf.printf "%d\n%!" i; Pure x) in Printf.printf "%d\n%!" j; Printf.printf "%s\n%!" rx; Pure 0

let _ = run @@ main ()