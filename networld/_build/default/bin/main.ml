

module Fiber = struct
    type fiber =
        | Step of {
            action : unit -> int;
            cont : int -> fiber;
        }
        | End of unit
    [@@deriving show]

    (* k: int -> fiber *)
    let (>>=) (Step fiber) k = Step { action = fiber.action; cont = fun a -> k a }
    (* let (let) (f, c) k = Step (f, c (fun a -> k a)) *)
    let step f = Step { action = f; cont = fun _ -> End () }
end

let const () = 1

(* 
    let* p = async () in
    let* a = await p in
    return a
*)

(* 
    async () >>= \p ->
    await p >>= \a ->
    return a
*)

(* 
    (Step f (\p -> return p : fiber))
    Step : (unit -> 'a) * ((a' -> fiber) -> fiber)
    (Async (\p -> (Await (\a -> return a)))
    Await <- fiber : ('a -> fiber)
*)

open Fiber

let () = 
    let m = (Fiber.step (fun () -> 1)) >>= (fun a -> Fiber.step (fun () -> 1) )
        (* let* _ = Fiber.step (fun () -> 1) in (End ()) *)
        in ()
(* 
bind : m a -> (a -> m b) -> m b
*)
(* 
set : a -> state a
get : unit -> state a
*)
(*  set >>= \s ->
    get () >>= \a ->
    Printf.print a
*)