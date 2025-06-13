(* open Fiber
open FiberOps
open Ops
(* open FiberOps *)
open Ops *)

(* FunctorのシグネチャとFreeモナドの定義 *)
module type Functor = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module Free (F : Functor) = struct
  type 'a free =
    | Free of 'a free F.t
    | Pure of 'a

  let rec bind m f = match m with
    | Free fm -> Free (F.map (fun x -> bind x f) fm)
    | Pure a -> f a

  let (let*) = bind
  let return a = Pure a
  let identity x = x
  let liftF command = Free (F.map return command)
end

(* FiberOps の定義 *)
module rec FiberOps : sig
  type 'a t =
    | Spawn of (unit -> 'a)
    | Yield of (unit -> 'a)
    | Set of (int * (unit -> 'a))
    | Get of (int -> 'a)
  val map : ('a -> 'b) -> 'a t -> 'b t
end = struct
  type 'a t =
    | Spawn of (unit -> 'a)
    | Yield of (unit -> 'a)
    | Set of (int * (unit -> 'a))
    | Get of (int -> 'a)

  let map f = function
    | Spawn task -> Spawn (fun () -> f (task ()))
    | Yield k -> Yield (fun () -> f (k ()))
    | Set (i, k) -> Set (i, fun () -> f (k ()))
    | Get k -> Get (fun i -> f (k i))
end

(* Fiberモジュール *)
module Fiber = Free(FiberOps)

(* Opsモジュール：Fiber操作 *)
module Ops = struct
  let spawn task = Fiber.liftF (FiberOps.Spawn task)
  let spawnf : (unit -> 'a Fiber.free) -> 'a Fiber.free = fun task -> task ()
  let yield () = Fiber.liftF (FiberOps.Yield (fun () -> ()))
end

(* Scheduler *)
module Scheduler = struct
  let run main =
    let q = Queue.create () in

    let enqueue f = Queue.push f q in

    let state = ref 0 in
    let rec loop fiber = 
      match fiber with
        | Fiber.Pure _ -> ()
        | Fiber.Free (FiberOps.Spawn task) ->
            enqueue (fun () -> loop (task ()))
        | Fiber.Free (FiberOps.Yield k) ->
            enqueue (fun () -> loop (k ()))
        | Fiber.Free (FiberOps.Set (i, k)) ->
            Printf.printf "set %d\n" i;
            state := i;
            Printf.printf "seed %d\n" !state;
            enqueue (fun () -> loop (k ()))
        | Fiber.Free (FiberOps.Get k) ->
            Printf.printf "get %d\n" !state;
            enqueue (fun () -> loop (k !state))
    in

    enqueue (fun () -> loop main);

    let rec schedule () =
      if not (Queue.is_empty q) then (
        let task = Queue.pop q in
        task ();
        schedule ()
      )
    in
    schedule ()
end

open Scheduler
module Meta = struct
  open Effect
  open Effect.Deep

  type uid = int

  type _ Effect.t +=
    | Connect : uid * uid -> (unit Fiber.free) Effect.t
    | Send : uid * string -> (unit Fiber.free) Effect.t

  let connect src dst = perform (Connect (src, dst))
  let send dst msg = perform (Send (dst, msg))

  let run prog = 
    try prog () with
    | effect (Connect (src, dst)), k -> 
        Printf.printf "handle connect %d %d\n%!" src dst;
        let task = 
          let open Fiber in
          let* () = Ops.yield () in
          let* () = send 0 "msg" in    let* _ = Fiber.liftF (FiberOps.Set (4, Fiber.identity)) in
          let* s = Fiber.liftF (FiberOps.Get Fiber.identity) in 
          Fiber.return (Printf.printf "state %d\n" s)
        in
        continue k (Ops.spawnf (fun () -> task))
      | effect (Send (dst, msg)), k ->
          Printf.printf "handle send %d %s\n%!" dst msg;
          continue k (Ops.spawn (fun () -> Printf.printf "sended\n%!"; ))
end