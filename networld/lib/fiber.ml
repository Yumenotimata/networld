open Free

module rec FiberOps : sig
  type 'a t =
    | Spawn of (unit -> 'a)
    | SpawnFree of ((unit -> 'a) * (unit -> 'a))
    | Yield of (unit -> 'a)
    | Waker of (((unit -> unit) -> unit) * (unit -> 'a))
    | Lift of (unit -> 'a)
  val map : ('a -> 'b) -> 'a t -> 'b t
end = struct
  type 'a t =
    | Spawn of (unit -> 'a)
    | SpawnFree of ((unit -> 'a) * (unit -> 'a))
    | Yield of (unit -> 'a)
    | Waker of (((unit -> unit) -> unit) * (unit -> 'a))
    | Lift of (unit -> 'a)

  let map fn = function
    | Spawn task -> Spawn (fun () -> fn (task ()))
    | SpawnFree (task, k) -> SpawnFree (Obj.magic task, (fun () -> fn (k ())))
    | Yield k -> Yield (fun () -> fn (k ()))
    | Waker (f, k) -> Waker (f, fun () -> fn (k ()))
    | Lift f -> Lift (fun () -> fn (f ()))
end

module Fiber = Free(FiberOps)

module Ops = struct
  let spawn task = Fiber.liftF (FiberOps.Spawn task)
  let spawnfree : (unit -> 'a Fiber.free) -> 'a Fiber.free = fun task -> Fiber.Free (FiberOps.SpawnFree (task, fun () -> Fiber.Pure ()))
  let spawnf : (unit -> 'a Fiber.free) -> 'a Fiber.free = fun task -> task ()
  let yield () = Fiber.liftF (FiberOps.Yield (fun () -> ()))
  let lift f = Fiber.liftF (FiberOps.Lift f)
  let liftF f a = Fiber.Pure (FiberOps.Lift (fun () -> f a))

  let waker f =
    Fiber.liftF (FiberOps.Waker (f, fun () -> ()))
end

module Scheduler = struct
  let run main =
    let q = Queue.create () in
    let enqueue f = Queue.push f q in

    let rec loop fiber = 
      match fiber () with
        | Fiber.Pure _ -> ()
        | Fiber.Free (FiberOps.Spawn task) ->
            enqueue (fun () -> loop @@ (fun () -> task ()))
        | Fiber.Free (FiberOps.Yield k) ->
            enqueue (fun () -> loop @@ (fun () -> k ()))
        | Fiber.Free (FiberOps.Waker (f, k)) -> 
            f (fun () -> Printf.printf "waker called\n"; enqueue (fun () -> loop @@ (fun () -> k ())))
        | Fiber.Free (FiberOps.SpawnFree (task, k)) ->
            enqueue (fun () -> loop @@ (fun () -> k ()));
            enqueue (fun () -> loop @@ (fun () -> task ()))
        | Fiber.Free (FiberOps.Lift f) -> 
            enqueue (fun () -> loop @@ f)
            (* f (fun () -> Printf.printf "waker called\n") *)
            (* enqueue (fun () -> loop @@ fun() -> k (fun () -> enqueue (fun () -> loop fiber))) *)
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