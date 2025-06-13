open Meta
open Fiber
(* open Ops *)
open Scheduler

let main () =
  Meta.run @@ fun () -> Scheduler.run @@
    let* _ = Fiber.liftF (FiberOps.Set (4, Fiber.identity)) in
    Meta.connect 0 0

let _ = main ()