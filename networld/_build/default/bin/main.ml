open Networld
open Fiber
open Ops
open Meta

let (let*) f m = let a = f in m a 
let (>>=) = Fiber.bind
let (>>) a b = let* _ = a in b

let f = ref (fun () ->  ())

let test_waker () =
  let open Fiber in
  let open Ops in
  
  Printf.printf "start\n%!";
  spawnfree (fun () ->
    let* pc = Meta.create () in
    let* router = Meta.create () in
    let* _ = Meta.connect pc router in
    spawnfree (fun () -> 
      Printf.printf "receive\n%!";
      let* msg = Meta.receive pc in
      Fiber.return @@ Printf.printf "Message received: %s\n%!" msg
    ) >>
    spawnfree (fun () -> 
      Printf.printf "send\n%!";
      let* _ = Meta.send pc "hello" in Fiber.return ()
    )
  )

let () = 
  let _ = Meta.run @@ fun () -> Scheduler.run (fun () -> test_waker ()) in () 