open Fiber

open Ops

let (let*) = Fiber.bind

let (>>) a b = let* _ = a in b

module Meta = struct
  open Effect
  open Effect.Deep

  type uid = int

  type device = {
    id: uid;
    buffer: string Queue.t;
  }

  type connection = {
    src: uid;
    dst: uid;
  }

  type network = {
    devices: (uid, device) Hashtbl.t;
    connections: connection list ref;
    next_id: int ref;
  }

  type _ Effect.t +=
    | Create : (uid Fiber.free) Effect.t
    | Connect : uid * uid -> (unit Fiber.free) Effect.t
    | Send : uid * string -> (unit Fiber.free) Effect.t
    | Receive : uid -> (string Fiber.free) Effect.t

  let create_device id = {
    id;
    buffer = Queue.create ();
  }

  let create_network ?(initial_device_capacity=16) () = {
    devices = Hashtbl.create initial_device_capacity;
    connections = ref [];
    next_id = ref 0;
  }

  let create () = perform Create
  let connect src dst = perform (Connect (src, dst))
  let send dst msg = perform (Send (dst, msg))
  let receive id = perform (Receive id)

  let is_connected network src dst =
    List.exists (fun conn -> conn.src = src && conn.dst = dst) !(network.connections)
    (* : (unit -> 'a Fiber.free) -> 'a Fiber.free *)
  let run  = fun prog -> 
    let network = create_network () in

    let get_device id =
      Hashtbl.find_opt network.devices id
    in

    let handle_create k =
      let id = !(network.next_id) in
      network.next_id := id + 1;
      let dev = create_device id in
      Hashtbl.add network.devices id dev;
      Printf.printf "Created device %d\n%!" id;
      continue k (Fiber.return id)
    in

    let handle_connect src dst k =
      match get_device src, get_device dst with
      | Some src_dev, Some dst_dev ->
          network.connections := { src = src_dev.id; dst = dst_dev.id } :: !(network.connections);
          Printf.printf "Connected device %d to %d\n%!" src_dev.id dst_dev.id;
          continue k (Fiber.return ())
      | None, _ ->
          Printf.printf "Source device %d does not exist\n%!" src;
          continue k (Fiber.return ())
      | _, None ->
          Printf.printf "Destination device %d does not exist\n%!" dst;
          continue k (Fiber.return ())
    in

    
    let wakers = Hashtbl.create 16 in

    let rec handle_receive id k =
      match get_device id with
      | Some dev ->
          if not (Queue.is_empty dev.buffer) then
            let msg = Queue.pop dev.buffer in
            continue k (Fiber.return msg)
          else
            Printf.printf "waker not called\n%!";
            continue k (waker (fun wake -> Printf.printf "waker called\n%!"; Hashtbl.add wakers 0 wake) >> lift (fun () -> Queue.pop dev.buffer))
      | None ->
          Printf.printf "Device %d does not exist\n%!" id;
          continue k (Fiber.return "unreachable!")
    in

    let handle_send dst msg k =
      match get_device dst with
      | Some dst_dev ->
          Printf.printf "send from %d to %d\n%!" dst dst_dev.id;
          Queue.add msg dst_dev.buffer;
          if Hashtbl.mem wakers 0 then begin
            let wake = Hashtbl.find wakers dst_dev.id in
            wake ();
            Printf.printf "waker called\n%!"
          end else
            Printf.printf "Message sent to device %d: %s\n%!" dst msg;
          continue k (Fiber.return ())
      | None ->
          Printf.printf "Device %d does not exist\n%!" dst;
          continue k (Fiber.return ())
    in

    let fiber = 
      try prog () with
      | effect Create, k -> handle_create k
      | effect (Connect (src, dst)), k -> handle_connect src dst k
      | effect (Send (dst, msg)), k -> handle_send dst msg k
      | effect (Receive id), k -> handle_receive id k
    in
    fiber
end 