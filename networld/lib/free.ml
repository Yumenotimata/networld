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

  let (>>) m1 m2 =
    let* _ = m1 in
    m2
end 