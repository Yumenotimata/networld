module type Functor = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module Free = functor (F : Functor) -> struct
	type 'a free =
		| Free of 'a free F.t
		| Pure of 'a

	let rec bind = 
		fun free m -> 
			match free with
				| Free f -> Free (F.map (fun a -> bind a m) f)
				| Pure a -> m a

	let (let*) m k = bind m k
  let return a = Pure a
  let identity x = x
  let liftF f = Pure (F.map return f)
end

