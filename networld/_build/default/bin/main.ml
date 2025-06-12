
	(* [@@deriving show] *)

module type Functor = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end


module Free = functor (F : Functor) -> struct
	type 'a free =
		| Free of 'a free F.t
		| Pure of 'a
		(* [@@deriving show] *)

	let rec bind = 
		fun f m -> 
			match f with
				| Free f -> Free (F.map (fun a -> bind a m) f)
				| Pure a -> m a

	let (let*) m k = bind m k
	let pure f = Pure f
end

module Stack = struct
	type 'a t = 
		| Push of int * 'a
		| Pop of (int -> 'a)
		| Get of (string -> 'a)

	let map =
		fun fn f -> 
			match f with
				| Pop k -> Pop (fun a -> fn (k a))
				| Push (a, f) -> Push (a, fn f)
				| Get k -> Get (fun a -> fn (k a))
end

module FreeStack = Free(Stack)
open FreeStack

	let rec run f = 
		let stack = ref 33 in
		let str = ref "hello" in
		match f with
		| (Free (Push (i, f))) -> Printf.printf "push %d\n" i; stack := i; run f
		| (Free (Pop f)) -> Printf.printf "pop\n"; run (f !stack)
		|	(Free (Get f)) -> run (f !str)
		| (Pure a) -> Printf.printf "pure\n"

let main () =
	let* x = Free (Pop (fun x -> Pure (x))) in
	let* _ = (Free (Stack.map pure (Push (x, ())))) in 
	let* s = Free (Get (fun s -> Pure (s))) in Pure (Printf.printf "%s\n" s)

let _ = 
	let f = main () in run f