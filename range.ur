fun fillWhile [a] [b] (init: b) (f: b -> option b) (g: b -> a) : list a =
	let
		fun fillWhile' next acc = 
			case (f next) of
				| Some nextNext => fillWhile' nextNext (g next :: acc)
				|	None => acc
	in
		fillWhile' init []
	end

fun fillRange [a] n m (f: int -> a): list a =
	let
		fun fill' i =
			if i <= m then
				Some(i+1)
			else
				None
	in
		fillWhile n fill' f
	end

fun filli [a] n (f: int -> a) : list a = fillRange 1 n f