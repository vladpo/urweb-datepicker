fun nthSplit s ch n = 
	let
		fun nthSplit' s acc = 
			case (String.split s ch) of 
				| Some(x, s') => nthSplit' s' (x::acc)
				| None => List.rev acc
	in
		List.nth (nthSplit' s []) n
	end