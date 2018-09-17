fun splits s ch = 
	let
		fun splits' s acc = 
			case (String.split s ch) of 
				| Some(x, s') => splits' s' (x::acc)
				| None => List.rev (s::acc)
	in
		splits' s []
	end

fun nthSplit s ch n = List.nth(splits s ch) n