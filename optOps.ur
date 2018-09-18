val isNone = Option.isNone

fun isSome [a] (x: option a): bool = Option.isSome x

fun or [a] (x: option a)(y: option a): option a = if isSome x then x else y

fun getOr [a] (x: option a) (y: a) : a = Option.get y x

val getOrFalse : option bool -> bool = fn mb => mb `getOr` False

val getOrTrue : option bool -> bool = fn mb => mb `getOr` True

fun mpOr[a][b](f: a -> b)(mx: option a)(y: b): b = getOr (Option.mp(fn x => f x) mx) y