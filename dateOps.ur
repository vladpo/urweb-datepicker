open OptOps

type date = {Day: int, Month: int, Year: int}

val showDate = mkShow(fn (d: date) => "{" ^ "Day: " ^ (show d.Day) ^ ", Month: " ^ (show d.Month) ^ ", Year: " ^ (show d.Year) ^ "}")

val eqDate = mkEq(fn (l: date) (r: date) => l.Day = r.Day && l.Month = r.Month && l.Year = r.Year)

fun bf(d1: date)(d2: date) = d1.Year < d2.Year || (d1.Year = d2.Year && d1.Month < d2.Month) || (d1.Year = d2.Year && d1.Month = d2.Month && d1.Day < d2.Day)

fun bfEq(d1: date)(d2: date) = d1 = d2 || (bf d1 d2)

fun af(d1: date)(d2: date) = not(d1 = d2) && not(bf d1 d2)

fun between(d1: date)(d2: date, d3: date) = bf d2 d1 && bf d1 d3
	
fun mbf(md1: option date)(d2: date): bool = mpOr (fn d => bf d d2) md1 False

fun bfm(d1: date)(md2: option date): bool = mpOr (fn d => bf d1 d) md2 False

fun mbfEq(md1: option date)(d2: date): bool = mpOr (fn d => bfEq d d2) md1 False

fun bfEqm(d1: date)(md2: option date): bool = mpOr (fn d => bfEq d1 d) md2 False

fun bfEqmOr(d1: date)(md2: option date): bool -> bool = fn b => mpOr (fn d => bfEq d1 d) md2 b

fun betweenm(d1: date)((md2: option date), (md3: option date)): bool = mpOr(fn d2 => mpOr(fn d3 => between d1 (d2, d3)) md3 False) md2 False