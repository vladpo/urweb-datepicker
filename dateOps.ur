type date = {Day: int, Month: int, Year: int}

fun bf (d1: date) (d2: date) = d1.Year < d2.Year || (d1.Year = d2.Year && d1.Month < d2.Month) || (d1.Year = d2.Year && d1.Month = d2.Month && d1.Day < d2.Day)

fun af (d1: date) (d2: date) = not(d1 = d2) && not(bf d1 d2)

fun between (d1: date) (d2: date, d3: date) = bf d2 d1 && bf d1 d3