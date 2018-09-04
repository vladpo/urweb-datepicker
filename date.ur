type date = {Year: int, Month: int, Day: int}

fun bf (d1: date) (d2: date) = d1.Year < d2.Year || (d1.Year = d2.Year && d1.Month < d2.Month) || (d1.Year = d2.Year && d1.Month = d2.Month && d1.Day < d2.Day)

fun af (d1: date) (d2: date) = not(d1 = d2) && not(bf d1 d2)