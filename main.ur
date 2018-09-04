open Styles
open Date

type calendar = {CurrentYear: int, Date: date}
type months = list (int * string)
datatype action = Prev | Next
type mouseState = {Over: bool, Clicked: bool}
type dayState = {MS: source mouseState, Date: date}
type bookedDates = {First: option date, Last: option date}
type state = {Calendar: source calendar, BookedDates: source bookedDates, DayMouseStates: list dayState}

val allMonths: months = 
	(0, "Ianuarie") ::
	(1, "Februarie") ::
	(2, "Martie") ::
	(3, "Aprilie") ::
	(4, "Mai")::
	(5, "Iunie")::
	(6, "Iulie")::
	(7, "August")::
	(8, "Septembrie")::
	(9, "Octombrie")::
	(10, "Noiembrie")::
	(11, "Decembrie")::
	[]

val weekDays =
	"Lu"::"Ma"::"Mi"::"Jo"::"Vi"::"Sa"::"Du"::[]

fun calendarMenu setRecordField (sc: source calendar) : xbody = 
	<xml>
		<button 
			value="prev"
			onclick={fn _ =>
				c <- get sc;
				set sc (setRecordField c Prev)
			}
		/>
		<button 
			value="next" 
			onclick={fn _ =>
				c <- get sc;
				set sc (setRecordField c Next)
			}
		/>
	</xml>

fun month i =
	case List.find (fn m => m.1 = i) allMonths of
			Some m => m.2
		| None => "Unknown month"

fun monthIndex i = 
	if i >= 0 then 
		mod i 12
	else 
		mod (neg i) 12

fun mapName [a] [nm:: Name] [rest::: {Type}] [[nm] ~ rest] (r: $([nm = a] ++ rest)) (f: a -> a) : $([nm = a] ++ rest) =
	(r -- nm) ++ {nm = f r.nm}

fun withName [a] [nm:: Name] [rest::: {Type}] [[nm] ~ rest] (r: $([nm = a] ++ rest)) (x: a) : $([nm = a] ++ rest) = mapName[a][nm] r (fn _ => x)

fun updateYear (c: calendar) (f: int -> int) = mapName [date] [#Date] c (fn d => mapName [int] [#Year] d f)
fun prevCalYear (c: calendar) cy = updateYear c (fn y => if y - 1 < cy then cy else y - 1)
fun nextCalYear (c: calendar) = updateYear c (fn y => y + 1)

fun updateMonth (c: calendar) (f: int -> int) = mapName [date] c (fn d => mapName [int] [#Month] d f)

fun updatePrevMonth (c: calendar) cy = 
	let
		val c' = updateMonth c (fn m => monthIndex (m - 1))
	in
		if c.Date.Month - 1 < 0 then
		 prevCalYear c' cy
	 else
	 	 c'
	end

fun nextCalMonth (c: calendar) = 
	let
		val c' = updateMonth c (fn m => monthIndex (m + 1))
	in
		if c.Date.Month + 1 >= 12 then
			nextCalYear c'
		else
			c'
	end

fun monthTime c i = fromDatetime (c.Date.Year + ((c.Date.Month + i) / 12)) ((c.Date.Month + i) % 12) 1 0 0 0
fun currentMonth c = monthTime c 0
fun nextMonth c = monthTime c 1
fun twoMonths c = monthTime c 2 
fun monthDays t1 t2 = diffInSeconds t1 t2 / (60*60*24) 
fun currentMonthDays c = monthDays (currentMonth c) (nextMonth c)

fun identity [a] x = x

fun isEmpty s = strlenGe s 0
fun nonEmpty s = not (isEmpty s 0)

fun fillWhile [a] [b] (init: y) (f: y -> option y) (g: y -> x) : list x =
	let
		fun fillWhile' next acc = 
			case f next of
				  Some nextNext => fillWhile' nextNext (g nextB :: acc)
				|	None => acc
	in
		fillWhile' init []
	end

fun fill [a] n (f: int -> a) : list a =
	let
		fun fill' i =
			if i <= n then
				Some(i+1)
			else
				None
	in
		fillWhile 1 fill' f
	end

fun calDays days : list string = List.rev(fill days (fn i => show i))

fun listItems xs c st = 
	let
		val filterClicked = 
			List.filterM(
				fn ds => 
					ms <- signal ds.MS
					return ms.Clicked) st.DayMouseStates
		
		fun setBookedDate [nm::Name] [[nm] ~ st.DayMouseStates.BookedDates] (md : option date) =
			bd <- get st.DayMouseStates.BookedDates;
			set st.DayMouseStates.BookedDates (seName[nm] bd md)

		fun calWithDay x = withName[#Day] c.Date (read x)

		fun setClicked ds b = 
			ms <- get ds.MS;
			set ds.MS (withName[#Clicked] ms b)

		fun setClickedForDay x b = 
			case (List.find(fn ds => ds.Date = (calWithDay x)) st.DayMouseStates) of
					Some(ds) => 
					setClicked ds b			
				|	None => ()
	in
		List.mapX(
			fn x => 
				<xml>
					<li
						onclick = {
							fn _ =>
								let
									val dateDayX = withName[#Day] c.Date (read x)
								in
									body
								end
								if nonEmpty x then
									case filterClicked of 
											[] => 
												setBookedDate[#First] Some(calWithDay x)
												setClickedForDay x true
										|	ds::[] => 
												if ds `bf` (dateDayX) || ds == dateDayX then
													setBookedDate[#Second] Some(calWithDay x)
												else ()
										| ds1::ds2::[] =>
												if 
												setBookedDate[#Second] None
												setBookedDate[#First] Some(calWithDay x)
										| _ => ()
								else ()
						}
						onmouseover={
							fn _ =>
								List.find
						dynClass={Styles.days_item}>{[x]}
					</li>
				</xml>) xs
	end

fun skipDays c = 
	let
		val i = (Datetime.dayOfWeekToInt <<< Datetime.dayOfWeek <<< Datetime.fromTime <<< currentMonth) <| c
		val j = Datetime.dayOfWeekToInt Datetime.Monday
		val k = if i - j >= 0 then i - j else neg(i - j)
	in
		fill(k)(fn _ => "")
	end

fun calendarWidget st = 
		<xml>
		{calendarMenu 
			(fn c (a: action) => 
				case a of 
						Prev => updatePrevMonth c c.CurrentYear
					| Next => nextCalMonth c)
			st.Calendar}
		<ul class={Styles.calendar_container}>
			<li>
				<ul class={Styles.month_container}>
					<li>
						<dyn 
							signal={
								c <- signal st.Calendar;
								return <xml>{[month(monthIndex c.Date.Month) ^ " " ^ (show c.Date.Year)]}</xml>}
						/>
					</li>
					<li>
						<ul class={Styles.days_container}>
							<dyn 
								signal={
									c <- signal st.Calendar;
									return (listItems(List.append weekDays (List.append(skipDays c)(calDays(currentMonthDays c)))) c st)}
							/>
						</ul>
					</li>
				</ul>
			</li>
			<li>
				<ul class={Styles.month_container}>
					<li>
						<dyn 
							signal={
								c <- signal st.Calendar;
								let
									val c' = nextCalMonth c
								in
									return <xml>{[month(monthIndex c'.Date.Month) ^ " " ^ (show c'.Date.Year)]}</xml>
								end
							}
						/>
					</li>
					<li>
						<ul class={Styles.days_container}>
							<dyn
								signal={
									c <- signal st.Calendar;
									let
										val c' = nextCalMonth c
									in
										return (listItems(List.append weekDays (List.append(skipDays c')(calDays(currentMonthDays c')))) c' st)
									end
								}
							/>
						</ul>
					</li>
				</ul>
			</li>
		</ul>
	</xml>

fun main () = 
	t <- now;
	let
		val lastDate = {Year = 2019, Month = 8, Day = 23}
		val c = {CurrentYear = datetimeYear t, Date = {Year = datetimeYear t, Month = datetimeMonth t, Day = datetimeDay t}}
		fun boundedCalDays c = 
			if c.Date.Year <= lastDate.Year && c.Date.Month <= lastDate.Month && c.Date.Day <= lastDate.Day then
				calDays <<< currentMonthDays <| c
			else
				calDays <<< currentMonthDays <| {CurrentYear = c.CurrentYear, Date = lastDate} 
		fun maybeNextCalMonth c = 
			if c.Date.Year <= lastDate.Year && c.Date.Month <= lastDate.Month && c.Date.Day <= lastDate.Day then
				Some(nextCalMonth c)
			else
				None
		val cs = fillWhile c maybeNextCalMonth identity
		val mapDayMonthYear c = List.map(fn d => (d, c.Date.Month, c.Date.Year))
	in
		st <- {
			Calendar = source c,
			BookedDates = source {First = None, Last = None},
				DayMouseStates = 
					List.map(
						fn (d, m, y) => 
							{MS = source {Over = false, Clicked = false}, Date = {Day = d, Month = m, Year = y}})
						(List.foldl(fn c  acc => List.append(mapDayMonthYear c (boundedCalDays c), acc)) [] cs)}
		return
			<xml>
				<head>
					<link rel="stylesheet" href="/styles.css"/>
				</head>
				<body>
					{calendarWidget st}
				</body>
			</xml>
	end