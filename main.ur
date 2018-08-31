open Styles

type date = {Year: int, Month: int, Day: int}
type calendar = {CurrentYear: int, Date: date}
type months = list (int * string)
datatype action = Prev | Next
type mouseState = {Over: bool, Clicked: bool}
type dayState = {MS: source mouseState, Date: date}
type bookedDates = {First: option date, Last: option date}
type state = {BookedDates: bookedDates, DayStates: list dayState}

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

fun updateField [a] [nm:: Name] [rest::: {Type}] [[nm] ~ rest] (r: $([nm = a] ++ rest)) (f: a -> a) : $([nm = a] ++ rest) =
	(r -- nm) ++ {nm = f r.nm}

fun updateYear (c: calendar) (f: int -> int) = updateField [date] [#Date] c (fn d => updateField [int] [#Year] d f)
fun prevCalYear (c: calendar) cy = updateYear c (fn y => if y - 1 < cy then cy else y - 1)
fun nextCalYear (c: calendar) = updateYear c (fn y => y + 1)

fun updateMonth (c: calendar) (f: int -> int) = updateField [date] c (fn d => updateField [int] [#Month] d f)

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

fun fill [a] n (f: int -> a) : list a = 
	let
		fun fill' i acc =
			if i <= n then
				fill' (i + 1) ((f i)::acc)
			else
				acc
	in
		fill' 1 []
	end

fun calendarDays days : list string = List.rev(fill days (fn i => show i))

fun listItems xs m dss = 
	List.mapX(
		fn x => 
			<xml>
				<li
					onclick = {
						fn _ =>
							List.map(dss
							set dss 
					onmouseover={
						fn _ =>
						dss <- get dss;

					dynClass={Styles.days_item}>{[x]}
				</li>
			</xml>) xs

fun skipDays c = 
	let
		val i = (Datetime.dayOfWeekToInt <<< Datetime.dayOfWeek <<< Datetime.fromTime <<< currentMonth) <| c
		val j = Datetime.dayOfWeekToInt Datetime.Monday
		val k = if i - j >= 0 then i - j else neg(i - j)
	in
		fill(k)(fn _ => "")
	end

fun calendarWidget sc dss = 
		<xml>
		{calendarMenu 
			(fn c (a: action) => 
				case a of 
						Prev => updatePrevMonth c c.CurrentYear
					| Next => nextCalMonth c)
			sc}
		<ul class={Styles.calendar_container}>
			<li>
				<ul class={Styles.month_container}>
					<li>
						<dyn 
							signal={
								c <- signal sc;
								return <xml>{[month(monthIndex c.Date.Month) ^ " " ^ (show c.Date.Year)]}</xml>}
						/>
					</li>
					<li>
						<ul class={Styles.days_container}>
							<dyn 
								signal={
									c <- signal sc;
									return (listItems(List.append weekDays (List.append(skipDays c)(calendarDays(currentMonthDays c)))) c.Date.Month dss)}
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
								c <- signal sc;
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
									c <- signal sc;
									let
										val c' = nextCalMonth c
									in
										return (listItems(List.append weekDays (List.append(skipDays c')(calendarDays(currentMonthDays c')))) c'.Date.Month dss)
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
		val c = {CurrentYear = datetimeYear t, Date = {Year = datetimeYear t, Month = datetimeMonth t, Day = datetimeDay t}}
		fun f = calendarDays <<< currentMonthDays
		val c' = nextCalMonth c
		val mapDaysWithMonth c = List.map(fn d => (d, c.Date.Month))

	in
		sc <- source c;
		st <- { 
			BookedDates = source {First = None, Last = None},
				DayStates = 
					List.map(
						fn (d, m) => 
							{S = source {Over = false, Clicked = false}, Day = d, Month = m})
						(List.append(mapDaysWithMonth c (f c))(mapDaysWithMonth c' (f c')))}
		return
			<xml>
				<head>
					<link rel="stylesheet" href="/styles.css"/>
				</head>
				<body>
					{calendarWidget sc dss}
				</body>
			</xml>
	end