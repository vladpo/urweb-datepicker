open Styles

type calendar = {Year: int, Month: int, Day: int}
type months = list (int * string)
datatype action = Prev | Next

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

fun updateField [nm:: Name] [rest::: {Type}] [[nm] ~ rest] (r: $([nm = int] ++ rest)) (f: int -> int) : $([nm = int] ++ rest) =
	(r -- nm) ++ {nm = f r.nm}

fun updateYear (c: calendar) (f: int -> int) = updateField [#Year] c f
fun updatePrevYear (c: calendar) cy = updateYear c (fn y => if y - 1 < cy then cy else y - 1)
fun updateNextYear (c: calendar) = updateYear c (fn y => y + 1)

fun updateMonth (c: calendar) (f: int -> int) = updateField [#Month] c f

fun updatePrevMonth (c: calendar) cy = 
	let
		val c' = updateMonth c (fn m => monthIndex (m - 1))
	in
		if c.Month - 1 < 0 then
		 updatePrevYear c' cy
	 else
	 	 c'
	end

fun updateNextMonth (c: calendar) = 
	let
		val c' = updateMonth c (fn m => monthIndex (m + 1))
	in
		if c.Month + 1 >= 12 then
			updateNextYear c'
		else
			c'
	end

fun monthTime c i = fromDatetime (c.Year + ((c.Month + i) / 12)) ((c.Month + i) % 12) 1 0 0 0
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

fun listItems xs= List.mapX(fn x => <xml><li class={Styles.days_item}>{[x]}</li></xml>) xs

fun skipDays c = 
	let
		val i = (Datetime.dayOfWeekToInt <<< Datetime.dayOfWeek <<< Datetime.fromTime <<< currentMonth) <| c
		val j = Datetime.dayOfWeekToInt Datetime.Monday
		val k = if i - j >= 0 then i - j else neg(i - j)
	in
		fill(k)(fn _ => "")
	end

fun calendarWidget sc t = 
		<xml>
		{calendarMenu 
			(fn c (a: action) => 
				case a of 
						Prev => updatePrevMonth c (datetimeYear t)
					| Next => updateNextMonth c)
			sc}
		<ul class={Styles.calendar_container}>
			<li>
				<ul class={Styles.month_container}>
					<li>
						<dyn 
							signal={
								c <- signal sc;
								return <xml>{[month(monthIndex c.Month) ^ " " ^ (show c.Year)]}</xml>}
						/>
					</li>
					<li>
						<ul class={Styles.days_container}>
							<dyn 
								signal={
									c <- signal sc;
									return (listItems (List.append weekDays (List.append(skipDays c)(calendarDays(currentMonthDays c)))))}
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
									val c' = updateNextMonth c
								in
									return <xml>{[month(monthIndex c'.Month) ^ " " ^ (show c'.Year)]}</xml>
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
										val c' = updateNextMonth c
									in
										return (listItems (List.append weekDays (List.append(skipDays c')(calendarDays (currentMonthDays c')))))
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
	sc <- source {Year = datetimeYear t, Month = datetimeMonth t, Day = datetimeDay t};
	return
		<xml>
			<head>
				<link rel="stylesheet" href="/styles.css"/>
			</head>
			<body>
				{calendarWidget sc t}
			</body>
		</xml>