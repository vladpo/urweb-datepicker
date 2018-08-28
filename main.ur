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

fun calendarEntry setRecordField getRecordField (sc: source calendar) : xbody = 
	<xml>
		<button 
			value="prev"
			onclick={fn _ =>
				c <- get sc;
				set sc (setRecordField c Prev)
			}
		/>
		<dyn signal={
			c <- signal sc;
			return <xml>{[getRecordField c]}</xml>}
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

fun updateMonth (c: calendar) (f: int -> int) = updateField [#Month] c f
fun updatePrevMonth (c: calendar) = updateMonth c (fn m => monthIndex (m - 1))
fun updateNextMonth (c: calendar) = updateMonth c (fn m => monthIndex (m + 1))

fun updateYear (c: calendar) (f: int -> int) = updateField [#Year] c f
fun updatePrevYear (c: calendar) cy = updateYear c (fn y => if y - 1 < cy then cy else y - 1)
fun updateNextYear (c: calendar) = updateYear c (fn y => y + 1)

fun firstOfMonth t = fromDatetime(datetimeYear t)(datetimeMonth t) 1 0 0 0
fun currentMonth t = fromDatetime(datetimeYear t)(datetimeMonth t) 0 0 0 0
fun nextMonth t = fromDatetime (datetimeYear t + ((datetimeMonth t + 1) / 12)) ((datetimeMonth t + 1) % 12) 0 0 0 0

fun nowMonthDays () = 
	t <- now;
	return (diffInSeconds (currentMonth t) (nextMonth t) / (60*60*24))

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

fun listItems xs= List.mapX(fn x => <xml><li class={Styles.flex_item}>{[x]}</li></xml>) xs

fun skipDays t = 
	let
		val i = (Datetime.dayOfWeekToInt <<< Datetime.dayOfWeek <<< Datetime.fromTime <<< firstOfMonth) <| t
		val j = Datetime.dayOfWeekToInt Datetime.Monday
		val k = if i - j >= 0 then i - j else neg(i - j)
	in
		fill(k)(fn _ => "")
	end

fun main () = 
	t <- now;
	days <- nowMonthDays ();
	sc <- source {Year = datetimeYear t, Month = datetimeMonth t, Day = datetimeDay t};
	return
		<xml>
			<head>
				<link rel="stylesheet" href="/styles.css"/>
			</head>
			<body>
				{calendarEntry 
					(fn c (a: action) => 
						case a of 
								Prev => updatePrevMonth c
							| Next => updateNextMonth c)
					(fn c => month c.Month)
					sc}
				<br/>
				{calendarEntry 
					(fn c (a: action) => 
						case a of
								Prev => updatePrevYear c (datetimeYear t)
							| Next => updateNextYear c)
					(fn c => show c.Year) 
					sc}
				<br/>
				<ul class={Styles.flex_container}>
				{listItems (List.append weekDays (List.append(skipDays t)(calendarDays days)))}
				</ul>
			</body>
		</xml>