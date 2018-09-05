open Styles

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
		|	Some m => m.2
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

fun fillWhile [a] [b] (init: b) (f: b -> option b) (g: b -> a) : list a =
	let
		fun fillWhile' next acc = 
			case (f next) of
				| Some nextNext => fillWhile' nextNext (g nextB :: acc)
				|	None => acc
	in
		fillWhile' init []
	end

fun filli [a] n (f: int -> a) : list a =
	let
		fun fill' i =
			if i <= n then
				Some(i+1)
			else
				None
	in
		fillWhile 1 fill' f
	end

fun calDays days : list string = List.rev(filli days (fn i => show i))

fun listItems xs c st = 
	let
		val filterClicked = 
			List.filterM(
				fn ds => 
					ms <- signal ds.MS;
					return ms.Clicked
			) st.DayMouseStates
		
		fun setBookedDate [nm::Name] [[nm] ~ bookedDates] (md : option date) =
			bd <- get st.DayMouseStates.BookedDates;
			set st.DayMouseStates.BookedDates (seName[nm] bd md)

		fun setMouseState [nm::Name] [[nm] ~ mouseState] ds b = 
			ms <- get ds.MS;
			set ds.MS (withName[nm] ms b)

		fun findDayState d = List.find(fn ds => ds.Date = d) st.DayMouseStates

		fun setMouseStateForDate [nm::Name] [[nm] ~ mouseState] d b = 
			case (findDayState d) of
				|	Some ds => 
					setMouseState [nm] ds b			
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
									if nonEmpty x then
										case filterClicked of 
											|	[] => 
													setBookedDate [#First] (Some dateDayX)
													setMouseStateForDate [#Clicked] dateDayX true
											|	ds::[] => 
													if ds `bf` (dateDayX) || ds = dateDayX then
														setBookedDate [#Second] (Some dateDayX)
													else ()
											| ds1::ds2::[] =>
													if dateDayX `bf` ds1 || ds1 `bf` dateDayX then
														setMouseState [#Clicked] ds1 false
														setMouseStateForDate [#Clicked] dateDayX true
														setBookedDate [#First] (Some dateDayX)
													else if ds2 = dateDayX || ds2 `bf` dateDayX then
														setMouseState [#Clicked] ds1 false
														setMouseState [#Clicked] ds2 false
														setMouseStateForDate [#Clicked] dateDayX true
														setBookedDate [#First] (Some dateDayX)
														setBookedDate [#Second] None
													else ()
											| _ => ()
									else ()
								end
						}
						onmouseover = {
							fn _ =>
								List.app(fn ds => setMouseState [#Over] ds false) st.DayMouseStates
								setMouseStateForDate [#Over] dateDayX true
						}
						dynClass = {
							case (findDayState dateDayX) of
								|	Some ds =>
										ms <- signal ds.MS;
										bd <- signal st.BookedDates;
										return 
											(if ms.Over && (not ms.Clicked) then
												if dateDayX `between` (bd.First, bd.Last) then
													classes Styles.days_item (classes Styles.day_over Styles.day_inbetween)
												else 
													classes Styles.days_item Styles.day_over
											else if dateDayX `between` (bd.First, bd.Last) then
												classes Styles.days_item Styles.day_inbetween
											else if ms.Clicked then
												classes Styles.days_item Styles.day_clicked
											else 
												Styles.days_item)
								| None => return Styles.days_item
						}
					>
						{[x]}
					</li>
				</xml>) xs
	end

fun skipDays c = 
	let
		val i = (Datetime.dayOfWeekToInt <<< Datetime.dayOfWeek <<< Datetime.fromTime <<< currentMonth) <| c
		val j = Datetime.dayOfWeekToInt Datetime.Monday
		val k = if i - j >= 0 then i - j else neg(i - j)
	in
		filli k (fn _ => "")
	end

fun calendarWidget st = 
		<xml>
		{calendarMenu 
			(fn c (a: action) => 
				case a of 
					|	Prev => updatePrevMonth c c.CurrentYear
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
	let
		val lastDate: date = {Year = 2019, Month = 8, Day = 23}
		
		val boundedCalDays =
			fn c =>
				if c.Date `bf` lastDate then
					calDays <<< currentMonthDays <| c
				else
					calDays <<< currentMonthDays <| (withName[#Date] c lastDate)
		
		val cs: list calendar = fillWhile c (fn c => if DateOps.bf(c.Date, lastDate) then Some(nextCalMonth c) else None) (fn c => c)
		
		fun mapDayMonthYear c = map(fn d => (d, c.Date.Month, c.Date.Year))

		fun initState c : state = 
			{	Calendar = source c, BookedDates = source {First = None, Last = None},
				DayMouseStates = 
					map(
						fn dmy => {MS = source {Over = false, Clicked = false}, Date = {Day = dmy.1, Month = dmy.2, Year = dmy.3}}
					) (List.foldl(fn c acc => List.append(mapDayMonthYear c (boundedCalDays c)) acc) [] cs)
			}
	in
		t <- now;
		return
			<xml>
				<head>
					<link rel="stylesheet" href="/styles.css"/>
				</head>
				<body>
					{calendarWidget (initState {CurrentYear = datetimeYear t, Date = {Year = datetimeYear t, Month = datetimeMonth t, Day = datetimeDay t}})}
				</body>
			</xml>
	end