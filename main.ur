open Styles
open DateOps
open OptOps
open Range
open StringOps

type calendar = {CurrentYear: int, Date: date}
type calEntry = {First: date, Last: date}
type months = list (int * string)
datatype action = Prev | Next
type mouseState = {Over: bool, Clicked: bool}
type dayState = {MS: source mouseState, Date: date}
type globalStates = {First: option date, Last: option date, PrevLast: option date, Over: option date, ClickedOut: bool}
type calSources = {Calendar: source calendar, GlobalStates: source globalStates, DayMouseStates: list dayState, Approved: list calEntry}

val showCalEntry = mkShow(fn (ce: calEntry) => "First: "^(show ce.First)^" -> Last: "^(show ce.Last))

table bookDate: {Id: int, First: string, Last: string, Approved: bool}
	PRIMARY KEY Id
sequence bookDate_seq

fun asDate md mm my = 
	case (md, mm, my) of
		| (Some d, Some m, Some y) => Some {Day = d, Month = m, Year = y}
		| _ => None

fun queryAllApproved (): transaction (list calEntry) =
	ces <- (query(SELECT * FROM bookDate WHERE bookDate.Approved={[True]})
					(fn r acc => 
						let
							val c = #"-"
							fun mRead ms = Option.bind(fn s => read s) ms
							fun day s = mRead(nthSplit s c 0)
							fun month s = mRead(nthSplit s c 1)
							fun year s = mRead(nthSplit s c 2)
							fun toDate s = asDate(day s)(month s)(year s)
						in
							case (toDate r.BookDate.First, toDate r.BookDate.Last) of
								| (Some first, Some last) => return (({First = first , Last = last})::acc)
								| _ => return acc
						end
					)
					[]);
	return (List.sort(fn x y => y.First `bfEq` x.First) ces)

fun bookDates mfirst mlast = 
	let
		fun toString md = 
			Option.bind(
				fn d =>
					case (show d.Day, show d.Month, show d.Year) of
						| (day, month, year) => Some (day^"-"^month^"-"^year)
						| _ => None) md
	in
		case (toString mfirst, toString mlast) of
		| (Some first, Some last) =>  
				id <- nextval bookDate_seq;
				dml(INSERT INTO bookDate (Id, First, Last, Approved) VALUES ({[id]}, {[first]}, {[last]}, {[True]}));
				return ()
		| _ => return ()
	end

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

val showDayState = mkShow(fn (ds: dayState) => show ds.Date)

fun calendarMenu modifyMonth (sc: source calendar) : xbody = 
	<xml>
		<button 
			value="prev"
			onclick={fn _ =>
				c <- get sc;
				set sc (modifyMonth c Prev)
			}
		/>
		<button 
			value="next" 
			onclick={fn _ =>
				c <- get sc;
				set sc (modifyMonth c Next)
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

fun mapName [a] [nm:: Name] [rest::: {Type}] [[nm] ~ rest] 
			(r: $([nm = a] ++ rest)) (f: a -> a) : $([nm = a] ++ rest) =
	(r -- nm) ++ {nm = f r.nm}

fun withName [a] [nm:: Name] [rest::: {Type}] [[nm] ~ rest] 
			 (r: $([nm = a] ++ rest)) (x: a) : $([nm = a] ++ rest) = mapName[nm] r (fn _ => x)

fun updateYear (c: calendar) (f: int -> int) = mapName [#Date] c (fn d => mapName [#Year] d f)
fun prevCalYear (c: calendar) cy = updateYear c (fn y => if y - 1 < cy then cy else y - 1)
fun nextCalYear (c: calendar) = updateYear c (fn y => y + 1)

fun updateMonth (c: calendar) (f: int -> int) = mapName [#Date] c (fn d => mapName [#Month] d f)

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

fun monthTime c i = 
	fromDatetime (c.Date.Year + ((c.Date.Month + i) / 12)) ((c.Date.Month + i) % 12) 1 0 0 0
fun currentMonth c = monthTime c 0
fun nextMonth c = monthTime c 1
fun twoMonths c = monthTime c 2 
fun monthDays t1 t2 = diffInSeconds t1 t2 / (60*60*24) 
fun currentMonthDays (c: calendar): int = monthDays (currentMonth c) (nextMonth c)

fun identity [a] (x: a) : a = x
fun swap [a] [b] (f: a -> a -> b) = fn x y => f y x

fun isEmpty s = String.length s = 0
fun nonEmpty s = not (isEmpty s)

fun calDays days : list string = List.rev(filli days (fn i => show i))

fun listItems xs (c: calendar) (st: calSources) =
	let
		val filterClickedM: transaction (list dayState) = 
			List.filterM(
				fn ds => 
					ms <- get ds.MS;
					return ms.Clicked
			) st.DayMouseStates

		fun gsWithFirst (md: option date) = fn gs => withName[#First] gs md
		fun gsWithLast (md: option date) = fn gs => withName[#Last] gs md
		fun gsWithPrevLast (md: option date) = fn gs => withName[#PrevLast] gs md
		fun gsWithOver (md: option date) = fn gs => withName[#Over] gs md
		fun gsWithClickedOut b = fn gs => withName[#ClickedOut] gs b
		fun setGlobalStates (f: globalStates -> globalStates) = 
			gs <- get st.GlobalStates;
			set st.GlobalStates (f gs)

		fun findDayState d = List.find(fn ds => ds.Date = d) st.DayMouseStates

		fun msWithOver b = fn ms => withName[#Over] ms b
		fun msWithClicked b = fn ms => withName[#Clicked] ms b
		fun setMouseState(ds: dayState)(f: mouseState -> mouseState) =
			ms <- get ds.MS;
			set ds.MS (f ms)
		fun setMouseStateForDate(d: date)(f: mouseState -> mouseState) = 
			case (findDayState d) of
				|	Some ds => setMouseState ds f
				| None => return ()

		fun first (ce: calEntry) = ce.First
		fun last (ce: calEntry) = ce.Last
		fun findApproved f mFirst = Option.mp f (List.find(fn ce => mFirst `mbf` (f ce)) st.Approved)

		fun dynStyles dateDayX =
			case (findDayState dateDayX) of
			|	Some ds =>
					ms <- signal ds.MS;
					gs <- signal st.GlobalStates;
					return
						(List.foldl
							(fn (c, b) cs => if b then classes cs c else cs)
						 	Styles.days_item
							((Styles.day_over,
								(isNone gs.First && ms.Over) || 
								(isNone gs.Last && gs.First `mbf` dateDayX && dateDayX `bfEqm` gs.Over) ||
								(isSome gs.Last && dateDayX `bfm` gs.First && gs.Over `mbfEq` dateDayX && dateDayX `bfmOrTrue` (findApproved first gs.Over)))::
							 (Styles.day_clicked, ms.Clicked)::
							 (Styles.day_inbetween,
							  gs.First `mbf` dateDayX && (dateDayX `bfEqmOr` gs.Last)(dateDayX `bfEqm` gs.PrevLast))::
							 (Styles.day_fade,
						 	  gs.ClickedOut &&
						 	  ((gs.First `mbfEq` dateDayX && dateDayX `bfm` gs.Over && dateDayX `bfEqm` gs.Last) ||
					 	  	(isNone gs.Last && gs.First `mbfEq` dateDayX && gs.Over `mbf` dateDayX && dateDayX `bfEqm` gs.PrevLast)))::
							 (Styles.day_disabled,
							 	isNone gs.Last &&
							 	((dateDayX `bfm` gs.First) ||
							 	(isSome gs.First && (findApproved first gs.First) `mbf` dateDayX)))::[]
						 	)
						)
			| None => 
					return (
						if dateDayX.Day = 0 then
							Styles.days_item
						else
							classes Styles.days_item Styles.day_disabled
					)
	in
		List.mapX(
			fn x => 
				let
					val dateDayX: date = withName[#Day] c.Date (Option.get 0 (read x))
				in
					<xml>
						<li
							onclick = {
								fn _ =>
									gs <- get st.GlobalStates;
									(filterClicked: list dayState) <- filterClickedM;
									case (filterClicked, x, findDayState dateDayX) of 
										| (_, "", _) => return ()
										| (_, _, None) => return ()
										|	([], _, _) => 
												setMouseStateForDate dateDayX (msWithClicked True);
												setGlobalStates(gsWithFirst (Some dateDayX))
										|	(ds::[], _, _) => 
												if isNone gs.Last && (ds.Date `bf` dateDayX || ds.Date = dateDayX) then
													setMouseStateForDate dateDayX (msWithClicked True);
													setGlobalStates((gsWithPrevLast None) <<< (gsWithLast(Some dateDayX)) <<< (gsWithClickedOut False))
												else if dateDayX `bf` ds.Date then
													setMouseState ds (msWithClicked False);
													setMouseStateForDate dateDayX (msWithClicked True);
													setGlobalStates(gsWithFirst(Some dateDayX) <<< (gsWithLast None) <<< (gsWithPrevLast None))
												else if isSome gs.First && isSome gs.Last && gs.Last `mbfEq` dateDayX then
													setMouseState ds (msWithClicked False);
													setMouseStateForDate dateDayX (msWithClicked True);
													setGlobalStates(gsWithFirst(Some dateDayX) <<< (gsWithPrevLast None) <<< (gsWithLast None) <<< (gsWithClickedOut False))
												else return ()
										| (ds1::ds2::[], _, _) =>
												if dateDayX `bf` ds1.Date || dateDayX `bf` ds2.Date then
													setMouseState ds1 (msWithClicked False);
													setMouseStateForDate dateDayX (msWithClicked True);
													setMouseState ds2 (msWithClicked False);
													setGlobalStates(gsWithFirst(Some dateDayX) <<< (gsWithPrevLast gs.Last) <<< (gsWithLast None) <<< (gsWithClickedOut False))
												else if ds2.Date = dateDayX || ds2.Date `bf` dateDayX then
													setMouseState ds1 (msWithClicked False);
													setMouseState ds2 (msWithClicked False);
													setMouseStateForDate dateDayX (msWithClicked True);
													setGlobalStates((gsWithFirst(Some dateDayX)) <<< (gsWithLast None) <<< (gsWithPrevLast None))
												else return ()
										| _ => return ()
							}
							
							onmouseover = {
								fn _ =>
									case (findDayState dateDayX) of
										|	Some _ =>
												setGlobalStates(gsWithOver(Some dateDayX));
												setMouseStateForDate dateDayX (msWithOver True)
										| None => return ()
							}
							
							onmouseout = {
								fn _ => 
									case (findDayState dateDayX) of
										|	Some _ =>
												setGlobalStates((gsWithOver None) <<< (gsWithClickedOut True));
												List.app(fn ds => setMouseState ds (msWithOver False)) st.DayMouseStates
										| None => return ()
							}
							
							dynClass = { dynStyles dateDayX}
						>
							{[x]}
						</li>
					</xml>
				end) xs
	end

fun skipDays c = 
	let
		val i = (Datetime.dayOfWeekToInt <<< Datetime.dayOfWeek <<< Datetime.fromTime <<< currentMonth) <| c
		val j = Datetime.dayOfWeekToInt Datetime.Monday
		val k = if i - j >= 0 then i - j else neg(i - j)
	in
		filli k (fn _ => "")
	end

fun calendarWidget (st: calSources) = 
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
		val lastDate: date = {Day = 23, Month = 8, Year = 2019}
		
		fun boundedCalDays c cc ces =
				let
					val cmd: int = currentMonthDays c	
				in
					List.filter
						(fn i => not (List.exists(fn ce => {Day = getOr (read i) 0, Month = c.Date.Month, Year = c.Date.Year} `betweenEq` (ce.First, ce.Last)) ces))
						(if c.Date.Month = lastDate.Month && c.Date.Year = lastDate.Year then
							List.rev(filli lastDate.Day (fn i => show i))
						else if c.Date = cc.Date then 
							List.rev(fillRange cc.Date.Day cmd (fn i => show i))
						else calDays cmd)
				end
		
		fun calendars c : list calendar = fillWhile c (fn c => if c.Date `bf` lastDate then Some(nextCalMonth c) else None) identity
		
		fun mapDayMonthYear c = List.mp(fn d => (d, c.Date.Month, c.Date.Year))

		val initState : transaction calSources = 
			approved <- queryAllApproved ();
			t <- now;
			let
				val cc = {CurrentYear = datetimeYear t, Date = {Day = datetimeDay t, Month = datetimeMonth t, Year = datetimeYear t}}
			in
				sc <- source cc;
				sbd <- source {First = None, Last = None, PrevLast = None, Over = None, ClickedOut = True};
				dmss <- List.mapM(
									fn dmy => 
										sms <- source {Over = False, Clicked = False};
										return {MS = sms, Date = {Day = (Option.get 0 (read (dmy.1))), Month = dmy.2, Year = dmy.3}}
								) (List.foldl(fn c acc => List.append(mapDayMonthYear c (boundedCalDays c cc approved)) acc) [] (calendars cc));
				return {Calendar = sc, GlobalStates = sbd, DayMouseStates = dmss, Approved = approved}
			end
	in
		cs <- initState;
		return
			<xml>
				<head>
					<link rel="stylesheet" href="/styles.css"/>
				</head>
				<body>
					{calendarWidget cs}
					<br/>
					<button value="Book" 
						onclick={
							fn _ => 
								gs <- get cs.GlobalStates;
								rpc (bookDates gs.First gs.Last);
								redirect (bless "/")
						}/>
				</body>
			</xml>
	end