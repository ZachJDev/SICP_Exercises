const string = "foobar"
let newString = ""
let j = string.length - 1 
while(j >= 0) {
    newString += string[j--]
}

console.log(newString)


const title = dv.current().file.name
const weekStart = DateTime.fromISO(title).startOf("week").plus({days: -1}).toISODate()
const weekEnd = weekStart.endOf("week").plus({days: 1})
console.log(weekStart, WeekEnd)
for (let group of dv.pages('"Personal/Exercise/Workouts"').filter(page => {
return weekStart <= page["date-taken"] && page["date-taken"] <= weekEnd
}).groupBy(p => p.type)) {
	dv.header(4, group.key)
	dv.table(["Workout", "Subtype",  "Blurb"],
		group.rows.map(k => [k.file.link, k["subtype"], k["blurb"]]))
}