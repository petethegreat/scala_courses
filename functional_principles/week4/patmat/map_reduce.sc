
def cleanedStringToCharList(s:String) = s.replaceAll("""[\p{Punct}\ ]""", "")
  .toLowerCase.toList

val charList = cleanedStringToCharList("There once was a man from Nantucket, and if anything moved he would bucket")

val grouped = charList.groupBy(x => x)
val charCounts = grouped.transform((k,v) => v.length)

val charCountsList = charCounts.toList
//charCounts.transform((k,v) => v.length)


// mapreduce it
val mapped = charList.map(x => (x,1))
val grouped2 = mapped.groupBy(x => x._1)
val reduced = grouped2.transform( (k,v) =>  v.reduce((a,b) => (a._1,a._2 + b._2) ))
val reducedValues = reduced.values
val reducedList = reducedValues.toList


// again
//val grouped_again = charList.groupBy(x => x)
val grouped_again = cleanedStringToCharList("hello world").groupBy(x => x)

val mapped_again = grouped_again.mapValues(x => x.map(y=>1))
val reduced_again = mapped_again.transform((k,v) => v.sum)
val listed_again = reduced_again.toList
listed_again.toSet

val one_lined = charList.groupBy(x => x).transform((_,v) => v.length).toList

// order our character tuples
val ordered_list = one_lined.sortBy(_._2) (Ordering[Int].reverse)


val mySet = Set(("e",1), ("w",1), ("h",1), ("r",1), ("d",1), ("l",3), ("o",2))
