import forcomp.Anagrams._

val occ1 = "abcdef".toList.map(x => (x,5))
val occ2 = "ace".toList.map(x => (x,2))
val occ3 = occ2:::List(('f',5))

def subtract( x:List[(Char,Int)], y:List[(Char,Int)]) = {
  (for {
    (xa,xi) <- x
    subtracted = y.find(fy => fy._1 == xa) match {
      case Some(fv:(Char,Int)) => xi - fv._2
      case None => xi
    }} yield (xa,subtracted)).filter(ov => ov._2 >0)
  }
@scala.annotation.tailrec
def subtract2( x:List[(Char,Int)], y:List[(Char,Int)]) :List[(Char,Int)] = y match{
  case ya::ys => subtract2(x.toMap.updated(ya._1,x.toMap.apply(ya._1) -ya._2 ).toList,ys)
  case Nil => x.filter( (v:(Char,Int)) => v._2 > 0).sorted
}

subtract2(occ1, occ2)
subtract2(occ1, occ3)
def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.reduce(_ + _))

val cat = sentenceOccurrences(List("one"))

val blah = combinations(cat).map(x => (x,dictionaryByOccurrences.get(x)))
// This is most of it.
// This gives a list of (subset, Option(wordlist))
// we need to
   // map/iterate/foldleft over the list
   // pattern match inside this iteration to pick out some(wordlist)
      // if there are wordlists, then map/iterate/fold again over these
      // if there are no wordlists (and only no wordlists, return/append the sentencelist



//base case should be an empty list
//recursive method

// Anagrams = we only care about cases where all letters are used

// if occurences is empty return List(currentSentence)


//for {
//  subset <-  combinations(occurenceRoom)
//  newWords <- subset.map(dictionaryByOccurrences.get)
//} yield (subset,newWords)
//goes to
//combinations(occurenseRoom).flatMap(subset => subset.map(dictionary.get).map(newwords => (newwords,subset)))

//combinations(sentenceOccurrences(List("catdog"))).flatMap(subset => subset.map)
//def moose(sentence: Sentence): List[Sentence] = {
//  @scala.annotation.tailrec
//  def moosemoose(occurenceRoom: Occurrences, currentSentence: Sentence): List[Sentence] = {
//    //      def wordsets = combinations(occurenceRoom).flatMap(w => dictionaryByOccurrences.get)
//    //      val moose = combinations(occurenceRoom).flatMap()
//    occurenceRoom match {
//      case List() => List(currentSentence)
//      case y::ys => { for {  // check that occurenceRoom is not empty
//        subset <- combinations(occurenceRoom)
//        newwordOpt <- dictionaryByOccurrences.get(subset)
//        newword <- Some(newwordOpt)
//      } yield moosemoose(subtract2(occurenceRoom,subset),newword::currentSentence)}.flatten
//    }
//  }
//} // doesn't work, type mismatch

val list1 = List(1,2,3)
val list2 = List("cat","dog","moose")

val sounds: List[String] = for {
  x<- list1
  y<- list2
} yield {(x,y) match {
  case (1,_) => "one"
  case (_,"cat") => "meow"
  case _ => "other"
}}

sounds

def chiwowow(sentence: Sentence): List[Sentence] = {
  def woof(occurenceRoom: Occurrences,currentSentence: Sentence): List[Sentence] ={
    if (occurenceRoom.isEmpty) List(currentSentence) else {
      {
        for { subset <- combinations(occurenceRoom) }
          yield { dictionaryByOccurrences.get(subset) match {
            case Some(listOfWords) => {
              for (ww <- listOfWords) yield woof(subtract(occurenceRoom,subset),ww::currentSentence)
              }.flatten
            case None => Nil
          }}.flatten
      }
    }}
  woof(sentenceOccurrences(sentence),Nil)
}


def chiwowow2(sentence: Sentence): List[Sentence] = {
  def woof(occurenceRoom: Occurrences,currentSentence: Sentence): List[Sentence] ={
    if (occurenceRoom.isEmpty) List(currentSentence) else {

        for { subset <- combinations(occurenceRoom)
              word <-  dictionaryByOccurrences.getOrElse(subset,Nil)
          sentence <- woof(subtract(occurenceRoom,subset),word::currentSentence)
              }
          yield sentence
    }}
  woof(sentenceOccurrences(sentence),Nil)
  }

val theAnagrams = chiwowow2(List("cat","dog"))




println("done?")


