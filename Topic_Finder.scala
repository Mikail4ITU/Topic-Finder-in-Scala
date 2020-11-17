
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.io.Source


object Topic_Finder extends App {
  //Creating word list array from the lines of text file lines
  def getWordList(line: String): Array[String] = {
    line.split("[ ,!?.;:\"']+")
  }

  //Creating map of words in given file
  def dict(filename: String): Map[String, Int] = {
    //Word map
    var wordMap = Map[String, Int]()
    // Reading file and gettin the lines
    val lines = Source.fromFile("C:/Users/new/Desktop/UNIGE/Msc.Business Analytics/Algorithmics and Data Management/" + filename).getLines().toArray

    var Count = 1
    for (line <- lines) {
      //Array of words in file
      var mList = getWordList(line)
      for (word <- mList) {
        //Transforming each variable to lowercase
        var Word = word.toLowerCase()
        //if the word is not in the map already we are adding to map
        if (!wordMap.contains(Word)) {
          // We are adding the key and value to the map
          wordMap += (Word -> Count)

        }
        else {
          //If the map already contains the word we increase the occurence
          if (wordMap.contains(Word))
            wordMap(Word) += 1


        }

      }
    }

    return wordMap
  }

  // Score function to compute frequency score by dividing the values of two maps
  def Score(mapName: mutable.Map[String, Int], mapName2: mutable.Map[String, Int]): mutable.Map[String, Double] = {
    // Creating Score Map
    val score = mutable.Map[String, Double]()
    for ((key, _) <- mapName) {
      //If the word in text file exists in the reference vocabulary file and the occurence of the word in text is not less than 3
      if (mapName2.contains(key) && mapName(key) > 3) {
        //Computing each score of keys
        score(key) = mapName(key).toDouble / mapName2(key)
      }
    }
    return score
  }

  //Creating map of test words via dict function
  var map = Map[String, Int]()
  map = dict("t5.txt")
  //Creating map of general vocabulary file words
  var map2 = Map[String, Int]()
  map2 = dict("gen-voc.txt")

  //Printing each size of maps
  println("The size of t5.txt file: " + map.size + " Words")
  println("The size of gen-voc.txt file: " + map2.size + " Words")
  //Creating score map via Score function
  var score = Score(map, map2)
  println("The size of Frequency file: " + score.size + " Words")
  //Sorting the score map in descending order
  var sortedMap = ListMap(score.toSeq.sortWith(_._2 > _._2): _*)
  println("Topics: ")
  //Printing first 20 words from sorted score map
  for ((key,value) <- sortedMap.take(20))  {
    println("Key: " + key, "Value: " + value)
  }
}