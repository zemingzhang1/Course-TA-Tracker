package model

import java.io.FileWriter

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.{BufferedSource, Source}
import util.control.Breaks._

object TA_App {


  val dayNameToNumber = Map("SUNDAY" -> "A", "MONDAY" -> "B", "TUESDAY" -> "C", "WEDNESDAY" -> "D", "THURSDAY" -> "E", "FRIDAY" -> "F", "SATURDAY" -> "G")
  val dayNumberToName = Map("A" -> "SUNDAY", "B" -> "MONDAY", "C" -> "TUESDAY", "D" -> "WEDNESDAY", "E" -> "THURSDAY", "F" -> "FRIDAY", "G" -> "SATURDAY")
  val dayOW = List("MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", "FRIDAY", "SATURDAY", "SUNDAY")


  var onView: String = ""
  var q1:mutable.Queue[String] = mutable.Queue()
  var q2:mutable.Queue[String] = mutable.Queue()
  var holder:Map[String,collection.mutable.Map[String,collection.mutable.Map[String,ListBuffer[String]]]] = Map()
  var holderTwo:Map[String,collection.mutable.Map[String,collection.mutable.Map[String,ListBuffer[String]]]] = Map()
  var holderThree: ListMap[String, ListMap[String, Map[String, List[String]]]] = ListMap()
  var finalOutput:List[String] = List()
  var classList:List[String] = List()


  def submitCsv(a: String, b: String, c: String): List[String] = {
    val fw = new FileWriter("fileTA.csv", true)
    if (a != ""  && b != ""  && c != "") {
      val tuple1 = b + "%" + a
      val tupleString: String = c.replace(",", "-") + "@" + tuple1
      fw.write("\n" + tupleString)
      fw.close()
      q1 = mutable.Queue()
      q2 = mutable.Queue()
      finalOutput = readCsv()
      finalOutput
    }
    else {
      finalOutput = List("PLEASE FILL IN ALL INPUTS AS DIRECTED")
      finalOutput
    }
  }

  def history():List[String]={
    var list: List[String] = List()
    val file: BufferedSource = Source.fromFile("searchHistory.csv")
    for (line <- file.getLines()) {
      list ::= line.toString
    }
    println(list + " THIS IS THE HISTORY")
    list
  }

  def readCsv(): List[String] = {
    var list: List[String] = List()
    val file: BufferedSource = Source.fromFile("fileTA.csv")
    for (line <- file.getLines()) {
      val splits = line.split("\n")
      for (i <- splits)
        list ::= i.toString
    }
    list = list.reverse.drop(1)
    for (k <- list) {
      val pairs = k.split("-").grouped(2)
      val map1 = pairs.map { case Array(k, v) => k -> v.split("[%@]").map(_.trim).toList }.toMap
      for ((k, v) <- map1) {
        classList ::= v(0)
        if (holder.contains(v(1))) {
          if (holder(v(1)).contains(k)) {
            if (holder(v(1))(k).contains(v(0))) {
              if (holder(v(1))(k)(v(0)).contains(v(2))) {
                null
              }
              else {
                holder(v(1))(k)(v(0)) += v(2)
              }
            }
            else {
              holder(v(1))(k) += v(0) -> ListBuffer(v(2))
            }
          }
          else {
            holder(v(1)) += k -> collection.mutable.Map(v(0) -> ListBuffer(v(2)))
          }
        }
        else {
          holder += v(1) -> mutable.Map(k -> collection.mutable.Map(v(0) -> ListBuffer(v(2))))
        }
      }
    }
    println(holder)
    holderTwo = holder
    println("readCsv working")
    output(sort(muToImmU()))
  }


  def search(string: String): List[String] = {
    readCsv()
    holderTwo = holder
    if(string == "SHOWING"){
      finalOutput = history()
      return finalOutput
    }
    if (string != "") {
        val sL = (for (i <- string.split(",")) yield i).toList
        var t = ""
        var n = ""
        var d = ""
        var c = ""
        for ((day, timeMap) <- holderTwo) {
          for ((time, classMap) <- timeMap) {
            for ((classes, nameList) <- classMap) {
              for (name <- nameList) {
                for (i <- sL) {
                  if (sL.length == 4) {
                    if (sL.contains(day) && sL.contains(time) && sL.contains(classes) && sL.contains(name)) {
                      holderTwo = holderTwo.empty
                      finalOutput = List("AT " + day + " DURING " + time + " FOR " + classes + " WITH " + name)
                      return finalOutput
                    }
                  }
                  if (dayOW.contains(i) && day != i) {
                    d = day
                    holderTwo -= day
                  }
                  if (i.contains(":") && time != i) {
                    t = time
                    timeMap -= time
                  }
                  if ((for (i <- 0 until 900) yield i.toString).toList.contains(i.takeRight(3)) && classes != i) {
                    c = classes
                    classMap -= classes
                  }
                  if (!dayOW.contains(i) && !i.contains(":") && !(for (i <- 0 until 900) yield i.toString).toList.contains(i.takeRight(3))
                    && name != i) {
                    n = name
                    nameList -= name
                  }
                }
              }
            }
          }
        }
      println(holderTwo)
      println("just searched")
    }
    else {
      println(holderTwo)
      println("just searched")
    }
    output(sort(muToImmU()))
  }


  def PN(tOF:Boolean):Map[String ,List[String]] ={
    readCsv()
    if ( q1.isEmpty || q2.isEmpty ) {
      val result = classList.map(_.toUpperCase()).distinct
      for (i <- result) {
        q1.enqueue(i.toUpperCase())
      }
      q2 = q1.reverse
      PN(tOF)
    }
    else {
      if(tOF) {
        if (onView.isEmpty) {
          onView = q1.dequeue()
          println(onView)
          q1.enqueue(onView)
        }
        else {
          var hold = ""
          while (onView != hold) {
            hold = q1.dequeue()
            q1.enqueue(hold)
          }
          onView = q1.dequeue()
          q1.enqueue(onView)
        }
      }
      else{
        if (onView.isEmpty) {
          onView = q2.dequeue()
          q2.enqueue(onView)
        }
        else {
          var hold = ""
          while (onView != hold) {
            hold = q2.dequeue()
            q2.enqueue(hold)
          }
          onView = q2.dequeue()
          q2.enqueue(onView)
        }
      }
    }
    println("on " + onView)
    Map(onView -> search(onView))
  }


  def muToImmU():Map[String,Map[String,Map[String,List[String]]]]={
    val immMap = holderTwo.map { case (day, timeMap) => (day,
      timeMap.toMap.map { case (time, classMap) => time ->
        classMap.toMap.map { case (classes, nameList) => classes -> nameList.toList }})}
    println("mapchanged")
    immMap
  }


  def sort(x:Map[String,Map[String,Map[String,List[String]]]]):Map[String,Map[String,Map[String,List[String]]]]= {
    val sortedholder = x.map { case (k, v) => (k.replaceAll(k, dayNameToNumber(k)),
      v.map { case (k1, v1) => (k1.replace(":", ".").toDouble, v1) })}
    val sorted = collection.immutable.ListMap(sortedholder.toSeq.sortBy(_._1):_*).
      map {case (key, value) => (key , collection.immutable.ListMap(value.toSeq.sortBy(_._1):_*))}
    val newholder = sorted.map { case (k, v) => (k.toString.replace(k.toString, dayNumberToName(k)),
      v.map { case (k1, v1) => (k1.toString.replace(".", ":") + "0", v1) })
    }
    println("just sorted")
    newholder
  }


  def output(h:Map[String,Map[String,Map[String,List[String]]]]): List[String] = {
    var holderForFinalOutput:List[String] = List()
      for ((day, mapTime) <- h) {
        for ((time, mapClasses) <- mapTime) {
          for ((classes, listNames) <- mapClasses) {
            for (name <- listNames) {
              holderForFinalOutput = holderForFinalOutput ::: List("AT " + day + " DURING " + time + " FOR " + classes + " WITH " + name)
            }
          }
        }
      }
    println("sorted")
    finalOutput = holderForFinalOutput.reverse
    holderTwo = holderTwo.empty
    holderForFinalOutput
  }

}
