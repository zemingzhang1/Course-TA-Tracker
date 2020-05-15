package GUIclient
import java.io.FileWriter

import akka.actor._
import model.TA_App

import scala.io.{BufferedSource, Source}

case class PN(tf:Boolean)
case class Search(s:String)


class Act extends Actor {
  override def receive: Receive = {

    case Search(s:String) =>
      val fw = new FileWriter("searchHistory.csv", true)
      fw.write("\n" + s)
      fw.close()

    case "SEARCHHISTORY" =>
      TA_App.history()

    case "CLEARSEARCHHISTORY" =>
      val fw = new FileWriter("searchHistory.csv")
      fw.write("HISTORY---------------------------------CLEARED")
      fw.close()
  }
}