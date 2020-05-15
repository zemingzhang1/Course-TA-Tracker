package GUIclient

import akka.actor._
import com.corundumstudio.socketio.listener.{ConnectListener, DataListener, DisconnectListener}
import com.corundumstudio.socketio.{AckRequest, Configuration, SocketIOClient, SocketIOServer}
import model.TA_App
import play.api.libs.json.Json


class Server() {
  val actorSystem: ActorSystem = ActorSystem("My-Actor-system")
  val Actor: ActorRef = actorSystem.actorOf(Props(classOf[Act]))
  val Actor1: ActorRef = actorSystem.actorOf(Props(classOf[Act]))

  val config: Configuration = new Configuration {
    setHostname("localhost")
    setPort(8010)
  }

  val server: SocketIOServer = new SocketIOServer(config)

  server.addConnectListener(new ConnectionListener())
  server.addDisconnectListener(new DisconnectionListener())
  server.addEventListener("submit", classOf[String], new SubmitListener(this))
  server.addEventListener("trueOfalse", classOf[String], new ButtonListener(this))
  server.addEventListener("search", classOf[String], new SearchListener(this))
  server.start()

}

object Server {
  def main(args: Array[String]): Unit = {
    new Server()
  }
}

class ConnectionListener() extends ConnectListener {
  override def onConnect(client: SocketIOClient): Unit = {
    println("Connected: " + client)
  }
}


class DisconnectionListener() extends DisconnectListener {
  override def onDisconnect(socket: SocketIOClient): Unit = {
    println("Disconnected: " + socket)
  }
}


class SubmitListener(server: Server) extends DataListener[String] {
  override def onData(socket: SocketIOClient, data: String, ackRequest: AckRequest): Unit = {
    println(data)
    val jsonValue = Json.parse(data)
    val tA = (jsonValue \ "TA").as[String]
    val oH = (jsonValue \ "OH").as[String]
    val classes = (jsonValue \ "CLASS").as[String]
    socket.sendEvent("output", Json.stringify(Json.toJson(TA_App.submitCsv(tA, oH, classes))))
  }
}

class ButtonListener(server: Server) extends DataListener[String] {
  override def onData(socket: SocketIOClient, data: String, ackRequest: AckRequest): Unit = {
    val jsonValue = Json.parse(data)
    val tOF = (jsonValue \ "tOf").as[Boolean]
    for((key,value)<- TA_App.PN(tOF)) {
      socket.sendEvent("classes-output", Json.stringify(Json.toJson(Map("class" -> Json.toJson(key), "output" -> Json.toJson(value)))))
    }
    server.Actor ! Search(TA_App.onView)
  }
}

class SearchListener(server: Server) extends DataListener[String] {
  override def onData(socket: SocketIOClient, data: String, ackSender: AckRequest): Unit = {
    println(data)
    val jsonValue = Json.parse(data)
    val search = (jsonValue \ "search").as[String]
    server.Actor ! Search(search)
    server.Actor1 ! search
    socket.sendEvent("output", Json.stringify(Json.toJson(TA_App.search(search))))
  }
}
