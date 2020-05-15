package GUIclient

import io.socket.client.{IO, Socket}
import io.socket.emitter.Emitter
import javafx.application.Platform
import javafx.event.ActionEvent
import play.api.libs.json._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, ListView, TextField}
import scalafx.scene.layout.GridPane


class OutputFromServer() extends Emitter.Listener {
  override def call(objects: Object*): Unit = {
    Platform.runLater(( ) => {
      val message = objects.apply(0).toString
      println(objects.apply(0).toString)
      GUI.output.getItems.clear()
      val newMessage = Json.parse(message).as[List[JsValue]]
      for (i <- newMessage) {
        GUI.output.getItems.addAll(i.toString())
      }
    })
  }
}


class ClassesOutput() extends Emitter.Listener {
  override def call(objects: Object*): Unit = {
    Platform.runLater(() => {
      val message = objects.apply(0).toString
      val newMessage = Json.parse(message)
      GUI.classDisplay.text.value = ""
      val classes = (newMessage \ "class").as[String]
      val output = (newMessage \ "output").as[List[JsValue]]
      GUI.classDisplay.text.value = classes
      GUI.output.getItems.clear()
      for (i <- output) {
        GUI.output.getItems.addAll(i.toString())
      }
    })
  }
}


object GUI extends JFXApp {

  var socket: Socket = IO.socket("http://localhost:8010/")
  socket.on("output", new OutputFromServer)
  socket.on("classes-output", new  ClassesOutput)
  socket.connect()

    val l1 = new Label("TA Names")
    val l2 = new Label("Days Of The Week")
    val l3 = new Label("MilitaryTime and[,] ClassLevels")
    val l4 = new Label("Class Display")
    val l5 = new Label("Name,Days...In any order, Only One Element")
    val l6 = new Label("Output Of TA Office Hour ")

    val classDisplay = new Label()
    val taInput = new TextField
    val oHInput = new TextField
    val classInput = new TextField
    val searchInput = new TextField
    val output = new ListView[String]()

    class GUIButtonSubmit() extends Button {
      minHeight = 30
      minWidth = 400
      onAction = (_: ActionEvent) => {
        socket.emit("submit", Json.toJson(Map("TA" -> Json.toJson(taInput.text.value.toUpperCase),
          "OH" -> Json.toJson(oHInput.text.value.toUpperCase),
          "CLASS" -> Json.toJson(classInput.text.value.toUpperCase)))
        )
        taInput.clear()
        oHInput.clear()
        classInput.clear()
      }
    }

    class GUIButtonPN(val answer: Boolean) extends Button {
      minHeight = 30
      minWidth = 200
      onAction = (_: ActionEvent) => {
        socket.emit("trueOfalse", Json.toJson(Map("tOf" -> Json.toJson(answer))))
    }
    }

    class GUIButtonSearch() extends Button {
      minHeight = 30
      minWidth = 200
      onAction = (_: ActionEvent) => {
        socket.emit("search", Json.toJson(Map("search" -> Json.toJson(searchInput.text.value.toUpperCase))))
      }
    }

    val submit: Button = new GUIButtonSubmit() {
      text = "Submit"
    }

    val previous: Button = new GUIButtonPN(false) {
      text = "Previous"
    }

    val next: Button = new GUIButtonPN(true) {
      text = "Next"
    }

    val search: Button = new GUIButtonSearch() {
      text = "Search"
    }

    stage = new PrimaryStage {
      title = "TA Helper"
      scene = new Scene() {
        content = List(
          new GridPane {
            hgap = 0
            vgap = 0.0
            add(l1, 1, 0, 1, 1)
            add(taInput, 1, 1, 1, 1)
            add(l2, 2, 0, 1, 1)
            add(oHInput, 2, 1, 1, 1)
            add(l3, 3, 0, 1, 1)
            add(classInput, 3, 1, 1, 1)
            add(submit, 4, 1, 2, 1)

            add(previous, 1, 3, 1, 1)
            add(l4, 2, 2, 1, 1)
            add(classDisplay, 2, 3, 1, 1)
            add(next, 3, 3, 1, 1)

            add(l5, 4, 2, 1, 1)
            add(searchInput, 4, 3, 1, 1)
            add(search, 5, 3, 1, 1)

            add(l6, 1, 4, 1, 1)
            add(output, 1, 5, 5, 1)
          })
      }
    }
}

