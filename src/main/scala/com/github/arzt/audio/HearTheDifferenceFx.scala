package com.github.arzt.audio

import javafx.application.{Application, Platform}
import javafx.scene.Scene
import javafx.scene.control.Button
import javafx.scene.layout.{BorderPane, GridPane, VBox}
import javafx.stage.Stage
import scala.jdk.CollectionConverters._

import java.util.concurrent.{Executors, TimeUnit}

object HearTheDifferenceFx {
  def main(args: Array[String]): Unit =
    Application.launch(classOf[HearTheDifferenceFx], args: _*)
}

class HearTheDifferenceFx extends Application {
  def start(primaryStage: Stage): Unit =
    val paras = getParameters().getUnnamed().asScala.toArray[String]
    primaryStage.setTitle("Hear the difference")
    val pane = new VBox(10)
    val s = new Scene(pane)
    primaryStage.setScene(s)
    primaryStage.show()
    Platform.setImplicitExit(true)
    val htd = HearTheDifference(paras)
    val executor = Executors.newScheduledThreadPool(1);
    val future = executor.schedule(htd, 0, TimeUnit.SECONDS)
    val buttons = paras.zipWithIndex.map {
      case (b, i) =>
        val button = new Button(i.toString)
        button.setOnAction(b => htd.setIndex(i))
        button
    }
    pane.getChildren.addAll(buttons.toList.asJavaCollection)
}
