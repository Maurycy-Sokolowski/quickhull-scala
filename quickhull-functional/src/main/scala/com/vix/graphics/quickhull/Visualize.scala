/**
 * QuickHull3D in Scala
 *
 * @author maurycy sokolowski
 *
 * Credit to John Loyd - 2004
 *
 * This is a 3D implementation of QuickHull based on the original paper by Barber, Dobkin, and Huhdanpaa and the C implementation known as qhull.
 * The algorithm has O(n log(n)) complexity, works with double precision numbers, is fairly robust with respect to degenerate situations, and allows the merging of co-planar faces.
 */
package com.vix.graphics.quickhull

import scala.collection.mutable.ListBuffer
import javafx.scene.Scene
import javafx.application.Application
import javafx.geometry.Point3D
import javafx.scene.Group
import javafx.scene.PerspectiveCamera
import javafx.scene.Scene
import javafx.scene.input.MouseEvent
import javafx.scene.paint.Color
import javafx.stage.Stage
import javafx.scene.paint.PhongMaterial
import javafx.scene.shape.Sphere
import javafx.scene.paint.Material
import javafx.scene.PointLight
import javafx.scene.shape.Cylinder
import javafx.event.Event
import javafx.event.EventHandler
import scala.util.{ Try, Success, Failure }

object Visualize {

  /**
   * Run for a simple demonstration of QuickHull3D.
   */
  def main(args: Array[String]) {
    System.setProperty("prism.dirtyopts", "false")
    Application.launch(classOf[Visualize], args: _*)
  }
}

trait JfxUtils {
  def mkEventHandler[E <: Event](f: E => Unit) = new EventHandler[E] { def handle(e: E) = f(e) }
}

class Visualize extends Application with JfxUtils {
  var anchorX: Double = _
  var anchorY: Double = _
  var anchorAngle: Double = _

  override def start(primaryStage: Stage) {
    primaryStage.setTitle("Quickhull3D")
    val points = Array.fill(1000)({
      val z = Math.random * 2 - 1
      val t = Math.random * 2 * math.Pi
      val w = Math.sqrt(1 - z * z)
      new Point3d(w * math.cos(t), w * math.sin(t), z)
    })
    val hull = new QuickHull3D() { build(points) }
    val l = getHull(hull).map(p => (p, Color.RED))
    showFigure(primaryStage, l, null, null)
  }

  def getHull(hull: QuickHull3D): List[Point3D] = {
    val vertices = hull.getVertices
    hull.getFaces.foldLeft(List[Point3D]())((l, f) => f.foldLeft(l)((ll, ff) => ll ++ List(new Point3D(vertices(ff).x.toFloat, vertices(ff).y.toFloat, vertices(ff).z.toFloat))))
  }

  def getSphere(c: Color, radius: Double): Sphere = {
    new Sphere(radius) {
      setMaterial(new PhongMaterial() {
        if (c == null) {
          setDiffuseColor(Color.BISQUE)
          setSpecularColor(Color.LIGHTBLUE)
        } else {
          setDiffuseColor(c)
          setSpecularColor(c)
        }
      })
    }
  }

  def showFigure(primaryStage: Stage, vertices: List[(Point3D, Color)], lines: List[(Cylinder, Color)], points: List[(Point3D, Color)]) {
    val factor = 500.0
    val parent = new Group() {
      setTranslateX(factor / 2)
      setTranslateY(factor / 2)
      setTranslateZ(0)
      setRotationAxis(new Point3D(1, 1, 1))
      Try(lines.foreach(ll => getChildren().add(ll._1)))
      Try(vertices.foreach(v => {
        val sphere = getSphere(v._2, 0.01f * factor)
        sphere.setTranslateX(factor / 2 + v._1.getX() * factor)
        sphere.setTranslateY(factor / 2 + v._1.getY() * factor)
        sphere.setTranslateZ(factor / 2 + v._1.getZ() * factor)
        getChildren().add(sphere)
      }))
      Try(points.foreach(p => {
        val sphere = getSphere(p._2, 0.1f)
        sphere.setTranslateX(p._1.getX())
        sphere.setTranslateY(p._1.getY())
        sphere.setTranslateZ(p._1.getZ())
        getChildren().add(sphere)
      }))
    }
    val root = new Group(parent) {
      getChildren().add(new PointLight(Color.ANTIQUEWHITE) {
        setTranslateX(factor)
        setTranslateY(-factor)
        setTranslateZ(-factor)
      })
    }
    primaryStage.setScene(new Scene(root, 1024, 768, true) {
      setOnMousePressed(mkEventHandler((event: MouseEvent) => {
        anchorX = event.getSceneX()
        anchorY = event.getSceneY()
        anchorAngle = parent.getRotate()
      }))
      setOnMouseDragged(mkEventHandler((event: MouseEvent) => {
        parent.setRotate(anchorAngle + anchorX - event.getSceneX())
      }))
      setCamera(new PerspectiveCamera(false))
    })
    primaryStage.show()
  }
}
