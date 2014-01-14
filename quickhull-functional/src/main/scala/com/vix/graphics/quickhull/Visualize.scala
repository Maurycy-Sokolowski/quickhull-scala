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
    val points = Array.ofDim[Point3d](1000)
    for (i <- 0 until points.length) {
      val z = Math.random * 2 - 1
      val t = Math.random * 2 * math.Pi
      val w = Math.sqrt(1 - z * z)
      points(i) = new Point3d(w * math.cos(t), w * math.sin(t), z)
    }
    val hull = new QuickHull3D()
    hull.build(points)
    val l = getHull(hull)
    showFigure(primaryStage, l, null, null, null, null, null)
  }

  def getHull(hull: QuickHull3D): List[Point3D] = {
    val vertices = hull.getVertices
    val faceIndices = hull.getFaces
    var pts = new ListBuffer[Point3D]()
    for (i <- 0 until faceIndices.length; k <- 0 until faceIndices(i).length) {
      val idx = faceIndices(i)(k)
      val p = new Point3D(vertices(idx).x.toFloat, vertices(idx).y.toFloat, vertices(idx).z.toFloat)
      pts += p
    }
    pts.toList
  }

  def addCamera(scene: Scene): PerspectiveCamera = {
    val perspectiveCamera = new PerspectiveCamera(false)
    scene.setCamera(perspectiveCamera)
    perspectiveCamera
  }

  def getSphere(c: Color, radius: Double): Sphere = {

    val sphereMaterial = new PhongMaterial()

    if (c == null) {
      sphereMaterial.setDiffuseColor(Color.BISQUE)
      sphereMaterial.setSpecularColor(Color.LIGHTBLUE)
    } else {
      sphereMaterial.setDiffuseColor(c)
      sphereMaterial.setSpecularColor(c)

    }
    val s = new Sphere(radius)
    s.setMaterial(sphereMaterial)
    return s
  }

  def showFigure(primaryStage: Stage,
    vertices: List[Point3D],
    vrtxColors: Array[Color],
    lines: List[Cylinder],
    lineColors: Array[Color],
    points: List[Point3D],
    ptsColors: Array[Color]) {

    val factor = 500.

    val parent = new Group()
    parent.setTranslateX(factor / 2)
    parent.setTranslateY(factor / 2)
    parent.setTranslateZ(0)

    parent.setRotationAxis(new Point3D(1, 1, 1))

    if (lines != null) {
      var i = 0
      for (pla <- lines) {
        val colorsArray = lineColors(i)
        i += 1
        parent.getChildren().add(pla)
      }
    }

    if (vertices != null) {

      var idx = 0
      for (p <- vertices) {
        val sphere = getSphere(if (vrtxColors == null) Color.RED else vrtxColors(idx), 0.02f * factor)
        idx += 1
        sphere.setTranslateX(factor / 2 + p.getX() * factor)
        sphere.setTranslateY(factor / 2 + p.getY() * factor)
        sphere.setTranslateZ(factor / 2 + p.getZ() * factor)
        parent.getChildren().add(sphere)
      }
    }

    if (points != null) {
      var i = 0
      for (p <- points) {
        val sphere = getSphere(if (ptsColors == null) Color.RED else ptsColors(i), 0.1f)
        i += 1
        sphere.setTranslateX(p.getX())
        sphere.setTranslateY(p.getY())
        sphere.setTranslateZ(p.getZ())
        parent.getChildren().add(sphere)
      }
    }

    val root = new Group(parent)
    val scene = new Scene(root, 1024, 768, true)

    scene.setOnMousePressed(mkEventHandler((event: MouseEvent) => {
      anchorX = event.getSceneX()
      anchorY = event.getSceneY()
      anchorAngle = parent.getRotate()
    }))

    scene.setOnMouseDragged(mkEventHandler((event: MouseEvent) => {
      parent.setRotate(anchorAngle + anchorX - event.getSceneX())
    }))

    val pointLight = new PointLight(Color.ANTIQUEWHITE)
    pointLight.setTranslateX(factor)
    pointLight.setTranslateY(-factor)
    pointLight.setTranslateZ(-factor)

    root.getChildren().add(pointLight)

    addCamera(scene)
    primaryStage.setScene(scene)
    primaryStage.show()
  }
}
