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
import javafx.scene.shape.TriangleMesh
import javafx.scene.shape.MeshView
import javafx.scene.shape.DrawMode
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
    val hull = new QuickHull3D() {
      build(points)
      triangulate
    }
    val vertices = hull.getVertices
    val vert3D = hull.getFaces.map(f => f.map(idx => new Point3D(vertices(idx).x.toFloat, vertices(idx).y.toFloat, vertices(idx).z.toFloat)))
    val l = getHull(hull).map(p => (p, Color.RED))
    showFigure(primaryStage, l, vert3D)
  }

  def getHull(hull: QuickHull3D): List[Point3D] = {
    val vertices = hull.getVertices
    hull.getFaces.foldLeft(List[Point3D]())((l, f) => f.foldLeft(l)((ll, ff) => ll ++ List(new Point3D(vertices(ff).x.toFloat, vertices(ff).y.toFloat, vertices(ff).z.toFloat))))
  }

  def getSphere(c: Color, radius: Double): Sphere = {
    new Sphere(radius) {
      setMaterial(new PhongMaterial() {
        setDiffuseColor(c)
        setSpecularColor(c)
      })
    }
  }

  def showFigure(primaryStage: Stage, vertices: List[(Point3D, Color)], triangles: Array[Array[Point3D]]) {
    val factor = 500f
    val parent = new Group() {
      setTranslateX(factor / 2)
      setTranslateY(factor / 2)
      setTranslateZ(0)
      setRotationAxis(new Point3D(1, 1, 1))
      Try(vertices.foreach(v => {
        val sphere = getSphere(v._2, 0.01f * factor)
        sphere.setTranslateX(factor / 2 + v._1.getX() * factor)
        sphere.setTranslateY(factor / 2 + v._1.getY() * factor)
        sphere.setTranslateZ(factor / 2 + v._1.getZ() * factor)
        getChildren.add(sphere)
      }))
      Try(triangles.foreach(p => {
        val pp = p.map(x => Array(x.getX.asInstanceOf[Float], x.getY.asInstanceOf[Float], x.getZ.asInstanceOf[Float])).flatten
        val mesh = new TriangleMesh {
          getTexCoords.addAll(
            0.5f, 0.5f, // t0 (it0 = 0)
            0.0f, 1.0f, // t1 (it1 = 1)
            1.0f, 1.0f // t2 (it2 = 2)
            )
          getFaces.addAll(
            0, 0, 2, 2, 1, 1, // iv0, it0, iv2, it2, iv1, it1 (front face)
            0, 0, 1, 1, 2, 2 // iv0, it0, iv1, it1, iv2, it2 back face
            )
          getPoints.addAll(
            pp(0) * factor,
            pp(1) * factor,
            pp(2) * factor,
            pp(3) * factor,
            pp(4) * factor,
            pp(5) * factor,
            pp(6) * factor,
            pp(7) * factor,
            pp(8) * factor)
        }
        val triangle = new MeshView(mesh) {
          setDrawMode(DrawMode.FILL);
          setMaterial(new PhongMaterial() {
            setDiffuseColor(Color.BISQUE)
            setSpecularColor(Color.LIGHTBLUE)
          })
          setTranslateX(factor / 2)
          setTranslateY(factor / 2)
          setTranslateZ(factor / 2)
        }
        getChildren.add(triangle)
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
