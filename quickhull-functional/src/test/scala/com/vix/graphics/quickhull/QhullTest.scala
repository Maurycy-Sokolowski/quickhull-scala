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

object QhullTest {

  var coords = Array()

  var faces: Array[Array[Int]] = Array()

  var hull = new QuickHull3D()
  val tester = new QuickHull3DTest()
  hull = new QuickHull3D()

  def main(args: Array[String]) {
    for (i <- 0 until 100) {
      var pnts = tester.randomCubedPoints(100, 1.0, 0.5)
      hull.setFromQhull(pnts, pnts.length / 3, false)
      pnts = tester.addDegeneracy(QuickHull3DTest.VERTEX_DEGENERACY, pnts, hull)
      hull.setFromQhull(pnts, pnts.length / 3, true)
      if (!hull.check(System.out)) {
        println("failed for qhull triangulated")
      }
      hull.setFromQhull(pnts, pnts.length / 3, false)
      if (!hull.check(System.out)) {
        println("failed for qhull regular")
      }
      hull.build(pnts, pnts.length / 3)
      hull.triangulate()
      if (!hull.check(System.out)) {
        println("failed for QuickHull3D triangulated")
      }
      hull.build(pnts, pnts.length / 3)
      if (!hull.check(System.out)) {
        println("failed for QuickHull3D regular")
      }
    }
  }
}
