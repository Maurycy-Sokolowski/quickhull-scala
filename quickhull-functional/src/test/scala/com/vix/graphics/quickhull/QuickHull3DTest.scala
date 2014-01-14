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

import QuickHull3DTest._

object QuickHull3DTest {

  val DOUBLE_PREC = 2.2204460492503131e-16
  val NO_DEGENERACY = 0
  val EDGE_DEGENERACY = 1
  val VERTEX_DEGENERACY = 2

  var triangulate = false
  var doTesting = true
  var doTiming = true
  var debugEnable = false
  var testRotation = true
  var degeneracyTest = VERTEX_DEGENERACY
  var epsScale = 2.0

  /**
   * Runs a set of tests on the QuickHull3D class, and prints
   * <code>Passed</code> if all is well. Otherwise, an error message and stack
   * trace are printed.
   *
   * <p>
   * If the option <code>-timing</code> is supplied, then timing information
   * is produced instead.
   */
  def main(args: Array[String]) {
    val tester = new QuickHull3DTest()
    for (i <- 0 until args.length) {
      if (args(i) == "-timing") {
        doTiming = true
        doTesting = false
      } else {
        println("Usage: java quickhull3d.QuickHull3DTest [-timing]")
        System.exit(1)
      }
    }
    if (doTesting) {
      tester.explicitAndRandomTests()
    }
    if (doTiming) {
      tester.timingTests()
    }
  }
}

/**
 * Testing class for QuickHull3D. Running the command
 *
 * <pre>
 *   java quickhull3d.QuickHull3DTest
 * </pre>
 *
 * will cause QuickHull3D to be tested on a number of randomly choosen input
 * sets, with degenerate points added near the edges and vertics of the convex
 * hull.
 *
 * <p>
 * The command
 *
 * <pre>
 *   java quickhull3d.QuickHull3DTest -timing
 * </pre>
 *
 * will cause timing information to be produced instead.
 *
 * @author John E. Lloyd, Fall 2004
 */
class QuickHull3DTest {

  /**
   * Returns true if two face index sets are equal, modulo a cyclical
   * permuation.
   *
   * @param indices1
   *            index set for first face
   * @param indices2
   *            index set for second face
   * @return true if the index sets are equivalent
   */
  def faceIndicesEqual(indices1: Array[Int], indices2: Array[Int]): Boolean = {
    if (indices1.length != indices2.length) {
      return false
    }
    val len = indices1.length
    var j: Int = 0
    j = 0
    while (j < len) {
      if (indices1(0) == indices2(j)) {
        //break
      }
      j += 1
    }
    if (j == len) {
      return false
    }
    for (i <- 1 until len if indices1(i) != indices2((j + i) % len)) {
      return false
    }
    true
  }

  /**
   * Returns the coordinates for <code>num</code> points whose x, y, and z
   * values are randomly chosen within a given range.
   *
   * @param num
   *            number of points to produce
   * @param range
   *            coordinate values will lie between -range and range
   * @return array of coordinate values
   */
  def randomPoints(num: Int, range: Double): Array[Double] = {
    val coords = Array.ofDim[Double](num * 3)
    for (i <- 0 until num; k <- 0 until 3) {
      coords(i * 3 + k) = 2 * range * (math.random - 0.5)
    }
    coords
  }

  private def randomlyPerturb(pnt: Point3d, tol: Double) {
    pnt.x += tol * (math.random - 0.5)
    pnt.y += tol * (math.random - 0.5)
    pnt.z += tol * (math.random - 0.5)
  }

  /**
   * Returns the coordinates for <code>num</code> randomly chosen points which
   * are degenerate which respect to the specified dimensionality.
   *
   * @param num
   *            number of points to produce
   * @param dimen
   *            dimensionality of degeneracy: 0 = coincident, 1 = colinear, 2
   *            = coplaner.
   * @return array of coordinate values
   */
  def randomDegeneratePoints(num: Int, dimen: Int): Array[Double] = {
    val coords = Array.ofDim[Double](num * 3)
    val pnt = new Point3d()
    val base = new Point3d()
    base.setRandom(-1, 1)
    val tol = DOUBLE_PREC
    if (dimen == 0) {
      for (i <- 0 until num) {
        pnt.set(base)
        randomlyPerturb(pnt, tol)
        coords(i * 3 + 0) = pnt.x
        coords(i * 3 + 1) = pnt.y
        coords(i * 3 + 2) = pnt.z
      }
    } else if (dimen == 1) {
      val u = new Vector3d()
      u.setRandom(-1, 1)
      u.normalize()
      for (i <- 0 until num) {
        val a = 2 * (math.random - 0.5)
        pnt.scale(a, u)
        pnt.add(base)
        randomlyPerturb(pnt, tol)
        coords(i * 3 + 0) = pnt.x
        coords(i * 3 + 1) = pnt.y
        coords(i * 3 + 2) = pnt.z
      }
    } else {
      val nrm = new Vector3d()
      nrm.setRandom(-1, 1)
      nrm.normalize()
      for (i <- 0 until num) {
        val perp = new Vector3d()
        pnt.setRandom(-1, 1)
        perp.scale(pnt.dot(nrm), nrm)
        pnt.sub(perp)
        pnt.add(base)
        randomlyPerturb(pnt, tol)
        coords(i * 3 + 0) = pnt.x
        coords(i * 3 + 1) = pnt.y
        coords(i * 3 + 2) = pnt.z
      }
    }
    coords
  }

  /**
   * Returns the coordinates for <code>num</code> points whose x, y, and z
   * values are randomly chosen to lie within a sphere.
   *
   * @param num
   *            number of points to produce
   * @param radius
   *            radius of the sphere
   * @return array of coordinate values
   */
  def randomSphericalPoints(num: Int, radius: Double): Array[Double] = {
    val coords = Array.ofDim[Double](num * 3)
    val pnt = new Point3d()
    var i = 0
    while (i < num) {
      pnt.setRandom(-radius, radius)
      if (pnt.norm() <= radius) {
        coords(i * 3 + 0) = pnt.x
        coords(i * 3 + 1) = pnt.y
        coords(i * 3 + 2) = pnt.z
        i += 1
      }
    }
    coords
  }

  /**
   * Returns the coordinates for <code>num</code> points whose x, y, and z
   * values are each randomly chosen to lie within a specified range, and then
   * clipped to a maximum absolute value. This means a large number of points
   * may lie on the surface of cube, which is useful for creating degenerate
   * convex hull situations.
   *
   * @param num
   *            number of points to produce
   * @param range
   *            coordinate values will lie between -range and range, before
   *            clipping
   * @param max
   *            maximum absolute value to which the coordinates are clipped
   * @return array of coordinate values
   */
  def randomCubedPoints(num: Int, range: Double, max: Double): Array[Double] = {
    val coords = Array.ofDim[Double](num * 3)
    for (i <- 0 until num; k <- 0 until 3) {
      var x = 2 * range * (math.random - 0.5)
      if (x > max) {
        x = max
      } else if (x < -max) {
        x = -max
      }
      coords(i * 3 + k) = x
    }
    coords
  }

  private def shuffleCoords(coords: Array[Double]): Array[Double] = {
    val num = coords.length / 3
    for (i <- 0 until num) {
      val i1 = (math.random * num).asInstanceOf[Int]
      val i2 = (math.random * num).asInstanceOf[Int]
      for (k <- 0 until 3) {
        val tmp = coords(i1 * 3 + k)
        coords(i1 * 3 + k) = coords(i2 * 3 + k)
        coords(i2 * 3 + k) = tmp
      }
    }
    coords
  }

  /**
   * Returns randomly shuffled coordinates for points on a three-dimensional
   * grid, with a presecribed width between each point.
   *
   * @param gridSize
   *            number of points in each direction, so that the total number
   *            of points produced is the cube of gridSize.
   * @param width
   *            distance between each point along a particular direction
   * @return array of coordinate values
   */
  def randomGridPoints(gridSize: Int, width: Double): Array[Double] = {
    val num = gridSize * gridSize * gridSize
    val coords = Array.ofDim[Double](num * 3)
    var idx = 0
    for (i <- 0 until gridSize; j <- 0 until gridSize; k <- 0 until gridSize) {
      coords(idx * 3 + 0) = (i / (gridSize - 1).toDouble - 0.5) * width
      coords(idx * 3 + 1) = (j / (gridSize - 1).toDouble - 0.5) * width
      coords(idx * 3 + 2) = (k / (gridSize - 1).toDouble - 0.5) * width
      idx += 1
    }
    shuffleCoords(coords)
    coords
  }

  def explicitFaceCheck(hull: QuickHull3D, checkFaces: Array[Array[Int]]) {
    val faceIndices = hull.getFaces
    if (faceIndices.length != checkFaces.length) {
      throw new Exception("Error: " + faceIndices.length + " faces vs. " + checkFaces.length)
    }
    val vtxIndices = hull.getVertexPointIndices
    for (j <- 0 until faceIndices.length) {
      val idxs = faceIndices(j)
      for (k <- 0 until idxs.length) {
        idxs(k) = vtxIndices(idxs(k))
      }
    }
    for (i <- 0 until checkFaces.length) {
      val cf = checkFaces(i)
      var j: Int = 0
      j = 0
      while (j < faceIndices.length) {
        if (faceIndices(j) != null) {
          if (faceIndicesEqual(cf, faceIndices(j))) {
            faceIndices(j) = null
            //break
          }
        }
        j += 1
      }
      if (j == faceIndices.length) {
        var s = ""
        for (k <- 0 until cf.length) {
          s += cf(k) + " "
        }
        throw new Exception("Error: face " + s + " not found")
      }
    }
  }

  var cnt: Int = 0

  def singleTest(coords: Array[Double], checkFaces: Array[Array[Int]]) {
    val hull = new QuickHull3D()
    hull.setDebug(debugEnable)
    hull.build(coords, coords.length / 3)
    if (triangulate) {
      hull.triangulate()
    }
    if (!hull.check(System.out)) {
      (new Throwable()).printStackTrace()
      System.exit(1)
    }
    if (checkFaces != null) {
      explicitFaceCheck(hull, checkFaces)
    }
    if (degeneracyTest != NO_DEGENERACY) {
      degenerateTest(hull, coords)
    }
  }

  def addDegeneracy(`type`: Int, coords: Array[Double], hull: QuickHull3D): Array[Double] = {
    var numv = coords.length / 3
    val faces = hull.getFaces
    val coordsx = Array.ofDim[Double](coords.length + faces.length * 3)
    for (i <- 0 until coords.length) {
      coordsx(i) = coords(i)
    }
    val lam = Array.ofDim[Double](3)
    val eps = hull.getDistanceTolerance
    for (i <- 0 until faces.length) {
      lam(0) = math.random
      lam(1) = 1 - lam(0)
      lam(2) = 0.0
      if (`type` == VERTEX_DEGENERACY && (i % 2 == 0)) {
        lam(0) = 1.0
        lam(1) = 0
        lam(2) = 0
      }
      for (j <- 0 until 3) {
        val vtxi = faces(i)(j)
        for (k <- 0 until 3) {
          coordsx(numv * 3 + k) += lam(j) * coords(vtxi * 3 + k) + epsScale * eps * (math.random - 0.5)
        }
      }
      numv += 1
    }
    shuffleCoords(coordsx)
    coordsx
  }

  def degenerateTest(hull: QuickHull3D, coords: Array[Double]) {
    val coordsx = addDegeneracy(degeneracyTest, coords, hull)
    val xhull = new QuickHull3D()
    xhull.setDebug(debugEnable)
    try {
      xhull.build(coordsx, coordsx.length / 3)
      if (triangulate) {
        xhull.triangulate()
      }
    } catch {
      case e: Exception => for (i <- 0 until coordsx.length / 3) {
        println(coordsx(i * 3 + 0) + ", " + coordsx(i * 3 + 1) + ", " +
          coordsx(i * 3 + 2) +
          ", ")
      }
    }
    if (!xhull.check(System.out)) {
      (new Throwable()).printStackTrace()
      System.exit(1)
    }
  }

  def rotateCoords(res: Array[Double],
    xyz: Array[Double],
    roll: Double,
    pitch: Double,
    yaw: Double) {
    val sroll = Math.sin(roll)
    val croll = Math.cos(roll)
    val spitch = Math.sin(pitch)
    val cpitch = Math.cos(pitch)
    val syaw = Math.sin(yaw)
    val cyaw = Math.cos(yaw)
    val m00 = croll * cpitch
    val m10 = sroll * cpitch
    val m20 = -spitch
    val m01 = croll * spitch * syaw - sroll * cyaw
    val m11 = sroll * spitch * syaw + croll * cyaw
    val m21 = cpitch * syaw
    val m02 = croll * spitch * cyaw + sroll * syaw
    val m12 = sroll * spitch * cyaw - croll * syaw
    val m22 = cpitch * cyaw
    var i = 0
    while (i < xyz.length - 2) {
      res(i + 0) = m00 * xyz(i + 0) + m01 * xyz(i + 1) + m02 * xyz(i + 2)
      res(i + 1) = m10 * xyz(i + 0) + m11 * xyz(i + 1) + m12 * xyz(i + 2)
      res(i + 2) = m20 * xyz(i + 0) + m21 * xyz(i + 1) + m22 * xyz(i + 2)
      i += 3
    }
  }

  def printCoords(coords: Array[Double]) {
    val nump = coords.length / 3
    for (i <- 0 until nump) {
      println(coords(i * 3 + 0) + ", " + coords(i * 3 + 1) + ", " +
        coords(i * 3 + 2) +
        ", ")
    }
  }

  def testException(coords: Array[Double], msg: String) {
    val hull = new QuickHull3D()
    var ex: Exception = null
    try {
      hull.build(coords)
    } catch {
      case e: Exception => ex = e
    }
    if (ex == null) {
      println("Expected exception " + msg)
      println("Got no exception")
      println("Input pnts:")
      printCoords(coords)
      System.exit(1)
    } else if (ex.getMessage == null || ex.getMessage != msg) {
      println("Expected exception " + msg)
      println("Got exception " + ex.getMessage)
      println("Input pnts:")
      printCoords(coords)
      System.exit(1)
    }
  }

  def test(coords: Array[Double], checkFaces: Array[Array[Int]]) {
    val rpyList = Array(Array(0, 0, 0), Array(10, 20, 30), Array(-45, 60, 91), Array(125, 67, 81))
    val xcoords = Array.ofDim[Double](coords.length)
    singleTest(coords, checkFaces)
    if (testRotation) {
      for (i <- 0 until rpyList.length) {
        val rpy = rpyList(i)
        rotateCoords(xcoords, coords, Math.toRadians(rpy(0)), Math.toRadians(rpy(1)), Math.toRadians(rpy(2)))
        singleTest(xcoords, checkFaces)
      }
    }
  }

  /**
   * Runs a set of explicit and random tests on QuickHull3D, and prints
   * <code>Passed</code> to System.out if all is well.
   */
  def explicitAndRandomTests() {
    try {
      var coords: Array[Double] = null
      println("Testing degenerate input ...")
      for (dimen <- 0 until 3; i <- 0 until 10) {
        coords = randomDegeneratePoints(10, dimen)
        if (dimen == 0) {
          testException(coords, "Input points appear to be coincident")
        } else if (dimen == 1) {
          testException(coords, "Input points appear to be colinear")
        } else if (dimen == 2) {
          testException(coords, "Input points appear to be coplanar")
        }
      }
      println("Explicit tests ...")
      coords = Array(21, 0, 0, 0, 21, 0, 0, 0, 0, 18, 2, 6, 1, 18, 5, 2, 1, 3, 14, 3, 10, 4, 14, 14, 3, 4, 10, 10, 6, 12, 5, 10, 15)
      test(coords, null)
      coords = Array(0.0, 0.0, 0.0, 21.0, 0.0, 0.0, 0.0, 21.0, 0.0, 2.0, 1.0, 2.0, 17.0, 2.0, 3.0, 1.0, 19.0, 6.0, 4.0, 3.0, 5.0, 13.0, 4.0, 5.0, 3.0, 15.0, 8.0, 6.0, 5.0, 6.0, 9.0, 6.0, 11.0)
      test(coords, null)
      println("Testing 20 to 200 random points ...")
      var n = 20
      while (n < 200) {
        for (i <- 0 until 10) {
          coords = randomPoints(n, 1.0)
          test(coords, null)
        }
        n += 10
      }
      println("Testing 20 to 200 random points in a sphere ...")
      n = 20
      while (n < 200) {
        for (i <- 0 until 10) {
          coords = randomSphericalPoints(n, 1.0)
          test(coords, null)
        }
        n += 10
      }
      println("Testing 20 to 200 random points clipped to a cube ...")
      n = 20
      while (n < 200) {
        for (i <- 0 until 10) {
          coords = randomCubedPoints(n, 1.0, 0.5)
          test(coords, null)
        }
        n += 10
      }
      println("Testing 8 to 1000 randomly shuffled points on a grid ...")
      n = 2
      while (n <= 10) {
        for (i <- 0 until 10) {
          coords = randomGridPoints(n, 4.0)
          test(coords, null)
        }
        n += 1
      }
    } catch {
      case e: Exception => {
        e.printStackTrace()
        System.exit(1)
      }
    }
    println("\nPassed\n")
  }

  /**
   * Runs timing tests on QuickHull3D, and prints the results to System.out.
   */
  def timingTests() {
    var t0: Long = 0l
    var t1: Long = 0l
    var n = 10
    val hull = new QuickHull3D()
    println("warming up ... ")
    for (i <- 0 until 2) {
      val coords = randomSphericalPoints(10000, 1.0)
      hull.build(coords)
    }
    val cnt = 10
    for (i <- 0 until 4) {
      n *= 10
      val coords = randomSphericalPoints(n, 1.0)
      t0 = System.currentTimeMillis()
      for (k <- 0 until cnt) {
        hull.build(coords)
      }
      t1 = System.currentTimeMillis()
      println(n + " points: " + (t1 - t0) / cnt.toDouble + " msec")
    }
  }
}
