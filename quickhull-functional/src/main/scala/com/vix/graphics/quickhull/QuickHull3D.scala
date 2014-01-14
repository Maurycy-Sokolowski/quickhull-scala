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
import java.io.PrintStream
import java.io.StreamTokenizer
import java.io.InputStreamReader
import scala.collection.mutable.ArrayBuffer

object QuickHull3D {

  /**
   * Specifies that (on output) vertex indices for a face should be listed in
   * clockwise order.
   */
  val CLOCKWISE = 0x1

  /**
   * Specifies that (on output) the vertex indices for a face should be
   * numbered starting from 1.
   */
  val INDEXED_FROM_ONE = 0x2

  /**
   * Specifies that (on output) the vertex indices for a face should be
   * numbered starting from 0.
   */
  val INDEXED_FROM_ZERO = 0x4

  /**
   * Specifies that (on output) the vertex indices for a face should be
   * numbered with respect to the original input points.
   */
  val POINT_RELATIVE = 0x8

  /**
   * Specifies that the distance tolerance should be computed automatically
   * from the input point data.
   */
  val AUTOMATIC_TOLERANCE = -1

  /**
   * Precision of a double.
   */
  private val DOUBLE_PREC = 2.2204460492503131e-16

  private val NONCONVEX_WRT_LARGER_FACE = 1

  private val NONCONVEX = 2
}

/**
 * Computes the convex hull of a set of three dimensional points.
 *
 * <p>
 * The algorithm is a three dimensional implementation of Quickhull, as
 * described in Barber, Dobkin, and Huhdanpaa, <a
 * href=http://citeseer.ist.psu.edu/barber96quickhull.html> ``The Quickhull
 * Algorithm for Convex Hulls''</a> (ACM Transactions on Mathematical Software,
 * Vol. 22, No. 4, December 1996), and has a complexity of O(n log(n)) with
 * respect to the number of points. A well-known C implementation of Quickhull
 * that works for arbitrary dimensions is provided by <a
 * href=http://www.qhull.org>qhull</a>.
 *
 * <p>
 * A hull is constructed by providing a set of points to either a constructor or
 * a {@link #build(Point3d[]) build} method. After the hull is built, its
 * vertices and faces can be retrieved using {@link #getVertices() getVertices}
 * and {@link #getFaces() getFaces}. A typical usage might look like this:
 *
 * <pre>
 * // x y z coordinates of 6 points
 * Point3d[] points = new Point3d[] { new Point3d( 0.0, 0.0, 0.0 ), new Point3d( 1.0, 0.5, 0.0 ), new Point3d( 2.0, 0.0, 0.0 ), new Point3d( 0.5, 0.5, 0.5 ), new Point3d( 0.0, 0.0, 2.0 ),
 * 		new Point3d( 0.1, 0.2, 0.3 ), new Point3d( 0.0, 2.0, 0.0 ), };
 *
 * QuickHull3D hull = new QuickHull3D();
 * hull.build( points );
 *
 * System.out.println( &quot;Vertices:&quot; );
 * Point3d[] vertices = hull.getVertices();
 * for ( int i = 0; i &lt; vertices.length; i++ ) {
 * 	Point3d pnt = vertices[i];
 * 	System.out.println( pnt.x + &quot; &quot; + pnt.y + &quot; &quot; + pnt.z );
 * }
 *
 * System.out.println( &quot;Faces:&quot; );
 * int[][] faceIndices = hull.getFaces();
 * for ( int i = 0; i &lt; vertices.length; i++ ) {
 * 	for ( int k = 0; k &lt; faceIndices[i].length; k++ ) {
 * 		System.out.print( faceIndices[i][k] + &quot; &quot; );
 * 	}
 * 	System.out.println( &quot;&quot; );
 * }
 * </pre>
 *
 * As a convenience, there are also {@link #build(double[]) build} and
 * {@link #getVertices(double[]) getVertex} methods which pass point information
 * using an array of doubles.
 *
 * <h3><a name=distTol>Robustness</h3> Because this algorithm uses floating
 * point arithmetic, it is potentially vulnerable to errors arising from
 * numerical imprecision. We address this problem in the same way as <a
 * href=http://www.qhull.org>qhull</a>, by merging faces whose edges are not
 * clearly convex. A face is convex if its edges are convex, and an edge is
 * convex if the centroid of each adjacent plane is clearly <i>below</i> the
 * plane of the other face. The centroid is considered below a plane if its
 * distance to the plane is less than the negative of a
 * {@link #getDistanceTolerance() distance tolerance}. This tolerance represents
 * the smallest distance that can be reliably computed within the available
 * numeric precision. It is normally computed automatically from the point data,
 * although an application may {@link #setExplicitDistanceTolerance set this
 * tolerance explicitly}.
 *
 * <p>
 * Numerical problems are more likely to arise in situations where data points
 * lie on or within the faces or edges of the convex hull. We have tested
 * QuickHull3D for such situations by computing the convex hull of a random
 * point set, then adding additional randomly chosen points which lie very close
 * to the hull vertices and edges, and computing the convex hull again. The hull
 * is deemed correct if {@link #check check} returns <code>true</code>. These
 * tests have been successful for a large number of trials and so we are
 * confident that QuickHull3D is reasonably robust.
 *
 * <h3>Merged Faces</h3> The merging of faces means that the faces returned by
 * QuickHull3D may be convex polygons instead of triangles. If triangles are
 * desired, the application may {@link #triangulate triangulate} the faces, but
 * it should be noted that this may result in triangles which are very small or
 * thin and hence difficult to perform reliable convexity tests on. In other
 * words, triangulating a merged face is likely to restore the numerical
 * problems which the merging process removed. Hence is it possible that, after
 * triangulation, {@link #check check} will fail (the same behavior is observed
 * with triangulated output from <a href=http://www.qhull.org>qhull</a>).
 *
 * <h3>Degenerate Input</h3>It is assumed that the input points are
 * non-degenerate in that they are not coincident, colinear, or colplanar, and
 * thus the convex hull has a non-zero volume. If the input points are detected
 * to be degenerate within the {@link #getDistanceTolerance() distance
 * tolerance}, an IllegalArgumentException will be thrown.
 *
 * @author John E. Lloyd, Fall 2004
 */
class QuickHull3D {

  protected var findIndex: Int = -1

  protected var charLength: Double = _

  protected var debug: Boolean = false

  protected var pointBuffer = new Array[Vertex](0)

  protected var vertexPointIndices = new Array[Int](0)

  private var discardedFaces = new Array[Face](3)

  private var maxVtxs = new Array[Vertex](3)

  private var minVtxs = new Array[Vertex](3)

  protected var faces = new ArrayBuffer[Face]()

  protected var horizon = new ArrayBuffer[HalfEdge]()

  private var newFaces = new ListBuffer[Face]()

  private var unclaimed = new VertexList()

  private var claimed = new VertexList()

  protected var numVertices: Int = _

  protected var numFaces: Int = _

  protected var numPoints: Int = _

  protected var explicitTolerance: Double = QuickHull3D.AUTOMATIC_TOLERANCE

  protected var tolerance: Double = _

  /**
   * Returns true if debugging is enabled.
   *
   * @return true is debugging is enabled
   * @see QuickHull3D#setDebug
   */
  def getDebug(): Boolean = debug

  /**
   * Enables the printing of debugging diagnostics.
   *
   * @param enable
   *            if true, enables debugging
   */
  def setDebug(enable: Boolean) {
    debug = enable
  }

  /**
   * Returns the distance tolerance that was used for the most recently
   * computed hull. The distance tolerance is used to determine when faces are
   * unambiguously convex with respect to each other, and when points are
   * unambiguously above or below a face plane, in the presence of <a
   * href=#distTol>numerical imprecision</a>. Normally, this tolerance is
   * computed automatically for each set of input points, but it can be set
   * explicitly by the application.
   *
   * @return distance tolerance
   * @see QuickHull3D#setExplicitDistanceTolerance
   */
  def getDistanceTolerance(): Double = tolerance

  /**
   * Sets an explicit distance tolerance for convexity tests. If
   * {@link #AUTOMATIC_TOLERANCE AUTOMATIC_TOLERANCE} is specified (the
   * default), then the tolerance will be computed automatically from the
   * point data.
   *
   * @param tol
   *            explicit tolerance
   * @see #getDistanceTolerance
   */
  def setExplicitDistanceTolerance(tol: Double) {
    explicitTolerance = tol
  }

  /**
   * Returns the explicit distance tolerance.
   *
   * @return explicit tolerance
   * @see #setExplicitDistanceTolerance
   */
  def getExplicitDistanceTolerance(): Double = explicitTolerance

  private def addPointToFace(vtx: Vertex, face: Face) {
    vtx.face = face
    if (face.outside == null) {
      claimed.add(vtx)
    } else {
      claimed.insertBefore(vtx, face.outside)
    }
    face.outside = vtx
  }

  private def removePointFromFace(vtx: Vertex, face: Face) {
    if (vtx == face.outside) {
      face.outside = if (vtx.next != null && vtx.next.face == face) vtx.next else null
    }
    claimed.delete(vtx)
  }

  private def removeAllPointsFromFace(face: Face): Vertex = {
    if (face.outside != null) {
      var end = face.outside
      while (end.next != null && end.next.face == face) {
        end = end.next
      }
      claimed.delete(face.outside, end)
      end.next = null
      face.outside
    } else {
      null
    }
  }

  /**
   * Creates a convex hull object and initializes it to the convex hull of a
   * set of points whose coordinates are given by an array of doubles.
   *
   * @param coords
   *            x, y, and z coordinates of each input point. The length of
   *            this array will be three times the the number of input points.
   * @throws IllegalArgumentException
   *             the number of input points is less than four, or the points
   *             appear to be coincident, colinear, or coplanar.
   */
  def this(coords: Array[Double]) {
    this()
    build(coords, coords.length / 3)
  }

  /**
   * Creates a convex hull object and initializes it to the convex hull of a
   * set of points.
   *
   * @param points
   *            input points.
   * @throws IllegalArgumentException
   *             the number of input points is less than four, or the points
   *             appear to be coincident, colinear, or coplanar.
   */
  def this(points: Array[Point3d]) {
    this()
    build(points, points.length)
  }

  private def findHalfEdge(tail: Vertex, head: Vertex): HalfEdge = {
    for (f <- faces) {
      val he = f.findEdge(tail, head)
      if (he != null) {
        return he
      }
    }
    null
  }

  protected def setHull(coords: Array[Double],
    nump: Int,
    faceIndices: Array[Array[Int]],
    numf: Int) {
    initBuffers(nump)
    setPoints(coords, nump)
    computeMaxAndMin()
    for (i <- 0 until numf) {
      val face = Face.create(pointBuffer, faceIndices(i))
      var he = face.he0
      do {
        val heOpp = findHalfEdge(he.head(), he.tail())
        if (heOpp != null) {
          he.setOpposite(heOpp)
        }
        he = he.next
      } while (he != face.he0);
      faces += face
    }
  }

  private def printQhullErrors(proc: Process) {
    var wrote = false
    val es = proc.getErrorStream
    while (es.available() > 0) {
      System.out.write(es.read())
      wrote = true
    }
    if (wrote) {
      println("")
    }
  }

  def setFromQhull(coords: Array[Double], nump: Int, triangulate: Boolean) {
    var commandStr = "./qhull i"
    if (triangulate) {
      commandStr += " -Qt"
    }
    try {
      val proc = Runtime.getRuntime.exec(commandStr)
      val ps = new PrintStream(proc.getOutputStream)
      val stok = new StreamTokenizer(new InputStreamReader(proc.getInputStream))
      ps.println("3 " + nump)
      for (i <- 0 until nump) {
        ps.println(coords(i * 3 + 0) + " " + coords(i * 3 + 1) + " " + coords(i * 3 + 2))
      }
      ps.flush()
      ps.close()
      var indexList = new Array[Int](3)
      stok.eolIsSignificant(true)
      printQhullErrors(proc)
      do {
        stok.nextToken()
      } while (stok.sval == null || !stok.sval.startsWith("MERGEexact"));
      for (i <- 0 until 4) {
        stok.nextToken()
      }
      if (stok.ttype != StreamTokenizer.TT_NUMBER) {
        println("Expecting number of faces")
        System.exit(1)
      }
      val numf = stok.nval.toInt
      stok.nextToken()
      val faceIndices = Array.ofDim[Array[Int]](numf)
      for (i <- 0 until numf) {
        indexList = new Array[Int](3)
        while (stok.nextToken() != StreamTokenizer.TT_EOL) {
          if (stok.ttype != StreamTokenizer.TT_NUMBER) {
            println("Expecting face index")
            System.exit(1)
          }
          indexList(0) = stok.nval.toInt
        }
        faceIndices(i) = Array.ofDim[Int](indexList.size)
        var k = 0
        for (it <- indexList) {
          faceIndices(i)(k) = it
          k += 1
        }
      }
      setHull(coords, nump, faceIndices, numf)
    } catch {
      case e: Exception => {
        e.printStackTrace()
        System.exit(1)
      }
    }
  }

  /**
   * Constructs the convex hull of a set of points whose coordinates are given
   * by an array of doubles.
   *
   * @param coords
   *            x, y, and z coordinates of each input point. The length of
   *            this array will be three times the number of input points.
   * @throws IllegalArgumentException
   *             the number of input points is less than four, or the points
   *             appear to be coincident, colinear, or coplanar.
   */
  def build(coords: Array[Double]) {
    build(coords, coords.length / 3)
  }

  /**
   * Constructs the convex hull of a set of points whose coordinates are given
   * by an array of doubles.
   *
   * @param coords
   *            x, y, and z coordinates of each input point. The length of
   *            this array must be at least three times <code>nump</code>.
   * @param nump
   *            number of input points
   * @throws IllegalArgumentException
   *             the number of input points is less than four or greater than
   *             1/3 the length of <code>coords</code>, or the points appear
   *             to be coincident, colinear, or coplanar.
   */
  def build(coords: Array[Double], nump: Int) {
    if (nump < 4) {
      throw new IllegalArgumentException("Less than four input points specified")
    }
    if (coords.length / 3 < nump) {
      throw new IllegalArgumentException("Coordinate array too small for specified number of points")
    }
    initBuffers(nump)
    setPoints(coords, nump)
    buildHull()
  }

  /**
   * Constructs the convex hull of a set of points.
   *
   * @param points
   *            input points
   * @throws IllegalArgumentException
   *             the number of input points is less than four, or the points
   *             appear to be coincident, colinear, or coplanar.
   */
  def build(points: Array[Point3d]) {
    build(points, points.length)
  }

  /**
   * Constructs the convex hull of a set of points.
   *
   * @param points
   *            input points
   * @param nump
   *            number of input points
   * @throws IllegalArgumentException
   *             the number of input points is less than four or greater then
   *             the length of <code>points</code>, or the points appear to be
   *             coincident, colinear, or coplanar.
   */
  def build(points: Array[Point3d], nump: Int) {
    if (nump < 4) {
      throw new IllegalArgumentException("Less than four input points specified")
    }
    if (points.length < nump) {
      throw new IllegalArgumentException("Point array too small for specified number of points")
    }
    initBuffers(nump)
    setPoints(points, nump)
    buildHull()
  }

  /**
   * Triangulates any non-triangular hull faces. In some cases, due to
   * precision issues, the resulting triangles may be very thin or small, and
   * hence appear to be non-convex (this same limitation is present in <a
   * href=http://www.qhull.org>qhull</a>).
   */
  def triangulate() {
    val minArea = 1000 * charLength * QuickHull3D.DOUBLE_PREC
    newFaces.clear()
    for (face <- faces) {
      if (face.mark == Face.VISIBLE) face.triangulate(newFaces.toList, minArea)
    }
    for (face <- newFaces) faces += face
  }

  protected def initBuffers(nump: Int) {
    if (pointBuffer.length < nump) {
      val newBuffer = Array.ofDim[Vertex](nump)
      vertexPointIndices = Array.ofDim[Int](nump)
      for (i <- 0 until pointBuffer.length) {
        newBuffer(i) = pointBuffer(i)
      }
      for (i <- pointBuffer.length until nump) {
        newBuffer(i) = new Vertex()
      }
      pointBuffer = newBuffer
    }
    faces.clear()
    claimed.clear()
    numFaces = 0
    numPoints = nump
  }

  protected def setPoints(coords: Array[Double], nump: Int) {
    for (i <- 0 until nump) {
      val vtx = pointBuffer(i)
      vtx.pnt.set(coords(i * 3 + 0), coords(i * 3 + 1), coords(i * 3 + 2))
      vtx.index = i
    }
  }

  protected def setPoints(pnts: Array[Point3d], nump: Int) {
    for (i <- 0 until nump) {
      val vtx = pointBuffer(i)
      vtx.pnt.set(pnts(i))
      vtx.index = i
    }
  }

  protected def computeMaxAndMin() {
    val max = new Vector3d()
    val min = new Vector3d()
    for (i <- 0 until 3) {
      maxVtxs(i) = pointBuffer(0)
      minVtxs(i) = pointBuffer(0)
    }
    max.set(pointBuffer(0).pnt)
    min.set(pointBuffer(0).pnt)
    for (i <- 1 until numPoints) {
      val pnt = pointBuffer(i).pnt
      if (pnt.x > max.x) {
        max.x = pnt.x
        maxVtxs(0) = pointBuffer(i)
      } else if (pnt.x < min.x) {
        min.x = pnt.x
        minVtxs(0) = pointBuffer(i)
      }
      if (pnt.y > max.y) {
        max.y = pnt.y
        maxVtxs(1) = pointBuffer(i)
      } else if (pnt.y < min.y) {
        min.y = pnt.y
        minVtxs(1) = pointBuffer(i)
      }
      if (pnt.z > max.z) {
        max.z = pnt.z
        maxVtxs(2) = pointBuffer(i)
      } else if (pnt.z < min.z) {
        min.z = pnt.z
        maxVtxs(2) = pointBuffer(i)
      }
    }
    charLength = Math.max(max.x - min.x, max.y - min.y)
    charLength = Math.max(max.z - min.z, charLength)
    tolerance = if (explicitTolerance == QuickHull3D.AUTOMATIC_TOLERANCE) 3 * QuickHull3D.DOUBLE_PREC *
      (Math.max(Math.abs(max.x), Math.abs(min.x)) + Math.max(Math.abs(max.y), Math.abs(min.y)) +
        Math.max(Math.abs(max.z), Math.abs(min.z)))
    else explicitTolerance
  }

  /**
   * Creates the initial simplex from which the hull will be built.
   */
  protected def createInitialSimplex() {
    var max = 0.
    var imax = 0
    for (i <- 0 until 3) {
      val diff = maxVtxs(i).pnt.get(i) - minVtxs(i).pnt.get(i)
      if (diff > max) {
        max = diff
        imax = i
      }
    }
    if (max <= tolerance) {
      throw new IllegalArgumentException("Input points appear to be coincident")
    }
    val vtx = Array.ofDim[Vertex](4)
    vtx(0) = maxVtxs(imax)
    vtx(1) = minVtxs(imax)
    val u01 = new Vector3d()
    val diff02 = new Vector3d()
    val nrml = new Vector3d()
    val xprod = new Vector3d()
    var maxSqr = 0.
    u01.sub(vtx(1).pnt, vtx(0).pnt)
    u01.normalize()
    for (i <- 0 until numPoints) {
      diff02.sub(pointBuffer(i).pnt, vtx(0).pnt)
      xprod.cross(u01, diff02)
      val lenSqr = xprod.normSquared()
      if (lenSqr > maxSqr && pointBuffer(i) != vtx(0) && pointBuffer(i) != vtx(1)) {
        maxSqr = lenSqr
        vtx(2) = pointBuffer(i)
        nrml.set(xprod)
      }
    }
    if (Math.sqrt(maxSqr) <= 100 * tolerance) {
      throw new IllegalArgumentException("Input points appear to be colinear")
    }
    nrml.normalize()
    var maxDist = 0.
    val d0 = vtx(2).pnt.dot(nrml)
    for (i <- 0 until numPoints) {
      val dist = Math.abs(pointBuffer(i).pnt.dot(nrml) - d0)
      if (dist > maxDist && pointBuffer(i) != vtx(0) && pointBuffer(i) != vtx(1) &&
        pointBuffer(i) != vtx(2)) {
        maxDist = dist
        vtx(3) = pointBuffer(i)
      }
    }
    if (Math.abs(maxDist) <= 100 * tolerance) {
      throw new IllegalArgumentException("Input points appear to be coplanar")
    }
    if (debug) {
      println("initial vertices:")
      println(vtx(0).index + ": " + vtx(0).pnt)
      println(vtx(1).index + ": " + vtx(1).pnt)
      println(vtx(2).index + ": " + vtx(2).pnt)
      println(vtx(3).index + ": " + vtx(3).pnt)
    }
    val tris = Array.ofDim[Face](4)
    if (vtx(3).pnt.dot(nrml) - d0 < 0) {
      tris(0) = Face.createTriangle(vtx(0), vtx(1), vtx(2))
      tris(1) = Face.createTriangle(vtx(3), vtx(1), vtx(0))
      tris(2) = Face.createTriangle(vtx(3), vtx(2), vtx(1))
      tris(3) = Face.createTriangle(vtx(3), vtx(0), vtx(2))
      for (i <- 0 until 3) {
        val k = (i + 1) % 3
        tris(i + 1).getEdge(1).setOpposite(tris(k + 1).getEdge(0))
        tris(i + 1).getEdge(2).setOpposite(tris(0).getEdge(k))
      }
    } else {
      tris(0) = Face.createTriangle(vtx(0), vtx(2), vtx(1))
      tris(1) = Face.createTriangle(vtx(3), vtx(0), vtx(1))
      tris(2) = Face.createTriangle(vtx(3), vtx(1), vtx(2))
      tris(3) = Face.createTriangle(vtx(3), vtx(2), vtx(0))
      for (i <- 0 until 3) {
        val k = (i + 1) % 3
        tris(i + 1).getEdge(0).setOpposite(tris(k + 1).getEdge(1))
        tris(i + 1).getEdge(2).setOpposite(tris(0).getEdge((3 - i) % 3))
      }
    }
    for (i <- 0 until 4) {
      faces += tris(i)
    }
    for (i <- 0 until numPoints) {
      val v = pointBuffer(i)
      if (v == vtx(0) || v == vtx(1) || v == vtx(2) || v == vtx(3)) {
        //continue
      }
      maxDist = tolerance
      var maxFace: Face = null
      for (k <- 0 until 4) {
        val dist = tris(k).distanceToPlane(v.pnt)
        if (dist > maxDist) {
          maxFace = tris(k)
          maxDist = dist
        }
      }
      if (maxFace != null) {
        addPointToFace(v, maxFace)
      }
    }
  }

  /**
   * Returns the number of vertices in this hull.
   *
   * @return number of vertices
   */
  def getNumVertices(): Int = numVertices

  /**
   * Returns the vertex points in this hull.
   *
   * @return array of vertex points
   * @see QuickHull3D#getVertices(double[])
   * @see QuickHull3D#getFaces()
   */
  def getVertices(): Array[Point3d] = {
    val vtxs = Array.ofDim[Point3d](numVertices)
    for (i <- 0 until numVertices) {
      vtxs(i) = pointBuffer(vertexPointIndices(i)).pnt
    }
    vtxs
  }

  /**
   * Returns the coordinates of the vertex points of this hull.
   *
   * @param coords
   *            returns the x, y, z coordinates of each vertex. This length of
   *            this array must be at least three times the number of
   *            vertices.
   * @return the number of vertices
   * @see QuickHull3D#getVertices()
   * @see QuickHull3D#getFaces()
   */
  def getVertices(coords: Array[Double]): Int = {
    for (i <- 0 until numVertices) {
      val pnt = pointBuffer(vertexPointIndices(i)).pnt
      coords(i * 3 + 0) = pnt.x
      coords(i * 3 + 1) = pnt.y
      coords(i * 3 + 2) = pnt.z
    }
    numVertices
  }

  /**
   * Returns an array specifing the index of each hull vertex with respect to
   * the original input points.
   *
   * @return vertex indices with respect to the original points
   */
  def getVertexPointIndices(): Array[Int] = {
    val indices = Array.ofDim[Int](numVertices)
    for (i <- 0 until numVertices) {
      indices(i) = vertexPointIndices(i)
    }
    indices
  }

  /**
   * Returns the number of faces in this hull.
   *
   * @return number of faces
   */
  def getNumFaces(): Int = faces.size

  /**
   * Returns the faces associated with this hull.
   *
   * <p>
   * Each face is represented by an integer array which gives the indices of
   * the vertices. These indices are numbered relative to the hull vertices,
   * are zero-based, and are arranged counter-clockwise. More control over the
   * index format can be obtained using {@link #getFaces(int)
   * getFaces(indexFlags)}.
   *
   * @return array of integer arrays, giving the vertex indices for each face.
   * @see QuickHull3D#getVertices()
   * @see QuickHull3D#getFaces(int)
   */
  def getFaces(): Array[Array[Int]] = getFaces(0)

  /**
   * Returns the faces associated with this hull.
   *
   * <p>
   * Each face is represented by an integer array which gives the indices of
   * the vertices. By default, these indices are numbered with respect to the
   * hull vertices (as opposed to the input points), are zero-based, and are
   * arranged counter-clockwise. However, this can be changed by setting
   * {@link #POINT_RELATIVE POINT_RELATIVE}, {@link #INDEXED_FROM_ONE
   * INDEXED_FROM_ONE}, or {@link #CLOCKWISE CLOCKWISE} in the indexFlags
   * parameter.
   *
   * @param indexFlags
   *            specifies index characteristics (0 results in the default)
   * @return array of integer arrays, giving the vertex indices for each face.
   * @see QuickHull3D#getVertices()
   */
  def getFaces(indexFlags: Int): Array[Array[Int]] = {
    val allFaces = Array.ofDim[Array[Int]](faces.size)
    var k = 0
    for (face <- faces) {
      allFaces(k) = Array.ofDim[Int](face.numVertices())
      getFaceIndices(allFaces(k), face, indexFlags)
      k += 1
    }
    allFaces
  }

  /**
   * Prints the vertices and faces of this hull to the stream ps.
   *
   * <p>
   * This is done using the Alias Wavefront .obj file format, with the
   * vertices printed first (each preceding by the letter <code>v</code>),
   * followed by the vertex indices for each face (each preceded by the letter
   * <code>f</code>).
   *
   * <p>
   * The face indices are numbered with respect to the hull vertices (as
   * opposed to the input points), with a lowest index of 1, and are arranged
   * counter-clockwise. More control over the index format can be obtained
   * using {@link #print(PrintStream,int) print(ps,indexFlags)}.
   *
   * @param ps
   *            stream used for printing
   * @see QuickHull3D#print(PrintStream,int)
   * @see QuickHull3D#getVertices()
   * @see QuickHull3D#getFaces()
   */
  def print(ps: PrintStream) {
    print(ps, 0)
  }

  /**
   * Prints the vertices and faces of this hull to the stream ps.
   *
   * <p>
   * This is done using the Alias Wavefront .obj file format, with the
   * vertices printed first (each preceding by the letter <code>v</code>),
   * followed by the vertex indices for each face (each preceded by the letter
   * <code>f</code>).
   *
   * <p>
   * By default, the face indices are numbered with respect to the hull
   * vertices (as opposed to the input points), with a lowest index of 1, and
   * are arranged counter-clockwise. However, this can be changed by setting
   * {@link #POINT_RELATIVE POINT_RELATIVE}, {@link #INDEXED_FROM_ONE
   * INDEXED_FROM_ZERO}, or {@link #CLOCKWISE CLOCKWISE} in the indexFlags
   * parameter.
   *
   * @param ps
   *            stream used for printing
   * @param indexFlags
   *            specifies index characteristics (0 results in the default).
   * @see QuickHull3D#getVertices()
   * @see QuickHull3D#getFaces()
   */
  def print(ps: PrintStream, indexFlags: Int) {
    if ((indexFlags & QuickHull3D.INDEXED_FROM_ZERO) == 0) {
      //indexFlags |= QuickHull3D.INDEXED_FROM_ONE
    }
    for (i <- 0 until numVertices) {
      val pnt = pointBuffer(vertexPointIndices(i)).pnt
      ps.println("v " + pnt.x + " " + pnt.y + " " + pnt.z)
    }
    for (face <- faces) {
      val indices = Array.ofDim[Int](face.numVertices())
      getFaceIndices(indices, face, indexFlags)
      ps.print("f")
      for (k <- 0 until indices.length) {
        ps.print(" " + indices(k))
      }
      ps.println("")
    }
  }

  private def getFaceIndices(indices: Array[Int], face: Face, flags: Int) {
    val ccw = ((flags & QuickHull3D.CLOCKWISE) == 0)
    val indexedFromOne = ((flags & QuickHull3D.INDEXED_FROM_ONE) != 0)
    val pointRelative = ((flags & QuickHull3D.POINT_RELATIVE) != 0)
    var hedge = face.he0
    var k = 0
    do {
      var idx = hedge.head().index
      if (pointRelative) {
        idx = vertexPointIndices(idx)
      }
      if (indexedFromOne) {
        idx += 1
      }
      indices(k) = idx
      k += 1
      hedge = (if (ccw) hedge.next else hedge.prev)
    } while (hedge != face.he0);
  }

  protected def resolveUnclaimedPoints(newFaces: ListBuffer[Face]) {
    var vtxNext = unclaimed.first()
    var vtx = vtxNext
    while (vtx != null) {
      vtxNext = vtx.next
      var maxDist = tolerance
      var maxFace: Face = null
      for (newFace <- newFaces if newFace.mark == Face.VISIBLE) {
        if (maxDist < 1000 * tolerance) {
          val dist = newFace.distanceToPlane(vtx.pnt)
          if (dist > maxDist) {
            maxDist = dist
            maxFace = newFace
          }
        }
      }
      if (maxFace != null) {
        addPointToFace(vtx, maxFace)
        if (debug && vtx.index == findIndex) {
          println(findIndex + " CLAIMED BY " + maxFace.getVertexString)
        }
      } else {
        if (debug && vtx.index == findIndex) {
          println(findIndex + " DISCARDED")
        }
      }
      vtx = vtxNext
    }
  }

  protected def deleteFacePoints(face: Face, absorbingFace: Face) {
    val faceVtxs = removeAllPointsFromFace(face)
    if (faceVtxs != null) {
      if (absorbingFace == null) {
        unclaimed.addAll(faceVtxs)
      } else {
        var vtxNext = faceVtxs
        var vtx = vtxNext
        while (vtx != null) {
          vtxNext = vtx.next
          val dist = absorbingFace.distanceToPlane(vtx.pnt)
          if (dist > tolerance) {
            addPointToFace(vtx, absorbingFace)
          } else {
            unclaimed.add(vtx)
          }
          vtx = vtxNext
        }
      }
    }
  }

  protected def oppFaceDistance(he: HalfEdge): Double = {
    he.face.distanceToPlane(he.opposite.face.getCentroid)
  }

  private def doAdjacentMerge(face: Face, mergeType: Int): Boolean = {
    var hedge = face.he0
    var convex = true
    do {
      val oppFace = hedge.oppositeFace()
      var merge = false
      if (mergeType == QuickHull3D.NONCONVEX) {
        if (oppFaceDistance(hedge) > -tolerance || oppFaceDistance(hedge.opposite) > -tolerance) {
          merge = true
        }
      } else {
        if (face.area > oppFace.area) {
          if (oppFaceDistance(hedge) > -tolerance) {
            merge = true
          } else if (oppFaceDistance(hedge.opposite) > -tolerance) {
            convex = false
          }
        } else {
          if (oppFaceDistance(hedge.opposite) > -tolerance) {
            merge = true
          } else if (oppFaceDistance(hedge) > -tolerance) {
            convex = false
          }
        }
      }
      if (merge) {
        if (debug) {
          println("  merging " + face.getVertexString + "  and  " + oppFace.getVertexString)
        }
        val numd = face.mergeAdjacentFace(hedge, discardedFaces)
        for (i <- 0 until numd) {
          deleteFacePoints(discardedFaces(i), face)
        }
        if (debug) {
          println("  result: " + face.getVertexString)
        }
        return true
      }
      hedge = hedge.next
    } while (hedge != face.he0);
    if (!convex) {
      face.mark = Face.NON_CONVEX
    }
    false
  }

  protected def calculateHorizon(eyePnt: Point3d,
    edge0nmut: HalfEdge,
    face: Face,
    horizon: ArrayBuffer[HalfEdge]) {
    deleteFacePoints(face, null)
    face.mark = Face.DELETED
    if (debug) {
      println("  visiting face " + face.getVertexString)
    }
    var edge: HalfEdge = null
    var edge0 = edge0nmut
    if (edge0 == null) {
      edge0 = face.getEdge(0)
      edge = edge0
    } else {
      edge = edge0.getNext
    }
    do {
      val oppFace = edge.oppositeFace()
      if (oppFace.mark == Face.VISIBLE) {
        if (oppFace.distanceToPlane(eyePnt) > tolerance) {
          calculateHorizon(eyePnt, edge.getOpposite, oppFace, horizon)
        } else {
          horizon += edge
          if (debug) {
            println("  adding horizon edge " + edge.getVertexString)
          }
        }
      }
      edge = edge.getNext
    } while (edge != edge0);
  }

  private def addAdjoiningFace(eyeVtx: Vertex, he: HalfEdge): HalfEdge = {
    val face = Face.createTriangle(eyeVtx, he.tail(), he.head())
    faces += face
    face.getEdge(-1).setOpposite(he.getOpposite)
    face.getEdge(0)
  }

  protected def addNewFaces(eyeVtx: Vertex, horizon: ArrayBuffer[HalfEdge]) {
    newFaces.clear()
    var hedgeSidePrev: HalfEdge = null
    var hedgeSideBegin: HalfEdge = null
    for (horizonHe <- horizon) {
      val hedgeSide = addAdjoiningFace(eyeVtx, horizonHe)
      if (debug) {
        println("new face: " + hedgeSide.face.getVertexString)
      }
      if (hedgeSidePrev != null) {
        hedgeSide.next.setOpposite(hedgeSidePrev)
      } else {
        hedgeSideBegin = hedgeSide
      }
      newFaces += hedgeSide.getFace
      hedgeSidePrev = hedgeSide
    }
    hedgeSideBegin.next.setOpposite(hedgeSidePrev)
  }

  protected def nextPointToAdd(): Vertex = {
    if (!claimed.isEmpty) {
      val eyeFace = claimed.first().face
      var eyeVtx: Vertex = null
      var maxDist = 0.
      var vtx = eyeFace.outside
      while (vtx != null && vtx.face == eyeFace) {
        val dist = eyeFace.distanceToPlane(vtx.pnt)
        if (dist > maxDist) {
          maxDist = dist
          eyeVtx = vtx
        }
        vtx = vtx.next
      }
      eyeVtx
    } else {
      null
    }
  }

  protected def addPointToHull(eyeVtx: Vertex) {
    horizon.clear()
    unclaimed.clear()
    if (debug) {
      println("Adding point: " + eyeVtx.index)
      println(" which is " + eyeVtx.face.distanceToPlane(eyeVtx.pnt) +
        " above face " +
        eyeVtx.face.getVertexString)
    }
    removePointFromFace(eyeVtx, eyeVtx.face)
    calculateHorizon(eyeVtx.pnt, null, eyeVtx.face, horizon)
    newFaces.clear()
    addNewFaces(eyeVtx, horizon)
    for (face <- newFaces if face.mark == Face.VISIBLE) while (doAdjacentMerge(face, QuickHull3D.NONCONVEX_WRT_LARGER_FACE))
      for (face <- newFaces if face.mark == Face.NON_CONVEX) {
        face.mark = Face.VISIBLE
        while (doAdjacentMerge(face, QuickHull3D.NONCONVEX)) true
      }
    resolveUnclaimedPoints(newFaces)
  }

  protected def buildHull() {
    var cnt = 0
    var eyeVtx: Vertex = null
    computeMaxAndMin()
    createInitialSimplex()
    do {
      eyeVtx = nextPointToAdd()
      if (eyeVtx != null) {

        addPointToHull(eyeVtx)
        cnt += 1
        if (debug) {
          println("iteration " + cnt + " done")
        }
      }
    } while (eyeVtx != null)
    reindexFacesAndVertices()
    if (debug) {
      println("hull done")
    }
  }

  private def markFaceVertices(face: Face, mark: Int) {
    val he0 = face.getFirstEdge
    var he = he0
    do {
      he.head().index = mark
      he = he.next
    } while (he != he0);
  }

  protected def reindexFacesAndVertices() {
    for (i <- 0 until numPoints) {
      pointBuffer(i).index = -1
    }
    numFaces = 0
    var toDelete = new ArrayBuffer[Face]()
    for (face <- faces) {
      if (face.mark != Face.VISIBLE) {
        toDelete += face
      } else {
        markFaceVertices(face, 0)
        numFaces += 1
      }
    }
    faces --= toDelete
    numVertices = 0
    for (i <- 0 until numPoints) {
      val vtx = pointBuffer(i)
      if (vtx.index == 0) {
        vertexPointIndices(numVertices) = i
        vtx.index = numVertices
        numVertices += 1
      }
    }
  }

  protected def checkFaceConvexity(face: Face, tol: Double, ps: PrintStream): Boolean = {
    var dist: Double = 0.0
    var he = face.he0
    do {
      face.checkConsistency()
      dist = oppFaceDistance(he)
      if (dist > tol) {
        if (ps != null) {
          ps.println("Edge " + he.getVertexString + " non-convex by " + dist)
        }
        return false
      }
      dist = oppFaceDistance(he.opposite)
      if (dist > tol) {
        if (ps != null) {
          ps.println("Opposite edge " + he.opposite.getVertexString + " non-convex by " +
            dist)
        }
        return false
      }
      if (he.next.oppositeFace() == he.oppositeFace()) {
        if (ps != null) {
          ps.println("Redundant vertex " + he.head().index + " in face " +
            face.getVertexString)
        }
        return false
      }
      he = he.next
    } while (he != face.he0);
    true
  }

  protected def checkFaces(tol: Double, ps: PrintStream): Boolean = {
    var convex = true
    for (face <- faces) {
      if (face.mark == Face.VISIBLE) {
        if (!checkFaceConvexity(face, tol, ps)) {
          convex = false
        }
      }
    }
    convex
  }

  /**
   * Checks the correctness of the hull using the distance tolerance returned
   * by {@link QuickHull3D#getDistanceTolerance getDistanceTolerance}; see
   * {@link QuickHull3D#check(PrintStream,double) check(PrintStream,double)}
   * for details.
   *
   * @param ps
   *            print stream for diagnostic messages; may be set to
   *            <code>null</code> if no messages are desired.
   * @return true if the hull is valid
   * @see QuickHull3D#check(PrintStream,double)
   */
  def check(ps: PrintStream): Boolean = check(ps, getDistanceTolerance)

  /**
   * Checks the correctness of the hull. This is done by making sure that no
   * faces are non-convex and that no points are outside any face. These tests
   * are performed using the distance tolerance <i>tol</i>. Faces are
   * considered non-convex if any edge is non-convex, and an edge is
   * non-convex if the centroid of either adjoining face is more than
   * <i>tol</i> above the plane of the other face. Similarly, a point is
   * considered outside a face if its distance to that face's plane is more
   * than 10 times <i>tol</i>.
   *
   * <p>
   * If the hull has been {@link #triangulate triangulated}, then this routine
   * may fail if some of the resulting triangles are very small or thin.
   *
   * @param ps
   *            print stream for diagnostic messages; may be set to
   *            <code>null</code> if no messages are desired.
   * @param tol
   *            distance tolerance
   * @return true if the hull is valid
   * @see QuickHull3D#check(PrintStream)
   */
  def check(ps: PrintStream, tol: Double): Boolean = {
    var dist: Double = 0.0
    val pointTol = 10 * tol
    if (!checkFaces(tolerance, ps)) {
      return false
    }
    for (i <- 0 until numPoints) {
      val pnt = pointBuffer(i).pnt
      for (face <- faces) {
        if (face.mark == Face.VISIBLE) {
          dist = face.distanceToPlane(pnt)
          if (dist > pointTol) {
            if (ps != null) {
              ps.println("Point " + i + " " + dist + " above face " + face.getVertexString)
            }
            return false
          }
        }
      }
    }
    true
  }
}
