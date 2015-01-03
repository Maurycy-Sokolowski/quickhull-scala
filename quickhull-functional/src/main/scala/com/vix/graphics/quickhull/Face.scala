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

import Face._
import scala.beans.{ BeanProperty, BooleanBeanProperty }

object Face {
  val VISIBLE = 1
  val NON_CONVEX = 2
  val DELETED = 3

  def createTriangle(v0: Vertex, v1: Vertex, v2: Vertex): Face = createTriangle(v0, v1, v2, 0)

  /**
   * Constructs a triangule Face from vertices v0, v1, and v2.
   *
   * @param v0
   *            first vertex
   * @param v1
   *            second vertex
   * @param v2
   *            third vertex
   */
  def createTriangle(v0: Vertex,
    v1: Vertex,
    v2: Vertex,
    minArea: Double): Face = {
    val face = new Face()
    val he0 = new HalfEdge(v0, face)
    val he1 = new HalfEdge(v1, face)
    val he2 = new HalfEdge(v2, face)
    he0.prev = he2
    he0.next = he1
    he1.prev = he0
    he1.next = he2
    he2.prev = he1
    he2.next = he0
    face.he0 = he0
    face.computeNormalAndCentroid(minArea)
    face
  }

  def create(vtxArray: Array[Vertex], indices: Array[Int]): Face = {
    val face = new Face()
    var hePrev: HalfEdge = null
    for (i <- 0 until indices.length) {
      val he = new HalfEdge(vtxArray(indices(i)), face)
      if (hePrev != null) {
        he.setPrev(hePrev)
        hePrev.setNext(he)
      } else {
        face.he0 = he
      }
      hePrev = he
    }
    face.he0.setPrev(hePrev)
    hePrev.setNext(face.he0)
    face.computeNormalAndCentroid()
    face
  }
}

/**
 * Basic triangular face used to form the hull.
 *
 * <p>
 * The information stored for each face consists of a planar normal, a planar
 * offset, and a doubly-linked list of three <a href=HalfEdge>HalfEdges</a>
 * which surround the face in a counter-clockwise direction.
 *
 * @author John E. Lloyd, Fall 2004
 */
class Face {

  var he0: HalfEdge = _

  @BeanProperty
  var normal = new Vector3d()

  var area: Double = _

  @BeanProperty
  var centroid = new Point3d()

  var planeOffset: Double = _

  var index: Int = _

  var numVerts: Int = _

  var next: Face = _

  var mark = VISIBLE

  var outside: Vertex = _

  def computeCentroid(centroid: Point3d) {
    centroid.setZero()
    var he = he0
    do {
      centroid.add(he.head().pnt)
      he = he.next
    } while (he != he0);
    centroid.scale(1 / numVerts.toDouble)
  }

  def computeNormal(normal: Vector3d, minArea: Double) {
    computeNormal(normal)
    if (area < minArea) {
      println("area=" + area)
      var hedgeMax: HalfEdge = null
      var lenSqrMax = 0.0
      var hedge = he0
      do {
        val lenSqr = hedge.lengthSquared()
        if (lenSqr > lenSqrMax) {
          hedgeMax = hedge
          lenSqrMax = lenSqr
        }
        hedge = hedge.next
      } while (hedge != he0);
      val p2 = hedgeMax.head().pnt
      val p1 = hedgeMax.tail().pnt
      val lenMax = Math.sqrt(lenSqrMax)
      val ux = (p2.x - p1.x) / lenMax
      val uy = (p2.y - p1.y) / lenMax
      val uz = (p2.z - p1.z) / lenMax
      val dot = normal.x * ux + normal.y * uy + normal.z * uz
      normal.x -= dot * ux
      normal.y -= dot * uy
      normal.z -= dot * uz
      normal.normalize()
    }
  }

  def computeNormal(normal: Vector3d) {
    var he1 = he0.next
    var he2 = he1.next
    val p0 = he0.head().pnt
    var p2 = he1.head().pnt
    var d2x = p2.x - p0.x
    var d2y = p2.y - p0.y
    var d2z = p2.z - p0.z
    normal.setZero()
    numVerts = 2
    while (he2 != he0) {
      val d1x = d2x
      val d1y = d2y
      val d1z = d2z
      p2 = he2.head().pnt
      d2x = p2.x - p0.x
      d2y = p2.y - p0.y
      d2z = p2.z - p0.z
      normal.x += d1y * d2z - d1z * d2y
      normal.y += d1z * d2x - d1x * d2z
      normal.z += d1x * d2y - d1y * d2x
      he1 = he2
      he2 = he2.next
      numVerts += 1
    }
    area = normal.norm()
    normal.scale(1 / area)
  }

  private def computeNormalAndCentroid() {
    computeNormal(normal)
    computeCentroid(centroid)
    planeOffset = normal.dot(centroid)
    var numv = 0
    var he = he0
    do {
      numv += 1
      he = he.next
    } while (he != he0);
    if (numv != numVerts) {
      throw new RuntimeException("face " + getVertexString + " numVerts=" + numVerts +
        " should be " +
        numv)
    }
  }

  private def computeNormalAndCentroid(minArea: Double) {
    computeNormal(normal, minArea)
    computeCentroid(centroid)
    planeOffset = normal.dot(centroid)
  }

  /**
   * Gets the i-th half-edge associated with the face.
   *
   * @param i
   *            the half-edge index, in the range 0-2.
   * @return the half-edge
   */
  def getEdge(i: Int): HalfEdge = {
    var imut = i;
    var he = he0
    while (imut > 0) {
      he = he.next
      imut -= 1
    }
    while (imut < 0) {
      he = he.prev
      imut += 1
    }
    he
  }

  def getFirstEdge(): HalfEdge = he0

  /**
   * Finds the half-edge within this face which has tail <code>vt</code> and
   * head <code>vh</code>.
   *
   * @param vt
   *            tail point
   * @param vh
   *            head point
   * @return the half-edge, or null if none is found.
   */
  def findEdge(vt: Vertex, vh: Vertex): HalfEdge = {
    var he = he0
    do {
      if (he.head() == vh && he.tail() == vt) {
        return he
      }
      he = he.next
    } while (he != he0);
    null
  }

  /**
   * Computes the distance from a point p to the plane of this face.
   *
   * @param p
   *            the point
   * @return distance from the point to the plane
   */
  def distanceToPlane(p: Point3d): Double = {
    normal.x * p.x + normal.y * p.y + normal.z * p.z - planeOffset
  }

  def numVertices(): Int = numVerts

  def getVertexString(): String = {
    var s: String = null
    var he = he0
    do {
      if (s == null) {
        s = "" + he.head().index
      } else {
        s += " " + he.head().index
      }
      he = he.next
    } while (he != he0);
    s
  }

  def getVertexIndices(idxs: Array[Int]) {
    var he = he0
    var i = 0
    do {
      idxs(i) = he.head().index
      i += 1
      he = he.next
    } while (he != he0);
  }

  private def connectHalfEdges(hedgePrev: HalfEdge, hedge: HalfEdge): Face = {
    var discardedFace: Face = null
    if (hedgePrev.oppositeFace() == hedge.oppositeFace()) {
      val oppFace = hedge.oppositeFace()
      var hedgeOpp: HalfEdge = null
      if (hedgePrev == he0) {
        he0 = hedge
      }
      if (oppFace.numVertices() == 3) {
        hedgeOpp = hedge.getOpposite.prev.getOpposite
        oppFace.mark = DELETED
        discardedFace = oppFace
      } else {
        hedgeOpp = hedge.getOpposite.next
        if (oppFace.he0 == hedgeOpp.prev) {
          oppFace.he0 = hedgeOpp
        }
        hedgeOpp.prev = hedgeOpp.prev.prev
        hedgeOpp.prev.next = hedgeOpp
      }
      hedge.prev = hedgePrev.prev
      hedge.prev.next = hedge
      hedge.opposite = hedgeOpp
      hedgeOpp.opposite = hedge
      oppFace.computeNormalAndCentroid()
    } else {
      hedgePrev.next = hedge
      hedge.prev = hedgePrev
    }
    discardedFace
  }

  def checkConsistency() {
    var hedge = he0
    var maxd = 0.0
    var numv = 0
    if (numVerts < 3) {
      throw new RuntimeException("degenerate face: " + getVertexString)
    }
    do {
      val hedgeOpp = hedge.getOpposite
      if (hedgeOpp == null) {
        throw new RuntimeException("face " + getVertexString + ": " + "unreflected half edge " +
          hedge.getVertexString)
      } else if (hedgeOpp.getOpposite != hedge) {
        throw new RuntimeException("face " + getVertexString + ": " + "opposite half edge " +
          hedgeOpp.getVertexString +
          " has opposite " +
          hedgeOpp.getOpposite.getVertexString)
      }
      if (hedgeOpp.head() != hedge.tail() || hedge.head() != hedgeOpp.tail()) {
        throw new RuntimeException("face " + getVertexString + ": " + "half edge " + hedge.getVertexString +
          " reflected by " +
          hedgeOpp.getVertexString)
      }
      val oppFace = hedgeOpp.face
      if (oppFace == null) {
        throw new RuntimeException("face " + getVertexString + ": " + "no face on half edge " +
          hedgeOpp.getVertexString)
      } else if (oppFace.mark == DELETED) {
        throw new RuntimeException("face " + getVertexString + ": " + "opposite face " +
          oppFace.getVertexString +
          " not on hull")
      }
      val d = Math.abs(distanceToPlane(hedge.head().pnt))
      if (d > maxd) {
        maxd = d
      }
      numv += 1
      hedge = hedge.next
    } while (hedge != he0);
    if (numv != numVerts) {
      throw new RuntimeException("face " + getVertexString + " numVerts=" + numVerts +
        " should be " +
        numv)
    }
  }

  def mergeAdjacentFace(hedgeAdj: HalfEdge, discarded: Array[Face]): Int = {
    val oppFace = hedgeAdj.oppositeFace()
    var numDiscarded = 0
    discarded(numDiscarded) = oppFace
    numDiscarded += 1
    oppFace.mark = DELETED
    val hedgeOpp = hedgeAdj.getOpposite
    var hedgeAdjPrev = hedgeAdj.prev
    var hedgeAdjNext = hedgeAdj.next
    var hedgeOppPrev = hedgeOpp.prev
    var hedgeOppNext = hedgeOpp.next
    while (hedgeAdjPrev.oppositeFace() == oppFace) {
      hedgeAdjPrev = hedgeAdjPrev.prev
      hedgeOppNext = hedgeOppNext.next
    }
    while (hedgeAdjNext.oppositeFace() == oppFace) {
      hedgeOppPrev = hedgeOppPrev.prev
      hedgeAdjNext = hedgeAdjNext.next
    }
    var hedge: HalfEdge = null
    hedge = hedgeOppNext
    while (hedge != hedgeOppPrev.next) {
      hedge.face = this
      hedge = hedge.next
    }
    if (hedgeAdj == he0) {
      he0 = hedgeAdjNext
    }
    var discardedFace: Face = null
    discardedFace = connectHalfEdges(hedgeOppPrev, hedgeAdjNext)
    if (discardedFace != null) {
      discarded(numDiscarded) = discardedFace
      numDiscarded += 1
    }
    discardedFace = connectHalfEdges(hedgeAdjPrev, hedgeOppNext)
    if (discardedFace != null) {
      discarded(numDiscarded) = discardedFace
      numDiscarded += 1
    }
    computeNormalAndCentroid()
    checkConsistency()
    numDiscarded
  }

  def triangulate(newFaces: List[Face], minArea: Double) {
    var hedge: HalfEdge = null
    if (numVertices() < 4) {
      return
    }
    val v0 = he0.head()
    hedge = he0.next
    var oppPrev = hedge.opposite
    var face0: Face = null
    hedge = hedge.next
    while (hedge != he0.prev) {
      val face = createTriangle(v0, hedge.prev.head(), hedge.head(), minArea)
      face.he0.next.setOpposite(oppPrev)
      face.he0.prev.setOpposite(hedge.opposite)
      oppPrev = face.he0
      newFaces :+ face
      if (face0 == null) {
        face0 = face
      }
      hedge = hedge.next
    }
    hedge = new HalfEdge(he0.prev.prev.head(), this)
    hedge.setOpposite(oppPrev)
    hedge.prev = he0
    hedge.prev.next = hedge
    hedge.next = he0.prev
    hedge.next.prev = hedge
    computeNormalAndCentroid(minArea)
    checkConsistency()
    var face = face0
    while (face != null) {
      face.checkConsistency()
      face = face.next
    }
  }
}
