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

/**
 * Represents the half-edges that surround each face in a counter-clockwise
 * direction.
 *
 * @author John E. Lloyd, Fall 2004
 */
class HalfEdge(var vertex: Vertex, var face: Face) {

  /**
   * Next half-edge in the triangle.
   */
  var next: HalfEdge = _

  /**
   * Previous half-edge in the triangle.
   */
  var prev: HalfEdge = _

  /**
   * Half-edge associated with the opposite triangle adjacent to this edge.
   */
  var opposite: HalfEdge = _

  /**
   * Sets the value of the next edge adjacent (counter-clockwise) to this one
   * within the triangle.
   *
   * @param edge
   *            next adjacent edge
   */
  def setNext(edge: HalfEdge) {
    next = edge
  }

  /**
   * Gets the value of the next edge adjacent (counter-clockwise) to this one
   * within the triangle.
   *
   * @return next adjacent edge
   */
  def getNext(): HalfEdge = next

  /**
   * Sets the value of the previous edge adjacent (clockwise) to this one
   * within the triangle.
   *
   * @param edge
   *            previous adjacent edge
   */
  def setPrev(edge: HalfEdge) {
    prev = edge
  }

  /**
   * Gets the value of the previous edge adjacent (clockwise) to this one
   * within the triangle.
   *
   * @return previous adjacent edge
   */
  def getPrev(): HalfEdge = prev

  /**
   * Returns the triangular face located to the left of this half-edge.
   *
   * @return left-hand triangular face
   */
  def getFace(): Face = face

  /**
   * Returns the half-edge opposite to this half-edge.
   *
   * @return opposite half-edge
   */
  def getOpposite(): HalfEdge = opposite

  /**
   * Sets the half-edge opposite to this half-edge.
   *
   * @param edge
   *            opposite half-edge
   */
  def setOpposite(edge: HalfEdge) {
    opposite = edge
    edge.opposite = this
  }

  /**
   * Returns the head vertex associated with this half-edge.
   *
   * @return head vertex
   */
  def head(): Vertex = vertex

  /**
   * Returns the tail vertex associated with this half-edge.
   *
   * @return tail vertex
   */
  def tail(): Vertex = if (prev != null) prev.vertex else null

  /**
   * Returns the opposite triangular face associated with this half-edge.
   *
   * @return opposite triangular face
   */
  def oppositeFace(): Face = {
    if (opposite != null) opposite.face else null
  }

  /**
   * Produces a string identifying this half-edge by the point index values of
   * its tail and head vertices.
   *
   * @return identifying string
   */
  def getVertexString(): String = {
    if (tail() != null) {
      "" + tail().index + "-" + head().index
    } else {
      "?-" + head().index
    }
  }

  /**
   * Returns the length of this half-edge.
   *
   * @return half-edge length
   */
  def length(): Double = {
    if (tail() != null) {
      head().pnt.distance(tail().pnt)
    } else {
      -1
    }
  }

  /**
   * Returns the length squared of this half-edge.
   *
   * @return half-edge length squared
   */
  def lengthSquared(): Double = {
    if (tail() != null) {
      head().pnt.distanceSquared(tail().pnt)
    } else {
      -1
    }
  }
}
