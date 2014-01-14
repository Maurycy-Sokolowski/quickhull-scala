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
 * Represents vertices of the hull, as well as the points from which it is
 * formed.
 *
 * @author John E. Lloyd, Fall 2004
 */
class Vertex {

  /**
   * Spatial point associated with this vertex.
   */
  var pnt: Point3d = new Point3d()

  /**
   * Back index into an array.
   */
  var index: Int = _

  /**
   * List forward link.
   */
  var prev: Vertex = _

  /**
   * List backward link.
   */
  var next: Vertex = _

  /**
   * Current face that this vertex is outside of.
   */
  var face: Face = _

  /**
   * Constructs a vertex with the specified coordinates and index.
   */
  def this(x: Double,
    y: Double,
    z: Double,
    idx: Int) {
    this()
    pnt = new Point3d(x, y, z)
    index = idx
  }
}
