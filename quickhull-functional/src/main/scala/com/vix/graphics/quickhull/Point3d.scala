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
 * A three-element spatial point.
 *
 * The only difference between a point and a vector is in the the way it is
 * transformed by an affine transformation. Since the transform method is not
 * included in this reduced implementation for QuickHull3D, the difference is
 * purely academic.
 *
 * @author John E. Lloyd, Fall 2004
 */
class Point3d extends Vector3d {

  /**
   * Creates a Point3d by copying a vector
   *
   * @param v
   *            vector to be copied
   */
  def this(v: Vector3d) {
    this()
    set(v)
  }

  /**
   * Creates a Point3d with the supplied element values.
   *
   * @param x
   *            first element
   * @param y
   *            second element
   * @param z
   *            third element
   */
  def this(x: Double, y: Double, z: Double) {
    this()
    set(x, y, z)
  }
}
