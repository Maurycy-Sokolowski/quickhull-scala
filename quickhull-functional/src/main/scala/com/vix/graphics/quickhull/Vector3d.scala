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

import java.util.Random
import Vector3d._

object Vector3d {

  /**
   * Precision of a double.
   */
  private val DOUBLE_PREC = 2.2204460492503131e-16
}

/**
 * A three-element vector. This class is actually a reduced version of the
 * Vector3d class contained in the author's matlib package (which was partly
 * inspired by javax.vecmath). Only a mininal number of methods which are
 * relevant to convex hull generation are supplied here.
 *
 * @author John E. Lloyd, Fall 2004
 */
class Vector3d {

  /**
   * First element
   */
  var x: Double = _

  /**
   * Second element
   */
  var y: Double = _

  /**
   * Third element
   */
  var z: Double = _

  /**
   * Creates a 3-vector by copying an existing one.
   *
   * @param v
   *            vector to be copied
   */
  def this(v: Vector3d) {
    this()
    set(v)
  }

  /**
   * Creates a 3-vector with the supplied element values.
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

  /**
   * Gets a single element of this vector. Elements 0, 1, and 2 correspond to
   * x, y, and z.
   *
   * @param i
   *            element index
   * @return element value throws ArrayIndexOutOfBoundsException if i is not
   *         in the range 0 to 2.
   */
  def get(i: Int): Double = i match {
    case 0 => {
      x
    }
    case 1 => {
      y
    }
    case 2 => {
      z
    }
    case _ => {
      throw new ArrayIndexOutOfBoundsException(i)
    }
  }

  /**
   * Sets the values of this vector to those of v1.
   *
   * @param v1
   *            vector whose values are copied
   */
  def set(v1: Vector3d) {
    x = v1.x
    y = v1.y
    z = v1.z
  }

  /**
   * Adds vector v1 to v2 and places the result in this vector.
   *
   * @param v1
   *            left-hand vector
   * @param v2
   *            right-hand vector
   */
  def add(v1: Vector3d, v2: Vector3d) {
    x = v1.x + v2.x
    y = v1.y + v2.y
    z = v1.z + v2.z
  }

  /**
   * Adds this vector to v1 and places the result in this vector.
   *
   * @param v1
   *            right-hand vector
   */
  def add(v1: Vector3d) {
    x += v1.x
    y += v1.y
    z += v1.z
  }

  /**
   * Subtracts vector v1 from v2 and places the result in this vector.
   *
   * @param v1
   *            left-hand vector
   * @param v2
   *            right-hand vector
   */
  def sub(v1: Vector3d, v2: Vector3d) {
    x = v1.x - v2.x
    y = v1.y - v2.y
    z = v1.z - v2.z
  }

  /**
   * Subtracts v1 from this vector and places the result in this vector.
   *
   * @param v1
   *            right-hand vector
   */
  def sub(v1: Vector3d) {
    x -= v1.x
    y -= v1.y
    z -= v1.z
  }

  /**
   * Scales the elements of this vector by <code>s</code>.
   *
   * @param s
   *            scaling factor
   */
  def scale(s: Double) {
    x = s * x
    y = s * y
    z = s * z
  }

  /**
   * Scales the elements of vector v1 by <code>s</code> and places the results
   * in this vector.
   *
   * @param s
   *            scaling factor
   * @param v1
   *            vector to be scaled
   */
  def scale(s: Double, v1: Vector3d) {
    x = s * v1.x
    y = s * v1.y
    z = s * v1.z
  }

  /**
   * Returns the 2 norm of this vector. This is the square root of the sum of
   * the squares of the elements.
   *
   * @return vector 2 norm
   */
  def norm(): Double = Math.sqrt(x * x + y * y + z * z)

  /**
   * Returns the square of the 2 norm of this vector. This is the sum of the
   * squares of the elements.
   *
   * @return square of the 2 norm
   */
  def normSquared(): Double = x * x + y * y + z * z

  /**
   * Returns the Euclidean distance between this vector and vector v.
   *
   * @return distance between this vector and v
   */
  def distance(v: Vector3d): Double = {
    val dx = x - v.x
    val dy = y - v.y
    val dz = z - v.z
    Math.sqrt(dx * dx + dy * dy + dz * dz)
  }

  /**
   * Returns the squared of the Euclidean distance between this vector and
   * vector v.
   *
   * @return squared distance between this vector and v
   */
  def distanceSquared(v: Vector3d): Double = {
    val dx = x - v.x
    val dy = y - v.y
    val dz = z - v.z
    (dx * dx + dy * dy + dz * dz)
  }

  /**
   * Returns the dot product of this vector and v1.
   *
   * @param v1
   *            right-hand vector
   * @return dot product
   */
  def dot(v1: Vector3d): Double = x * v1.x + y * v1.y + z * v1.z

  /**
   * Normalizes this vector in place.
   */
  def normalize() {
    val lenSqr = x * x + y * y + z * z
    val err = lenSqr - 1
    if (err > (2 * DOUBLE_PREC) || err < -(2 * DOUBLE_PREC)) {
      val len = Math.sqrt(lenSqr)
      x /= len
      y /= len
      z /= len
    }
  }

  /**
   * Sets the elements of this vector to zero.
   */
  def setZero() {
    x = 0
    y = 0
    z = 0
  }

  /**
   * Sets the elements of this vector to the prescribed values.
   *
   * @param x
   *            value for first element
   * @param y
   *            value for second element
   * @param z
   *            value for third element
   */
  def set(x: Double, y: Double, z: Double) {
    this.x = x
    this.y = y
    this.z = z
  }

  /**
   * Computes the cross product of v1 and v2 and places the result in this
   * vector.
   *
   * @param v1
   *            left-hand vector
   * @param v2
   *            right-hand vector
   */
  def cross(v1: Vector3d, v2: Vector3d) {
    val tmpx = v1.y * v2.z - v1.z * v2.y
    val tmpy = v1.z * v2.x - v1.x * v2.z
    val tmpz = v1.x * v2.y - v1.y * v2.x
    x = tmpx
    y = tmpy
    z = tmpz
  }

  /**
   * Sets the elements of this vector to uniformly distributed random values
   * in a specified range, using a supplied random number generator.
   *
   * @param lower
   *            lower random value (inclusive)
   * @param upper
   *            upper random value (exclusive)
   * @param generator
   *            random number generator
   */
  def setRandom(lower: Double, upper: Double) {
    val range = upper - lower
    x = math.random * range + lower
    y = math.random * range + lower
    z = math.random * range + lower
  }

  /**
   * Returns a string representation of this vector, consisting of the x, y,
   * and z coordinates.
   *
   * @return string representation
   */
  override def toString(): String = x + " " + y + " " + z
}
