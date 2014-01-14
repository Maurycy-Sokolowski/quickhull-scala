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
 * Maintains a double-linked list of vertices for use by QuickHull3D
 */
class VertexList {

  private var head: Vertex = _

  private var tail: Vertex = _

  /**
   * Clears this list.
   */
  def clear() {
    head = null
    tail = null
  }

  /**
   * Adds a vertex to the end of this list.
   */
  def add(vtx: Vertex) {
    if (head == null) {
      head = vtx
    } else {
      tail.next = vtx
    }
    vtx.prev = tail
    vtx.next = null
    tail = vtx
  }

  /**
   * Adds a chain of vertices to the end of this list.
   */
  def addAll(vtxnmut: Vertex) {
    var vtx = vtxnmut
    if (head == null) {
      head = vtx
    } else {
      tail.next = vtx
    }
    vtx.prev = tail
    while (vtx.next != null) {
      vtx = vtx.next
    }
    tail = vtx
  }

  /**
   * Deletes a vertex from this list.
   */
  def delete(vtx: Vertex) {
    if (vtx.prev == null) {
      head = vtx.next
    } else {
      vtx.prev.next = vtx.next
    }
    if (vtx.next == null) {
      tail = vtx.prev
    } else {
      vtx.next.prev = vtx.prev
    }
  }

  /**
   * Deletes a chain of vertices from this list.
   */
  def delete(vtx1: Vertex, vtx2: Vertex) {
    if (vtx1.prev == null) {
      head = vtx2.next
    } else {
      vtx1.prev.next = vtx2.next
    }
    if (vtx2.next == null) {
      tail = vtx1.prev
    } else {
      vtx2.next.prev = vtx1.prev
    }
  }

  /**
   * Inserts a vertex into this list before another specificed vertex.
   */
  def insertBefore(vtx: Vertex, next: Vertex) {
    vtx.prev = next.prev
    if (next.prev == null) {
      head = vtx
    } else {
      next.prev.next = vtx
    }
    vtx.next = next
    next.prev = vtx
  }

  /**
   * Returns the first element in this list.
   */
  def first(): Vertex = head

  /**
   * Returns true if this list is empty.
   */
  def isEmpty(): Boolean = head == null
}
