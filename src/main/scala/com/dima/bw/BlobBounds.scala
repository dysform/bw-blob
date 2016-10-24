package com.dima.bw

import java.io.File

object BlobBounds {
  def main(args: Array[String]): Unit = {
    val file = new File(args(0))
    val grid = GridGen.fromFile(file)

    val resultSlow = BlobFinder.naive(grid)
    val resultFast = BlobFinder.heuristic(grid)

    println("Naive: " + resultSlow)
    println("Fast:  " + resultFast)
  }
}

case class Result(boundary: Boundary, visited: Map[Coord, Boolean], path: List[Coord])  {
  override def toString =
    List("Reads: ",visited.size,
      " top: ",boundary.top,
      " left: " + boundary.left,
      " bottom: ",boundary.bottom,
      " right: ",boundary.right).mkString/*,
    " path:\n",r.path.reverse.mkString("\n")).mkString*/
}

object BlobFinder {
  def naive(grid: Grid): Result = {
    val b1: Boundary = new Boundary()

    val coords = {
      for {i <- 0 until grid.size; j <- 0 until grid.size}
        yield Coord(i, j)
    }.toList

    Result(b1.updated(coords.filter(c => grid(c))), coords.map(c => (c, grid(c))).toMap, Nil)
  }

  def breadth(grid: Grid): Result = {
    val firstMoveOpt = grid.findFirst()

    def search(queue: List[Coord], visited: Map[Coord, Boolean], boundary: Boundary, path: List[Coord]): Result = {
      queue match {
        case Nil =>
          Result(boundary, visited, path)
        case head :: tail =>
          val moves = grid.moves(head)
          val validMoves = moves.filterNot(c => visited.contains(c))
          val pathMoves = validMoves.filter(m => grid(m))
          search(
            tail ++ pathMoves,
            visited ++ validMoves.map(m => (m, grid(m))),
            boundary.updated(head),
            head :: path)
      }
    }

    firstMoveOpt.fold(Result(new Boundary(Coord(-1, -1)), Map.empty, Nil))(fm =>
      search(List(fm.initial), fm.visited, fm.boundary, Nil))
  }

  def heuristic(grid: Grid): Result = {
    val firstMoveOpt = grid.findFirst()

    def search(queue: List[Coord], visited: Map[Coord, Boolean], boundary: Boundary, path: List[Coord]): Result = {
      queue match {
        case Nil =>
          grid.boundaryMove(boundary, visited).fold(
            Result(boundary, visited, path))(c => {
            search(List(c), visited.updated(c, true), boundary.updated(c), path)
          })
        case head :: tail =>
          val moves = grid.moves(head)
          val validMoves = moves.filterNot(c => visited.contains(c) || boundary.containsCoord(c))
          val pathMoves = validMoves.filter(m => grid(m))
          search(
            tail ++ pathMoves,
            visited ++ validMoves.map(m => (m, grid(m))),
            boundary.updated(pathMoves),
            head :: path)
      }
    }

    firstMoveOpt.fold(Result(new Boundary(Coord(-1, -1)), Map.empty, Nil))(fm =>
      search(List(fm.initial), fm.visited, fm.boundary, Nil))
  }
}

case class Coord(y: Int, x: Int)

case class Boundary(left: Int, top: Int, right: Int, bottom: Int) {
  def this(coord: Coord) = this(coord.x, coord.y, coord.x, coord.y)

  def this() = this(Integer.MAX_VALUE, Integer.MAX_VALUE, -1, -1)

  def containsCoord(coord: Coord): Boolean =
    coord.x >= left &&
      coord.x <= right &&
      coord.y >= top &&
      coord.y <= bottom

  def updated(coord: Coord): Boundary = updated(List(coord))

  def updated(coords: List[Coord]): Boundary = {
    if (coords.isEmpty)
      this
    else {
      val l = Math.min(left, coords.map(_.x).min)
      val r = Math.max(right, coords.map(_.x).max)
      val t = Math.min(top, coords.map(_.y).min)
      val b = Math.max(bottom, coords.map(_.y).max)

      Boundary(l, t, r, b)
    }
  }

  def toStream =
    (left to right).toStream.map(Coord(top, _)) ++
      (top to bottom).toStream.map(Coord(_, left)) ++
      (top to bottom).toStream.map(Coord(_, right)) ++
      (left to right).toStream.map(Coord(bottom, _))
}

case class Grid(size: Int) {
  val data: Array[Boolean] = new Array(size * size)

  def apply(coord: Coord) = data(coord.y * size + coord.x)

  def set(coord: Coord) = data(coord.y * size + coord.x) = true

  def getCoord(n: Int) = Coord(n / size, n % size)

  def left(p: Coord) = Coord(p.y, p.x - 1)

  def right(p: Coord) = Coord(p.y, p.x + 1)

  def down(p: Coord) = Coord(p.y + 1, p.x)

  def up(p: Coord) = Coord(p.y - 1, p.x)

  def legal(p: Coord): Boolean =
    p.x >= 0 &&
      p.y >= 0 &&
      p.x < size &&
      p.y < size

  case class FirstMove(initial: Coord, visited: Map[Coord, Boolean], boundary: Boundary)

  def findFirst(): Option[FirstMove] = {
    val points = scala.util.Random.shuffle((0 until size * size).toList)
    //val points = (0 until size*size).toList

    val visited = points.takeWhile(p => !data(p)).map(n => getCoord(n)).map(coord => (coord, this (coord))).toMap

    if (visited.size == size * size)
      None
    else
      Some(FirstMove(
        getCoord(points(visited.size)),
        visited,
        new Boundary(getCoord(points(visited.size)))
      ))
  }

  def moves(p: Coord): List[Coord] =
    List(left(p), right(p), up(p), down(p)).filter(legal)

  def boundaryMove(boundary: Boundary, visited: Map[Coord, Boolean]): Option[Coord] = {
    val stream = boundary.toStream.filterNot(visited.contains).filter(this (_))

    if (stream.nonEmpty)
      Option(stream.head)
    else
      None
  }

  override def toString = {
    val b = new StringBuilder()

    for (i <- 0 until size) {
      for (j <- 0 until size) {
        if (this (Coord(i, j)))
          b.append("1")
        else
          b.append("0")
      }
      b.append("\n")
    }

    b.toString
  }
}

object GridGen {
  def fromFile(file: File): Grid = {
    val lines = io.Source.fromFile(file).getLines().toList

    val grid = Grid(lines.length)

    for (i <- lines.indices) {
      for (j <- lines.indices) {
        if (lines(i)(j) == '1')
          grid.set(Coord(i, j))
      }
    }

    grid
  }

  def random(size: Int): Grid = {
    val grid: Grid = Grid(size)

    var current = grid.getCoord(Math.abs(scala.util.Random.nextInt()) % (size * size))
    val numMoves = Math.abs(scala.util.Random.nextInt()) % (size * size)

    for (i <- 0 until numMoves) {
      grid.set(current)
      val moves = grid.moves(current)
      val index = Math.abs(scala.util.Random.nextInt()) % moves.size
      current = moves(index)
    }

    grid
  }
}