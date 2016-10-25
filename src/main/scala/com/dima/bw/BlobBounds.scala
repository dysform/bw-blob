package com.dima.bw

import java.io.File
import scala.util.Random

object BlobBounds {
  def main(args: Array[String]): Unit = {
    val file = new File(args(0))
    val grid = GridGen.fromFile(file)

    val resultSlow = BlobFinder.naive(grid)
    val resultFast = BlobFinder.smart(grid)

    println("Naive: " + resultSlow)
    println("Fast:  " + resultFast)
  }
}

case class Result(boundary: Boundary, reads: Int)  {
  override def toString =
    List("Reads: ",reads," ",boundary).mkString
}

case class Coord(y: Int, x: Int)
case class Move(coord: Option[Coord], visited: Set[Coord])

object BlobFinder {
  def naive(grid: Grid): Result = {
    val coords = {
      for {i <- 0 until grid.size; j <- 0 until grid.size}
        yield Coord(i, j)
    }.toList

    Result(
      new Boundary().updated(coords.filter(c => grid(c))),
      coords.size)
  }

  def smart(grid: Grid): Result = {
    def search(queue: List[Coord], visited: Set[Coord], boundary: Boundary, path: List[Coord]): Result = {
      queue match {
        case Nil =>
          val move = grid.boundaryMove(boundary, visited)
          if (move.coord == None)
            Result(boundary, visited.size + move.visited.size)
          else
            search(List(move.coord.get), visited ++ move.visited, boundary.updated(move.coord.get), path)

        case head :: tail =>
          val validMoves = grid.moves(head)
            .filterNot(c => visited.contains(c) || boundary.contains(c))
          val pathMoves = validMoves.filter(m => grid(m)) // cell read
          search(
            tail ++ pathMoves,
            visited ++ validMoves,
            boundary.updated(pathMoves),
            head :: path)
      }
    }

    val move = grid.firstMove()
    if (move.coord == None)
      Result(new Boundary(), move.visited.size)
    else
      search(List(move.coord.get), move.visited, new Boundary(move.coord.get), Nil)
  }
}

case class Boundary(top: Int, left: Int, bottom: Int, right: Int) {
  def this(coord: Coord) = this(coord.y, coord.x, coord.y, coord.x)
  def this() = this(Integer.MAX_VALUE, Integer.MAX_VALUE, -1, -1)

  def contains(coord: Coord): Boolean =
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

      Boundary(t, l, b, r)
    }
  }

  override def toString =
    List("top: ",top,
    " left: " + left,
    " bottom: ",bottom,
    " right: ",right).mkString

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

  def coord(n: Int) = Coord(n / size, n % size)

  def legal(p: Coord): Boolean =
    p.x >= 0 &&
      p.y >= 0 &&
      p.x < size &&
      p.y < size

  def firstMove(): Move = {
    val points = Random.shuffle((0 until size * size).toList).map(coord).toStream
    //val points = (0 until size*size).toList.map(coord(_)).toStream
    move(points)
  }

  def boundaryMove(boundary: Boundary, visited: Set[Coord]): Move =
    move(boundary.toStream.filterNot(visited.contains))

  def move(stream: Stream[Coord]) = {
    val visited = stream.takeWhile(!this(_)).toSet // cell reads

    if(stream.size!=visited.size)
      Move(Option(stream(visited.size)),visited + stream(visited.size))
    else
      Move(None,visited)
  }

  def moves(p: Coord): List[Coord] = {
    val left = Coord(p.y, p.x - 1)
    val right = Coord(p.y, p.x + 1)
    val down = Coord(p.y + 1, p.x)
    val up = Coord(p.y - 1, p.x)
    List(left, right, up, down).filter(legal)
  }

  override def toString = {
    def str(b: Boolean) = if(b) "1" else 0

    (0 until size).map(y=>{
      (0 until size).map(x=>str(this(Coord(y,x)))).mkString
    }).mkString("\n")
  }
}

object GridGen {
  def fromFile(file: File): Grid =
    fromLines(io.Source.fromFile(file).getLines().toList)

  def fromLines(lines: List[String]): Grid = {
    val grid = Grid(lines.length)

    for (i <- lines.indices;j <- lines.indices)
        if (lines(i)(j) == '1')
          grid.set(Coord(i, j))

    grid
  }

  def random(size: Int): Grid = {
    val grid: Grid = Grid(size)

    var current = grid.coord(Math.abs(Random.nextInt()) % (size * size))
    val numMoves = Math.abs(Random.nextInt()) % (size * size)

    for (i <- 0 until numMoves) {
      grid.set(current)
      val moves = grid.moves(current)
      val index = Math.abs(Random.nextInt()) % moves.size
      current = moves(index)
    }

    grid
  }
}