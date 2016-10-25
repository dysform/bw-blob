package com.dima.bw

import java.io.File
import scala.util.Random

object BlobBounds {
  def main(args: Array[String]): Unit = {
    val file = new File(args(0))
    val grid = GridGen.fromFile(file)

    val resultSlow = BlobFinder.naive(grid)
    val resultSmart = BlobFinder.smart(grid)

    println("Naive: " + resultSlow)
    println("Smart:  " + resultSmart)
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
      for (i <- 0 until grid.size ;j <- 0 until grid.size)
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
            search(
              List(move.coord.get),
              (visited ++ move.visited) + move.coord.get,
              boundary.updated(move.coord.get), path)

        case head :: tail =>
          val validMoves =
            grid.moves(head)
            .filterNot(visited.contains(_))
            .filterNot(boundary.contains(_))
          val pathMoves = validMoves.filter(grid(_)) // cell read
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
      search(List(move.coord.get), move.visited+move.coord.get, new Boundary(move.coord.get), Nil)
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

  def updated(coord: Coord): Boundary = {
    val l = Math.min(left, coord.x)
    val r = Math.max(right, coord.x)
    val t = Math.min(top, coord.y)
    val b = Math.max(bottom, coord.y)

    Boundary(t, l, b, r)
  }

  def updated(coords: List[Coord]): Boundary =
    coords match {
      case Nil => this
      case _ => coords.foldLeft(this)((b,c)=>b.updated(c))
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

case class Grid(size: Int, coords: List[Coord]) {
  val data: Array[Boolean] = new Array(size * size)
  coords.foreach(coord=>data(coord.y * size + coord.x) = true)

  def apply(coord: Coord) = data(coord.y * size + coord.x)

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
    Move(stream.drop(visited.size).headOption,visited)
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
    val coords = for {i <- lines.indices
                      j <- lines.indices
                      if (lines(i)(j) == '1')
    } yield (Coord(i, j))

    Grid(lines.size, coords.toList)
  }

  def random(size: Int): Grid = {
    val fake = Grid(size, Nil)
    val current = fake.coord(Math.abs(Random.nextInt()) % (size * size))
    val numMoves = Math.abs(Random.nextInt()) % (size * size)

    def coords(current: List[Coord]): List[Coord] =
      if(numMoves==0)
        Nil
      else if(current.size==numMoves)
        current
      else {
        val moves = fake.moves(current.head)
        val index = Math.abs(Random.nextInt()) % moves.size
        coords(moves(index) :: current)
      }

    Grid(size,coords(List(current)))
  }
}