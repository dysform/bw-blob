package com.dima.bw

import java.io.File

object BlobBounds {
  def main(args: Array[String]): Unit = {
    val file = new File("src/test/resources/blob2.txt")
    val grid = GridGen.fromFile(file)
    val lheuristic = heuristic(grid)

    println("Heuri: " + makeString(lheuristic))
   }


  def makeString(list: List[Int]) =
    "Cells: " + list(0) + " top: " + list(1) + " left: " + list(2) + " bottom: " + list(3) + " right: " + list(4)

  def naive(grid: Grid): List[Int] = {
    val b1: Boundary = new Boundary()

    val reads = grid.data.length
    val coords = {
      for{i<-0 until grid.size;j<-0 until grid.size}
      yield Coord(i,j)}
      .filter(grid(_)).toList

    val b2: Boundary = b1.update(coords,grid)

    List(reads,b2.top,b2.left,b2.bottom,b2.right)
  }

  def breadth(grid: Grid): List[Int] = {
    val firstMoveOpt = grid.findFirst()

    def walk(queue: List[Coord], visited: Map[Coord,Boolean], boundary: Boundary): (Boundary,Int) = {
      queue match {
        case Nil =>
          (boundary, visited.size)
        case head :: tail => {
          val moves = grid.moves(head)
          val validMoves = moves.filterNot(c => visited.contains(c))
          val pathMoves = validMoves.filter(m => grid(m))
          walk(
            tail ++ pathMoves,
            visited ++ validMoves.map(m => (m, grid(m))),
            boundary.update(List(head), grid))
        }
      }
    }

    val result = firstMoveOpt.fold((new Boundary(Coord(-1,-1)),-1))(fm=>
      walk(List(fm.initial), fm.visited,fm.boundary))

    List(result._2,result._1.top,result._1.left,result._1.bottom,result._1.right)
  }

  def heuristic(grid: Grid): List[Int] = {
    val firstMoveOpt = grid.findFirst()

    def walk(queue: List[Coord], visited: Map[Coord,Boolean], boundary: Boundary): (Boundary,Int) = {
      queue match {
        case Nil => {
            grid.boundaryMove(boundary, visited).fold(
            (boundary, visited.size))(c=>{
            walk(List(c), visited.updated(c, true), boundary.update(List(c), grid))
          })
        }
        case head :: tail => {
         // println(head + " " + boundary+" "+visited.filter(v=>v._2).map(_._1))
          val moves = grid.moves(head)
          val validMoves = moves.filterNot(c => visited.contains(c) || boundary.inside(c))
          val pathMoves = validMoves.filter(m => grid(m))
          walk(
            tail ++ pathMoves,
            visited ++ validMoves.map(m => (m, grid(m))),
            boundary.update(pathMoves, grid))
        }
      }
    }

    val result = firstMoveOpt.fold((new Boundary(Coord(-1,-1)),-1))(fm=>
      walk(List(fm.initial), fm.visited,fm.boundary))


    List(result._2,result._1.top,result._1.left,result._1.bottom,result._1.right)
  }


}

case class Coord(y: Int, x: Int)

case class Boundary(left: Int, top: Int, right: Int, bottom: Int) {
  def this(coord: Coord) = this(coord.x, coord.y, coord.x, coord.y)
  def this() = this(Integer.MAX_VALUE,Integer.MAX_VALUE,-1,-1)

  def inside(coord: Coord): Boolean =
    (coord.x >= left && coord.x <= right && coord.y >= top && coord.y <= bottom)

  def update(coords: List[Coord], grid: Grid): Boundary = {
    if(coords.isEmpty)
      this
    else {
      val l = Math.min(left, coords.map(_.x).min)
      val r = Math.max(right, coords.map(_.x).max)
      val t = Math.min(top, coords.map(_.y).min)
      val b = Math.max(bottom, coords.map(_.y).max)

      Boundary(l, t, r, b)
    }
  }
}

case class Grid(size: Int) {
  val data: Array[Boolean] = new Array(size*size)

  def apply(coord: Coord) = data(coord.y * size + coord.x)
  def get(n: Int): Boolean = data(n)
  def set(coord: Coord) = data(coord.y * size + coord.x) = true
  def set(n: Int): Unit = data(n) = true

  def getCoord(n: Int) = Coord(n/size, n%size)

  def left(p: Coord) = Coord(p.y,p.x-1)
  def right(p: Coord) = Coord(p.y,p.x+1)
  def down(p: Coord) = Coord(p.y+1,p.x)
  def up(p: Coord) = Coord(p.y-1,p.x)

  def valid(p: Coord): Boolean =
    p.x >= 0 &&
      p.y >= 0 &&
      p.x < size &&
      p.y < size

  case class FirstMove(initial: Coord, visited: Map[Coord,Boolean], boundary: Boundary)

  def findFirst(): Option[FirstMove] = {
    //val points = scala.util.Random.shuffle((0 until size*
    val points = (0 until size*size).toList

    val visited = points.takeWhile(p => !data(p)).map(n=>getCoord(n)).map(coord=>(coord,this(coord))).toMap

    if(visited.size==size*size)
      None
    else
      Some(FirstMove(
        getCoord(points(visited.size)),
        visited,
        new Boundary(getCoord(points(visited.size)))
      ))
  }

  def moves(p: Coord): List[Coord] =
    List(left(p),right(p),up(p),down(p)).filter(valid)

  def boundaryMove(boundary: Boundary, visited: Map[Coord,Boolean]): Option[Coord] = {
    val top = (boundary.left to boundary.right).map(Coord(boundary.top,_))
    val left = (boundary.top to boundary.bottom).map(Coord(_,boundary.left))
    val right = (boundary.top to boundary.bottom).map(Coord(_,boundary.right))
    val bottom = (boundary.left to boundary.right).map(Coord(boundary.bottom,_))

    val all = (top ++ left ++ right ++ bottom).filter(!visited.contains(_))

    val stream = all.toStream.filter(this(_))

    if(!stream.isEmpty)
      Option(stream.head)
     else
      None
  }

  override def toString = {
    val b = new StringBuilder()

    for(i<-0 until size) {
      for(j<-0 until size) {
        if(this(Coord(i,j)))
          b.append("1")
        else
          b.append("0")
      }
      b.append("\n")
    }

    b.toString
  }
}

case object GridGen {
  def fromFile(file: File): Grid = {
    val lines = io.Source.fromFile(file).getLines().toList

    val grid = new Grid(lines.length)

    for (i <- lines.indices) {
      for (j <- lines.indices) {
        if (lines(i)(j) == '1')
          grid.set(Coord(i, j))
      }
    }

    grid
  }

  def random(size: Int): Grid = {
    val grid: Grid = new Grid(size)

    var current = grid.getCoord(Math.abs(scala.util.Random.nextInt())%(size*size))
    val numMoves = Math.abs(scala.util.Random.nextInt())%((size*size))

    for(i<-0 until numMoves) {
      grid.set(current)
      val moves = grid.moves(current)
      val index = Math.abs(scala.util.Random.nextInt()) % moves.size
      current = moves(index)
    }

    grid
  }
}