package com.dima.bw

import java.io.File

object BlobBounds {
  def main(args: Array[String]): Unit = {

    //val file = new File("src/test/resources/blob1.txt")

    //val grid = GridGen.fromFile(file)

    val grid = GridGen.random(10)

    println("Naive: "+naive(grid))

    var ave = 0
    for(i<-0 until 100) {
      val list = breadth(grid)
      ave+=list.head
    }

    println("Breadth: "+breadth(grid) + " Average="+ave/100)

    println(grid)
    //println(GridGen.random(10))
  }

  def naive(grid: Grid): List[Int] = {
    val b1: Boundary = new Boundary()

    val reads = grid.data.length
    val coords = {
      for{i<-0 to grid.size;j<-0 to grid.size}
      yield Coord(i,j)}
      .toList

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
          val moves = grid.getNextMoves(head)
          val validMoves = moves.filterNot(c=>visited.contains(c))
          val pathMoves = validMoves.filter(m=>grid(m))
          walk(
            tail++pathMoves,
            visited ++ validMoves.map(m=>(m,grid(m))),
            boundary.update(validMoves,grid))
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

/*  def inside(n: Int): Boolean = {
    val y = n/size
    val x = n%size

    (x > left && x < right && y > top && y < bottom)
  }*/

  def update(coords: List[Coord], grid: Grid): Boundary = {
    val l = Math.min(left, coords.map(_.x).min)
    val r = Math.max(right, coords.map(_.x).max)
    val t = Math.min(top, coords.map(_.y).min)
    val b = Math.max(bottom, coords.map(_.y).max)

    Boundary(l,t,r,b)
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
    !(p.x < 0 || p.y < 0 || p.x >= size || p.y >= size)

  case class FirstMove(initial: Coord, visited: Map[Coord,Boolean], boundary: Boundary)

  def findFirst(): Option[FirstMove] = {
    val points = scala.util.Random.shuffle(0 to 99)

    val visited = points.takeWhile(p => !data(p)).map(n=>getCoord(n)).map(coord=>(coord,this(coord))).toList.toMap

    if(visited.size==size*size)
      None
    else
      Some(FirstMove(
        getCoord(points(visited.size)),
        visited,
        new Boundary(getCoord(points(visited.size)))
      ))
  }

  def getNextMoves(p: Coord): List[Coord] =
    List(left(p),right(p),up(p),down(p)).filter(valid)

  override def toString = {
    val b = new StringBuilder()

    for(i<-0 until size) {
      for(j<-0 until size) {
        var k = "0"
        if(this(Coord(i,j)))k="1"
        b.append(k)
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
    val current = Math.abs(scala.util.Random.nextInt())%(size*size)
    val moves = Math.abs(scala.util.Random.nextInt())%(size*size)
    var points: List[Coord] = Nil
    val grid: Grid = new Grid(size)

    for(i<-0 until moves) {
      val nextMoves = grid.getNextMoves(grid.getCoord(current))
      points = nextMoves(  Math.abs(scala.util.Random.nextInt()) % nextMoves.size  ) :: points
    }

    points.foreach(coord=>grid.set(coord))
    grid
  }
}
