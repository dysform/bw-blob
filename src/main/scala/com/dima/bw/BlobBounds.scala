package com.dima.bw

import java.io.File

object BlobBounds {
  def main(args: Array[String]): Unit = {
    val file = new File("src/test/resources/blob1.txt")

    val lines = io.Source.fromFile(file).getLines().toList

    val blob = load(lines)

    println(naive(blob))

    var ave = 0;
    for(i<-0 until 100) {
      val list = walk(blob)
      println(list(0))
      ave+=list(0)
    }

    println("Average="+ave/100)
  }

  def naive(grid: Grid): List[Int] = {
    var right,bottom = -1
    var left,top = 10
    val reads = 100
    for(i<-0 to 9;j<-0 to 9) {
      if(grid(i,j)) {
        if(i<top)
          top = i
        if(i>bottom)
          bottom=i
        if(j<left)
          left=j
        if(j>right)
          right=j
      }
    }

    List(reads,top,left,bottom,right)
  }

  def naive2(grid: Map[Int,Boolean], size: Int): List[Int] = {
    var right,bottom = -1
    var left,top = 10
    val reads = 100
    grid.toStream.filter(_._2).map(_._1).foreach (p=> {
      val i = p/size
      val j = p%size
      if (i < top)
        top = i
      if (i > bottom)
        bottom = i
      if (j < left)
        left = j
      if (j > right)
        right = j
    })

    List(grid.size,top,left,bottom,right)
  }

  def walk(grid: Grid): List[Int] = {
    val firstMove = grid.findFirst()

    def walk(queue: List[Int], visited: Map[Int,Boolean]): Map[Int, Boolean] = {
      queue match {
        case Nil => visited
        case head :: tail => {
          val moves = grid.getNextMoves(head, visited)

          walk(tail++(moves.filter(_.valid).map(_.next)), (visited ++ moves.map(p=>(p.next,p.valid))))
        }
      }
    }

    val visited = walk(firstMove.initial, firstMove.visited)

    naive2(visited, grid.size)
  }

  def load(lines: List[String]): Grid = {
    val grid = new Grid(lines.length)

    for (i <- 0 until lines.length) {
      for (j <- 0 until lines.length) {
        if (lines(i)(j) == '1')
          grid.set(i, j)
      }
    }

    grid
  }
}

case class Grid(size: Int) {
  val data: Array[Boolean] = new Array(size*size)

  def apply(y: Int, x: Int): Boolean = data(y * size + x)
  def set(y: Int, x: Int): Unit = data(y * size + x) = true

  case class FirstMove(initial: List[Int], visited: Map[Int,Boolean])

  def findFirst(): FirstMove = {
    val points = scala.util.Random.shuffle(0 to 99)

    val visited = points.takeWhile(p => !data(p)).map((_,false)).toMap

    if(visited.size==points.size)
      FirstMove(Nil, visited)
    else
      FirstMove(List(points(visited.size)), visited)
  }

  case class Point(next: Int, valid: Boolean)

  def nextMove(nextPoint: Int, visited: Map[Int,Boolean], outOfBounds: Boolean) =
    if(outOfBounds || visited.contains(nextPoint))
      None
    else
      Some(Point(nextPoint,data(nextPoint)))

  def getNextMoves(n: Int, visited: Map[Int,Boolean]): List[Point] = {
    val y = n/size
    val x = n%size

    val left = nextMove((y*size)+(x-1),visited,x==0)
    val up = nextMove((y-1)*size+x,visited,y==0)
    val right = nextMove((y*size)+(x+1),visited,x==size-1)
    val down = nextMove((y+1)*size+x,visited,y==size-1)

    List(left,right,up,down).filterNot(_==None).map(_.get)
  }
}

case class RandomWalkGen(val size: Int=10) {
  def generate(): Array[Boolean] = {
    val start = Math.abs(scala.util.Random.nextInt())%size
    val moves = Math.abs(scala.util.Random.nextInt())%size

    new Array(1)
  }
}

