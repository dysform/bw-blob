package com.dima.bw

import java.io.File

object BlobBounds {
  def main(args: Array[String]): Unit = {

    //val file = new File("src/test/resources/blob1.txt")

    //val grid = GridGen.fromFile(file)

    val grid = GridGen.random(10)

    println("Naive: "+naive(grid))

    var ave = 0;
    for(i<-0 until 100) {
      val list = walk(grid)
      ave+=list(0)
    }

    println("Breadth: "+walk(grid) + " Average="+ave/100)

    println(grid)
    //println(GridGen.random(10))
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

  def walk(grid: Grid): List[Int] = {
    val firstMove = grid.findFirst()

    val x = firstMove.initial(0)%grid.size
    val y = firstMove.initial(0)/grid.size

    def walk(queue: List[Int], visited: Map[Int,Boolean], boundary: Boundary): (Boundary,Map[Int,Boolean]) = {
      queue match {
        case Nil => {
          (boundary, visited)
        }
        case head :: tail => {
          val moves = grid.getNextMoves(head, visited, boundary)

          walk(tail++(moves._1.filter(_.valid).map(_.index)), (visited ++ moves._1.map(p=>(p.index,p.valid))), moves._2)
        }
      }
    }

    val result = walk(firstMove.initial, firstMove.visited,Boundary(x,y,x,y,grid.size))
    List(result._2.size,result._1.top,result._1.left,result._1.bottom,result._1.right)
  }
}

case class Point(index: Int, valid: Boolean)

case class Boundary(left: Int, top: Int, right: Int, bottom: Int, size: Int) {
  def inside(n: Int): Boolean = {
    val y = n/size
    val x = n%size

    (x > left && x < right && y > top && y < bottom)
  }

  def update(left: Option[Point], top: Option[Point],right: Option[Point],bottom: Option[Point]): Boundary = {
    new Boundary(
      left.fold(this.left)(p=>if(p.valid)Math.min(p.index%size,this.left)else this.left),
      top.fold(this.top)(p=>if(p.valid)Math.min(p.index/size,this.top)else this.top),
      right.fold(this.right)(p=>if(p.valid)Math.max(p.index%size,this.right)else this.right),
      bottom.fold(this.bottom)(p=>if(p.valid)Math.max(p.index/size,this.bottom)else this.bottom),
      this.size)
  }
}

case class Grid(size: Int) {
  val data: Array[Boolean] = new Array(size*size)

  def apply(y: Int, x: Int) = data(y * size + x)
  def get(n: Int): Boolean = data(n)
  def set(y: Int, x: Int) = data(y * size + x) = true
  def set(n: Int): Unit = data(n) = true

  case class FirstMove(initial: List[Int], visited: Map[Int,Boolean])

  def findFirst(): FirstMove = {
    val points = scala.util.Random.shuffle(0 to 99)

    val visited = points.takeWhile(p => !data(p)).map((_,false)).toList.toMap

    if(visited.size==points.size)
      FirstMove(Nil, visited)
    else
      FirstMove(List(points(visited.size)), visited)
  }

  def getNextMoves(n: Int, visited: Map[Int,Boolean], boundary: Boundary): (List[Point],Boundary) = {
    def nextMove(nextPoint: Int, outOfBounds: Boolean) =
      if(outOfBounds || visited.contains(nextPoint) /*|| boundary.inside(n)*/)
        None
      else
        Some(Point(nextPoint,data(nextPoint)))

    val y = n/size
    val x = n%size

    val left = nextMove((y*size)+(x-1),x==0)
    val up = nextMove((y-1)*size+x,y==0)
    val right = nextMove((y*size)+(x+1),x==size-1)
    val down = nextMove((y+1)*size+x,y==size-1)

    (List(left,up,right,down).filterNot(_==None).map(_.get),boundary.update(left,up,right,down))
  }

  override def toString = {
    val b = new StringBuilder()

    for(i<-0 until size) {
      for(j<-0 until size) {
        var k = "0"
        if(this(i,j))k="1"
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

    for (i <- 0 until lines.length) {
      for (j <- 0 until lines.length) {
        if (lines(i)(j) == '1')
          grid.set(i, j)
      }
    }

    grid
  }

  def random(size: Int): Grid = {
    var current = Math.abs(scala.util.Random.nextInt())%(size*size)
    val moves = Math.abs(scala.util.Random.nextInt())%(size*size)

    var points: List[Int] = Nil

    for(i<-0 until moves) {
      current = nextMove(current, size)
      points = current :: points
    }

    val grid: Grid = new Grid(size)
    points.foreach(grid.set(_))
    grid
  }

  def nextMove(n: Int, size: Int): Int = {
    val y = n/size
    val x = n%size

    val list = List(
    (y*size)+(x-1),
    (y-1)*size+x,
    (y*size)+(x+1),
    (y+1)*size+x)

    val k = Math.abs(scala.util.Random.nextInt())%4

    if(k==0 && x==0)
      n
    else if(k==1 && y==0)
      n
    else if(k==2 && x==size-1)
      n
    else if(k==3 &&y==size-1)
      n
    else
      list(k)
  }
}

