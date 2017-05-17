import java.io.FileWriter
import java.util.Scanner

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import java.io.FileOutputStream
import java.io.PrintWriter
import java.io.File

object PageRankAssign {

  def appendtoFile(fileName: String, nNodes: Int, nEdges: Int) {
    val fw = new PrintWriter(new FileOutputStream(new File(fileName)))
    try {
      var i = 0;
      fw.write(nNodes + "\n")

      while (i < nEdges) {
        val writeNextInt = new scala.util.Random().nextInt(nNodes.toInt)
        val writeEdgeNextInt = new scala.util.Random().nextInt(nNodes.toInt)
        if (writeNextInt.!=(writeEdgeNextInt)) {
          fw.write(writeNextInt + " " + writeEdgeNextInt + "\n")
          i = i + 1
        }
      }
      fw.flush()
	  fw.close()
    } catch {
      case t: Throwable => t.printStackTrace()
    } 
  }
  def buildSparse(fileName: String): (Int, mutable.HashMap[(Int, Int), Double], mutable.HashMap[Int, Double]) = {

    val scan = new Scanner(io.Source.fromFile(fileName).bufferedReader())
    val s = scan.nextInt
    val A = new mutable.HashMap[(Int, Int), Int]
    val rows = new mutable.HashMap[Int, Int]

    while (scan.hasNextInt) {
      val (row, column) = scan.nextInt -> scan.nextInt
      rows(row) = rows.get(row).getOrElse(0) + 1
      A(row -> column) = A.get(row -> column).getOrElse(0) + 1
    }

    val H = A.map {
      case ((row, column), value) =>
        (row -> column) -> value.toDouble / rows(row)
    }

    val D = new mutable.HashMap[Int, Double]
    (0 until s).foreach { row =>
      if (!rows.contains(row)) {
        D(row) = 1d / s
      }
    }

    (s, H, D)
  }

  def main(args: Array[String]): Unit = {

    while (true) {
      println("Enter your choice,\n 1. Want Page ranking for Wiki Data\n 2.Page ranking for random data \n 3.Exit")
      val choice = scala.io.StdIn.readLine().toInt

      if (choice.==(2)) {
        println("Enter the number of nodes you want: ")
        val nNodes = scala.io.StdIn.readLine().toInt
        println("Enter number of edges you want: ")
        val nEdges = scala.io.StdIn.readLine().toInt
        if (nEdges.<(nNodes)) {
          appendtoFile("xyz.txt", nNodes, nEdges)

          var (s, h, d) = buildSparse("xyz.txt")
          calPageRank(s, h, d)
        } else {
          println("Number of edges can't be greater than the number of nodes")
        }
      } else if (choice.==(1)) {
        println("The numbers represent the respective pages from A-k")
        var (s, h, d) = buildSparse("wiki.txt")
        calPageRank(s, h, d)
      } else if (choice.==(3)) {
        System.exit(1);
      } else {
        println("Enter valid choice")
      }
    }
  }
  def calPageRank(s: Int, h: mutable.HashMap[(Int, Int), Double], d: mutable.HashMap[Int, Double]): Unit = {
    val dampingValue = 0.85d
    var initialPageRank = ArrayBuffer.fill(s)(1d * 100 / s)
    val outBoundLink = ArrayBuffer.fill(s)(0d)
    var sum = 0d

    (0 until 1000).foreach { index =>
      sum = initialPageRank.sum * ((1d - dampingValue) / s) //(1-dampValue/N)*initialPageRank
      initialPageRank.transform(dampingValue *)

      outBoundLink.transform(i => 0d)
      h.foreach {
        case ((row, column), value) =>
          outBoundLink(column) += initialPageRank(row) * value
      }
      initialPageRank = outBoundLink.map(sum + d.map { case (row, value) => initialPageRank(row) * value }.sum +)
    }

    println("For random links between the " + s + " nodes respective page rank values are:")
    initialPageRank.zipWithIndex.sorted.map {
      case (index, value) =>
        println(s"$value: ${index.formatted("%.3f")}%")
    }

  }

}