package com.github.arzt.files

import java.io.File
import java.nio.file.{Path, Paths}
import scala.annotation.tailrec

def listFilesRec(file: File): LazyList[File] =
  if (file.isDirectory)
    LazyList.from(file.listFiles()).flatMap(listFilesRec)
  else
    LazyList(file)

@tailrec
def listFilesTailRec(file: LazyList[File], acc: LazyList[File]): LazyList[File] =
  file match
    case LazyList() =>
      acc
    case head #:: tail if head.isDirectory =>
      println(head)
      listFilesTailRec(head.listFiles() ++: tail, acc)
    case head #:: tail =>
      listFilesTailRec(tail, head +: acc)

object LoadFilles:
  def main(args: Array[String]): Unit =
    println("test")
    val hui = Paths.get("/media/frog/musik")
    val res = listFilesTailRec(LazyList(hui.toFile), LazyList()).map(_.getCanonicalPath)
    val result = res.map(
      x => {
        //println(x)
        x
      }
    ).toArray
    print(result.length)
