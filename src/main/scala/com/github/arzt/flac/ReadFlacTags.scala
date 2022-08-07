package com.github.arzt.flac

import com.github.arzt.flac

import java.io.{BufferedInputStream, File, FileInputStream}

object ReadFlacTags:

  def getBytes(f: File, size: Int = 1024*10): LazyList[Byte] =
    val is = new FileInputStream(f)
    val buf = new Array[Byte](size)
    LazyList
      .continually(is.read(buf))
      .takeWhile(_ > 0)
      .flatMap(buf.view.take)


  def main(args: Array[String]): Unit =
    val f = new File("/home/sebastian/Downloads/other/music/thermals/The+Thermals/The Thermals/2010 - Personal Life [FLAC]/01 - I'm Gonna Change Your Life.flac")
    //val f = new File("/home/sebastian/Downloads/other/music/The Cure - Assemblage - 1991 (12CD FLAC)/The Cure - Assemblage - 1991 (12CD FLAC).zip")
    val content = getBytes(f)
    val test = content.foldLeft(FlacStart())(FlacFileParser.transition)
    content.map(
      b =>
        println(b)
        b
    ).toArray
    println(content)
