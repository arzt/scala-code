package com.github.arzt.flac
import scala.collection.immutable.ArraySeq 

trait FlacElement

case class FlacHeader(header: String = "") extends FlacElement

case class FlacFile(metadata: Seq[MetaData])

case class FlacFileParser(element: FlacElement, file: FlacFile = FlacFile.apply(List[MetaData]()))

case class MetaDataHeader(isLast: Boolean, blockType: Int, length: Seq[Byte]) extends FlacElement

case class MetaData(isLast: Boolean, blockType: Int, length: Int, bytes: Seq[Byte]) extends FlacElement


object FlacFileParser:
  def transition(state: FlacFileParser, input: Byte): FlacFileParser =
    (state, input) match
      case (FlacFileParser(FlacHeader(""), f), 'f') =>
        FlacFileParser(FlacHeader("f"), f)
      case (FlacFileParser(FlacHeader("f"), f), 'L') =>
        FlacFileParser(FlacHeader("fL"), f)
      case (FlacFileParser(FlacHeader("fL"), f), 'a') =>
        FlacFileParser(FlacHeader("fLa"), f)
      case (FlacFileParser(FlacHeader("fLa"), f), 'C') =>
        FlacFileParser(FlacHeader("fLaC"), f)
      case (FlacFileParser(FlacHeader("fLaC"), f), byte) =>
        val metadata = MetaDataHeader(byte < 0, byte, Vector())
        FlacFileParser(metadata, f)
      case (FlacFileParser(MetaDataHeader(isLast, blockType, Seq(a)), f), byte) =>
        FlacFileParser(MetaDataHeader(isLast, blockType, Vector(a, byte)), f)
      case (FlacFileParser(MetaDataHeader(isLast, blockType, Seq(a, b)), f), byte) =>
        FlacFileParser(MetaDataHeader(isLast, blockType, Vector(a, b, byte)), f)
      case (FlacFileParser(MetaDataHeader(isLast, blockType, Seq(a, b, c)), f), byte) =>
        val length = 5
        FlacFileParser(MetaData(isLast, blockType, lenght = 5, Vector(), f)
      case _ => throw RuntimeException("Unsupported byte sequence. Stream might me corrupted.")
