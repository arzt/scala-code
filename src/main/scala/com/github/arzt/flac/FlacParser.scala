package com.github.arzt.flac

import java.io.RandomAccessFile
import scala.collection.immutable.ArraySeq
import MetadataBlockType.*

import scala.annotation.tailrec

enum MetadataBlockType(code: Int):
  case StreamInfoType extends MetadataBlockType(0)
  case PaddingType extends MetadataBlockType(1)
  case ApplicationType extends MetadataBlockType(2)
  case SeekTableType extends MetadataBlockType(3)
  case VorbisCommentType extends MetadataBlockType(4)
  case CueSheetType extends MetadataBlockType(5)
  case PictureType extends MetadataBlockType(6)
  case InvalidType extends MetadataBlockType(127)

case class MetadataBlockHeader(isLast: Boolean, blockType: MetadataBlockType, blockLength: Int)

trait MetadataBlockData

case class StreamInfo(minBlocksizeSamples: Int,
                      maxBlocksizeSampls: Int,
                      minFrameSizeBytes: Int,
                      maxFrameSizeBytes: Int,
                      sampleRate: Int,
                      numChannels: Int,
                      bitsPerSample: Int,
                      totalSamples: Long,
                      md5: BigInt) extends MetadataBlockData

case class PaddingBlockData(bytes: IndexedSeq[Byte]) extends MetadataBlockData

case class VorbisCommentBlockData(comments: List[String]) extends MetadataBlockData

case class MetadataBlock(header: MetadataBlockHeader, data: MetadataBlockData)

case class RawFlac(metadataBlocks: LazyList[MetadataBlock], error: Option[String])

object FlacParser:

  private
  def fileToBytes(input: RandomAccessFile): LazyList[Byte] =
    val buf = new Array[Byte](10240)

    val buf2 = IArray(buf)
    LazyList
      .continually(
        {
          println("evaluated!")
          input.read(buf)
        }
      )
      .takeWhile(_ >= 0)
      .flatMap(buf.view.slice(0, _))

  def parseMetadataBlockHeader(bytes: LazyList[Byte]): (MetadataBlockHeader, LazyList[Byte]) =
      val a #:: b #:: c #:: d #:: tail = bytes : @unchecked
      val isLast = a < 0
      val blockTypeOrdinal = a & 0x7F
      val blockType = MetadataBlockType.fromOrdinal(blockTypeOrdinal)
      val blockLength = BigInt(Array(b, c, d)).toInt
      (MetadataBlockHeader(isLast, blockType, blockLength), tail)

  def parseMetadataBlockData(bytes: LazyList[Byte]): (MetadataBlockData, LazyList[Byte]) = ???

  def parsePaddingBlock(bytes: LazyList[Byte], length: Int): (PaddingBlockData, LazyList[Byte]) =
    val (a, b) = bytes.splitAt(length)
    val seq = ArraySeq(a: _*)
    (PaddingBlockData(seq), b)

  def parseVorbisCommentBlock(bytes: LazyList[Byte], length: Int): (VorbisCommentBlockData, LazyList[Byte]) =
    val a #:: b #:: c #:: d #:: bytes1 = bytes : @unchecked
    val vendorLength = BigInt(Array(d, c, b, a)).toInt
    val (dat, bytes2) = bytes1.splitAt(vendorLength)
    val vendorString = new String(Array(dat: _*))

    val a1 #:: b1 #:: c1 #:: d1 #:: bytes3 = bytes2 : @unchecked
    val listLength = BigInt(Array(d1, c1, b1, a1)).intValue

    @tailrec
    def parseComment(i: Int, acc: List[String], tail: LazyList[Byte]): (List[String], LazyList[Byte]) =
      if (i == 0)
        (acc, tail)
      else
        val a #:: b #:: c #:: d #:: nextTail = tail : @unchecked
        val commentLength = BigInt(Array(d, c, b, a)).intValue
        val (commentData, outputTail) = nextTail.splitAt(commentLength)
        val comment = new String(Array(commentData: _*))
        parseComment(i - 1, comment :: acc, outputTail)

    val (comments, bytes4) = parseComment(listLength, Nil, bytes3)
    (VorbisCommentBlockData(comments), bytes4)

  def parseStreamInfoBlock(bytes: LazyList[Byte]): (StreamInfo, LazyList[Byte]) =
    val (data, tail) = bytes.splitAt(34)
    val d = data.toIndexedSeq.map(_ & 0xFF)
    val minBlockSizeSamples = (d(0) << 8) + d(1)
    val maxBlockSizeSamples = (d(2) << 8) + d(3)
    val minFrameSizeBytes = (d(4) << 16) + (d(5) << 8) + d(6)
    val maxFrameSizeBytes = (d(7) << 16) + (d(8) << 8) + d(9)
    val sampleRate = ((d(10) << 16) + (d(11) << 8) + d(12)) >> 4
    """
    ________ ________ ____xxx_
    """
    val numChannels = ((d(12) >> 1) & 0x7) + 1
    """
    _______X XXXX____
    """
    val bitsPerSample = ((d(12) & 1) << 4) + ((d(13) >> 4) & 15) + 1
    val totalSamples = ((d(13).toLong & 1) << 32) + (d(14) << 24) + (d(15) << 16) + (d(16) << 8) + d(17)
    val array = data.splitAt(18)._2.toArray
    val md5 = BigInt(array)
    val test = md5 << 5
    val info = StreamInfo(minBlockSizeSamples, maxBlockSizeSamples, minFrameSizeBytes, maxFrameSizeBytes, sampleRate,
      numChannels, bitsPerSample, totalSamples, md5)
    (info, tail)

  def parseMetadataBlock(bytes: LazyList[Byte]): (MetadataBlock, LazyList[Byte]) =
    val (header, tail) = parseMetadataBlockHeader(bytes)
    val (data, tail2) = header match
      case MetadataBlockHeader(isLast, StreamInfoType, length) =>
        parseStreamInfoBlock(tail)
      case MetadataBlockHeader(isLast, PaddingType, length) =>
        parsePaddingBlock(tail, length)
      case MetadataBlockHeader(isLast, VorbisCommentType, length) =>
        parseVorbisCommentBlock(tail, length)
      case MetadataBlockHeader(isLast, _, length) =>
        parsePaddingBlock(tail, length)
    (MetadataBlock(header, data), tail2)

  def parseMetadataBlocks(bytes: LazyList[Byte]): (LazyList[MetadataBlock], LazyList[Byte]) =
    val (block, tail) = parseMetadataBlock(bytes)
    if (block.header.isLast)
      (LazyList(block), tail)
    else
      val (blocks, tail2) = parseMetadataBlocks(tail)
      (block #:: blocks, tail2)

  def parseFlac(bytes: LazyList[Byte]): RawFlac =
    bytes match
      case 'f' #:: 'L' #:: 'a' #:: 'C' #:: tail =>
        val (blocks, tail3) = parseMetadataBlocks(tail)
        RawFlac(blocks, None)
      case _ =>
        RawFlac(LazyList.empty, error = Some("you did all wrong!"))

  def main(args: Array[String]): Unit =
    val testFiles = List(
      "/media/frog/musik/Adam Green - 2003 - Friends of Mine/Adam Green - 01 - Bluebirds.flac",
      "/media/frog/musik/Actress - 2017 - AZD/Actress - 05 - Cyn.flac",
      "/media/frog/musik/Sofia Kourtesis - 2023 - Madres/Sofia Kourtesis - 05 - Habla con ella.flac",
      "/media/frog/musik/Franz Ferdinand - 2004 - Franz Ferdinand/Franz Ferdinand - 10 - Come On Home.flac"
    )
    val result = testFiles.take(1)
      .map(
        testFile =>
          val mode = "r"
          val raFile = new RandomAccessFile(testFile, mode)
          val bytes = fileToBytes(raFile)
          val flac = parseFlac(bytes)
          flac
      )
    println(result)

