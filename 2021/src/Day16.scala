import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day16 {

  case class Packet[A](versionId: Int, typeId: Int, data: A) {
    def sumVersionIds: Int = data match {
      case _: Long => versionId
      case ll: List[Packet[_]] => versionId + ll.map(_.sumVersionIds).sum
    }

    def value: Long = data match {
      case v: Long => v
      case ll: List[Packet[_]] => typeId match {
        case 0 => ll.map(_.value).sum
        case 1 => ll.map(_.value).product
        case 2 => ll.map(_.value).min
        case 3 => ll.map(_.value).max
        case 5 => if (ll.head.value > ll.last.value) 1 else 0
        case 6 => if (ll.head.value < ll.last.value) 1 else 0
        case 7 => if (ll.head.value == ll.last.value) 1 else 0
      }
    }
  }

  class BitIterator(val originalIterator: Source) extends Iterator[Int] {
    private val localBuffer = mutable.Queue[Int]()

    override def hasNext: Boolean = originalIterator.hasNext || localBuffer.nonEmpty

    override def next(): Int = {
      if (localBuffer.isEmpty) localBuffer.enqueueAll(nextList())
      localBuffer.dequeue()
    }

    private def nextList() = ("0000" + nextHexToBinary()).takeRight(4).map(_ - '0')
    private def nextHexToBinary() = Integer.parseInt(originalIterator.next().toString, 16).toBinaryString
  }

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2021/input_day16.txt")) {
      source =>
        val ll = readPackets(new BitIterator(source))
        println(ll.map(_.sumVersionIds).sum)
        println(ll.head.value)
    }
  }

  def readPackets(bb: Iterator[Int]): List[Packet[_]] = {
    def readIntPacket(it: Iterator[Int], version: Int, typeId: Int) = {
      @tailrec
      def consumePacket(acc: List[Int]): Long = {
        val k = it.take(5).toList
        if (k.head == 1) consumePacket(acc ++ k.tail)
        else java.lang.Long.parseLong((acc ++ k.tail).mkString, 2)
      }

      new Packet[Long](version, typeId, consumePacket(Nil))
    }

    def readOpPacket(it: Iterator[Int], version: Int, typeId: Int): Packet[List[Packet[_]]] = it.next() match {
      case 0 =>
        val length = Integer.parseInt(it.take(15).mkString, 2)
        val nextIterator = it.take(length).toList.iterator
        new Packet[List[Packet[_]]](version, typeId, readPackets(nextIterator, Nil))
      case _ =>
        val count = Integer.parseInt(it.take(11).mkString, 2)
        new Packet[List[Packet[_]]](version, typeId, readNextNPackets(it, count))
    }

    def readNextNPackets(it: Iterator[Int], count: Int) = {
      @tailrec
      def readNextNPackets(acc: List[Packet[_]], count: Int): List[Packet[_]] = {
        if (count == 0) acc
        else readNextPacket(it) match {
          case Some(packet) => readNextNPackets(acc :+ packet, count - 1)
        }
      }

      readNextNPackets(Nil, count)
    }

    def readNextPacket(it: Iterator[Int]): Option[Packet[_]] = {
      it.duplicate match {
        case (a, b) =>
          if (a.forall(_ == 0)) Option.empty
          else {
            val (version, typeId) = (
              Integer.parseInt(b.take(3).mkString, 2),
              Integer.parseInt(b.take(3).mkString, 2))

            if (typeId == 4) Option(readIntPacket(b, version, typeId))
            else Option(readOpPacket(b, version, typeId))
          }
      }
    }

    @tailrec
    def readPackets(it: Iterator[Int], acc: List[Packet[_]]): List[Packet[_]] = {
      if (it.isEmpty) acc
      else readNextPacket(it) match {
        case Some(packet) => readPackets(it, acc :+ packet)
        case None => acc
      }
    }

    readPackets(bb, Nil)
  }
}
