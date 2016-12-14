package crypto101

import scala.annotation.tailrec

/**
  * Created by erodriguez on 12/12/16.
  */
trait BlockCypher {
  def encode(plaintext: String)(fn: String => EncodedText): EncodedText
  def decode(ciphertext: String)(fn: EncodedText => String): String
}

class PermutationBlockCypher(key: String) extends BlockCypher {

  val blockSize = key.length
  val rang: Range = 0 until blockSize

  @tailrec
  private def fact(n: Int, acc: Int): Int = {
    if (n == 0 || n == 1) acc
    else fact(n - 1, acc * n)
  }

  val permutation: IndexedSeq[Int] = {
    val perms: Iterator[IndexedSeq[Int]] = rang.permutations
    val n = blockSize - 1
    val l = fact(n, 1)
    var i = 0;
    while (i <= l) {
      perms.next()
      i += 1
    }
    perms.next()
  }

  override def encode(plaintext: String)(fn: String => EncodedText): EncodedText =
    fn(divideIntoBlocks(plaintext).map(encodeBlock).mkString)

  override def decode(ciphertext: EncodedText)(fn: EncodedText => String): String =
    divideIntoBlocks(fn(ciphertext)).map(decodeBlock).mkString.trim

  private def divideIntoBlocks(text: String): List[String] = {

    def fillBlanks(orig: String): String = {
      var i = 0
      var s = orig
      for (i <- s.length until blockSize) s += " "
      s
    }

    // blocksize will be same as key.length
    var list: List[String] = text.grouped(blockSize).toList
    if (list.last.length < blockSize) {
      val ul = list.take(list.length - 1)
      ul :+ fillBlanks(list.last)
    } else list
  }

  private def encodeBlock(plainBlock: String): String = {
    // the block has the same length as the key
    val chrs: List[Char] = plainBlock.toList
    permutation.foldLeft("")((acc, i) => chrs(i) +: acc)
  }

  private def decodeBlock(cipherBlock: EncodedText): String = {
    val chrs: List[Char] = cipherBlock.toList
    rang.foldLeft("")((acc:String, i:Int) => chrs(permutation.indexOf(i)) +: acc)
  }
}

object PermutationBlockCypher {
  def apply(key: String): PermutationBlockCypher = new PermutationBlockCypher(key)
}

object Runner {
  def main(args: Array[String]): Unit = {
    val cipher = PermutationBlockCypher("mysecret")

    val p1 = "Miners process transactions on the blockchain. What this really means is that miners collect all the transactions into a block and then validate and verify the information based on preset rules."
    val c1: EncodedText = cipher.encode(p1)(identity)
    println(s"PlainTX: $p1\nEncoded: $c1")
    val d1 = cipher.decode(c1)(identity)
    println(s"Decoded: $d1")

    val oneTime = OneTimeCypher(267)
    val c2 = cipher.encode(p1)(oneTime.encode)
    println(s"Encoded: $c2")

    val p2 = "The target is self adjusting. That is, it has a bit of intelligence. In the bitcoin blockchain, the target is re-adjusted every 2 weeks so that on average, the target is achieved every 10 minutes."
    val c3 = cipher.encode(p2)(oneTime.encode)
    println(s"Encoded: $c3")

    val diff = oneTime.reusedKey(false)(c2, c3)
    println(s"$diff")
    val i0 = diff.indexOf(0)
    println(s"Position $i0 for first text is ${oneTime.helper(c2)(i0)} (${p1(i0)})")
    println(s"Position $i0 for second text is ${oneTime.helper(c3)(i0)} (${p2(i0)})")

    val x1 = oneTime.helper(c2)(i0).toInt ^ p1(i0)
    println(s"XOR of ${oneTime.helper(c2)(i0)} and ${p1(i0)} = ${x1} (key)")

    val x2 = oneTime.helper(c3)(i0).toInt ^ p2(i0)
    println(s"XOR of ${oneTime.helper(c3)(i0)} and ${p2(i0)} = ${x2} (key)")

    val wrongOneTime285 = OneTimeCypher(285)
    val wrongOneTime273 = OneTimeCypher(273)

    val d285 = cipher.decode(c2)(wrongOneTime285.decode)
    println(s"Decoded: $d285")

    val d273 = cipher.decode(c2)(wrongOneTime273.decode)
    println(s"Decoded: $d273")

    val d2 = cipher.decode(c2)(oneTime.decode)
    println(s"Decoded: $d2")

    import OneTimeCypher._
    println(s"Most frequent byte c2: ${oneTime.helper(c2).mostFrequentByte}")
    println(s"Most frequent byte c3: ${oneTime.helper(c3).mostFrequentByte}")
    println(s"${Char.char2int('e') ^ 299}")
    println(s"${Char.char2int(' ') ^ 299}")
  }
}