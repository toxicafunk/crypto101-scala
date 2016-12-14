package crypto101

class OneTimeCypher(key: Int) { // 'key' gets a setter, 'val key' creates a getter and a setter,
  def encode(plaintext: String): String =
    plaintext.getBytes.map(_ ^ key).foldLeft("")((acc, cur) => acc + cur.toString)

  val pat = s".{${key.toString.length}}".r

  def decode(ciphertext: String): String = {
    helper(ciphertext).map(_.toInt ^ key).map(_.toChar).mkString
  }

  def helper(s: String): List[String] = {
    pat.findAllIn(s).toList
  }

  def reusedKey(applyEncode: Boolean)(p1: String, p2: String): Seq[Int] = {
    val b1 = if (applyEncode) helper(encode(p1)) else helper(p1)
    val b2 = if (applyEncode) helper(encode(p2)) else helper(p2)
    xor(b1.map(_.toInt), b2.map(_.toInt))
  }

  def xor(s1: Seq[Int], s2: Seq[Int]): Seq[Int] = (s1 zip s2).map(elem => elem._1 ^ elem._2)

}

object OneTimeCypher {
  def apply(key: Int) = new OneTimeCypher(key)

  implicit class BinaryUtils(val bs1: Seq[Int]) {
    def xor(bs2: Seq[Int]): Seq[Int] = (bs1 zip bs2).map(elem => elem._1 ^ elem._2)
  }

  implicit class StringUtils(val xs: List[String]) {
    def mostFrequentByte: (String, Int) = xs.groupBy(s => s).map( (elem) => elem._1 -> elem._2.length ).maxBy(_._2)
  }

  def main (args: Array[String] ): Unit = {
    val cipher = OneTimeCypher(267)

    val p1 = "This is a test"
    val p2 = "Another 1 test"

    val diff1 = cipher.reusedKey(true)(p1, p2)
    println(s"From 2 plaintexts: $diff1")

    val c1 = cipher.encode(p1)
    val c2 = cipher.encode(p2)

    val diff2 = cipher.reusedKey(false)(c1, c2)
    println(s"From 2 ciphertexts: $diff2")

    println(s"c1: $c1\nc2: $c2")

    println(s"Most frequent byte c1: ${cipher.helper(c1).mostFrequentByte}")
    println(s"Most frequent byte c2: ${cipher.helper(c2).mostFrequentByte}")

    println(Seq(299) xor Seq(Char.char2int(' ')))

    val c = "383366376383"
    val p = "test"

    println((cipher.helper(c).map(_.toInt) xor p.getBytes.map(Byte.byte2int(_))))

    val d = cipher.decode(c1.mkString)
    println(d )

    println(('a' to 'z').map(Char.char2int(_)))

    val p3 = "Miners process transactions on the blockchain. What this really means is that miners collect all the transactions into a block and then validate and verify the information based on preset rules."
    val p4 = "The target is self adjusting. That is, it has a bit of intelligence. In the bitcoin blockchain, the target is re-adjusted every 2 weeks so that on average, the target is achieved every 10 minutes."

    val c3 = cipher.encode(p3)
    val c4 = cipher.encode(p4)
    val reuse = cipher.reusedKey(false)(c3, c4)
    println(s"$reuse")
    val i0 = reuse.indexOf(0)
    println(s"Position $i0 for first text is ${cipher.helper(c3)(i0)} (${p3(i0)})")
    println(s"Position $i0 for second text is ${cipher.helper(c4)(i0)} (${p4(i0)})")

    val x = cipher.helper(c3)(i0).toInt ^ p3(i0)
    println(s"XOR of ${cipher.helper(c3)(i0)} and ${p3(i0)} = ${x} (key)")
  }
}
