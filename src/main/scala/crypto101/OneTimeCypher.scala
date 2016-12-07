package crypto101

class OneTimeCypher(key: Int) {
  def code(plaintext: String): String =
    plaintext.getBytes.map(_ ^ key).foldLeft("")((acc, cur) => acc + cur.toString)

  val pat = s".{${key.toString.length}}".r

  def decode(ciphertext: String): String = {
    helper(ciphertext).map(_.toInt ^ key).map(_.toChar).mkString
  }

  def helper(s: String): List[String] = {
    pat.findAllIn(s).toList
  }

  def reusedKey(encode: Boolean)(p1: String, p2: String): List[Int] = {
    val b1 = if (encode) helper(code(p1)) else helper(p1)
    val b2 = if (encode) helper(code(p2)) else helper(p2)
    (b1 zip b2).map(elem => elem._1.toInt ^ elem._2.toInt)
  }
}

object OneTimeCypher {
  def apply(key: Int) = new OneTimeCypher(key)

  def main (args: Array[String] ): Unit = {
    val s1 = "This is a test"
    val s2 = "Another 1 test"
    val cipher = OneTimeCypher(267)

    val b1 = cipher.code(s1)
    val b2 = cipher.code(s2)
    println(s"$b1\n$b2")

    val l = cipher.reusedKey(true)(s1, s2)
    println(l)

    val invPlain = cipher.reusedKey(false)(b1, b2)
    println(invPlain)

    println(invPlain.sum)

    val d = cipher.decode(invPlain.mkString)
    println(d )
  }
}
