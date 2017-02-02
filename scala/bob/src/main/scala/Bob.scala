class Bob {
  def isQuestion(msg: String): Boolean =
    msg.endsWith("?")

  def isNothing(msg: String): Boolean =
    msg.trim.isEmpty

  def isYelling(msg: String): Boolean = {
    val letters = msg.filter(_.isLetter)
    letters.forall(_.isUpper) && !letters.isEmpty
  }

  def hey(msg: String): String =
    if (isNothing(msg))
      "Fine. Be that way!"
    else if (isYelling(msg))
      "Whoa, chill out!"
    else if (isQuestion(msg))
      "Sure."
    else
      "Whatever."
}
