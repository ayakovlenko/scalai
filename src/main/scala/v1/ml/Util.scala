package v1.ml

object Util {

  def softmax(scores: Seq[Double]): Seq[Double] = {
    val sum = (scores map math.exp).sum

    scores map (math.exp(_) / sum)
  }
}
