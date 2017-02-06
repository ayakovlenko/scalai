import la.v1.Vect

object Statistics {

  def correlation(xs: Seq[Double], ys: Seq[Double]): Double = {
    cov(xs, ys) / (sd(xs) * sd(ys))
  }

  def cov(xs: Seq[Double], ys: Seq[Double]): Double = {
    val xsMean = avg(xs)
    val ysMean = avg(ys)

    expectation {
      ((Vect(xs) - xsMean) * (Vect(ys) - ysMean)).seq
    }
  }

  def sd(xs: Seq[Double]): Double = {
    val mean = avg(xs)

    math.sqrt {
      xs.map(x => math.pow(x - mean, 2)).sum / xs.length
    }
  }

  def expectation(xs: Seq[Double]): Double = {
    val n: Double = xs.length
    val probability = xs groupBy identity mapValues (_.length / n)

    (xs map (x => x * probability(x))).sum
  }

  def avg(xs: Seq[Double]): Double = {
    xs.sum / xs.length
  }
}
