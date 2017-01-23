object Correlation {

  def correlation(xs: Seq[Double], ys: Seq[Double]): Double = {
    cov(xs, ys) / (sd(xs) * sd(ys))
  }

  def cov(xs: Seq[Double], ys: Seq[Double]): Double = {
    val xsMean = avg(xs)
    val ysMean = avg(ys)

    expectation {
      (xs map (_ - xsMean)) zip (ys map (_ - ysMean)) map {
        case (x, y) => x * y
      }
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

  def avg(xs: Seq[Double]) = {
    xs.sum / xs.length
  }
}
