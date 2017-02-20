package v1

import v1.la.Vect

object Statistics {

  def correlation(xs: Seq[Double], ys: Seq[Double]): Double = {
    cov(xs, ys) / (sd(xs) * sd(ys))
  }

  def correlation2(xs: Seq[Double], ys: Seq[Double]): Double = avg {
    zScore(Vect(xs)) * zScore(Vect(ys))
  }

  def cov(xs: Seq[Double], ys: Seq[Double]): Double = expectation {
    (Vect(xs) - avg(xs)) * (Vect(ys) - avg(ys))
  }

  def sd(xs: Seq[Double]): Double = {
    val mean = avg(xs)

    math.sqrt {
      xs.map(x => math.pow(x - mean, 2)).sum / xs.length
    }
  }

  def expectation(xs: Seq[Double]): Double = (Vect(xs) / xs.length).sum

  def avg(xs: Seq[Double]): Double = xs.sum / xs.length

  def zScore(xs: Vect): Vect = (xs - avg(xs)) / sd(xs)
}
