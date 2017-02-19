package v1.la

case class Vect(override val seq: Seq[Double]) extends Iterable[Double] {

  def +(that: Vect): Vect = {
    require(this.seq.length == that.seq.length, "addition of vectors of different length")

    Vect {
      this.seq zip that.seq map {
        case (a, b) => a + b
      }
    }
  }

  def +(x: Double): Vect = Vect(seq map (_ + x))

  def -(that: Vect): Vect = {
    require(this.seq.length == that.seq.length, "subtraction of vectors of different length")

    this + (that * -1)
  }

  def -(x: Double): Vect = this + -x

  def *(scalar: Double): Vect = Vect(seq map (_ * scalar))

  def *(that: Vect): Vect = {
    require(this.seq.length == that.seq.length, "multiplication of vectors of different length")

    Vect {
      this.seq zip that.seq map {
        case (a, b) => a * b
      }
    }
  }

  def magnitude: Double = math.sqrt((seq map (x => math.pow(x, 2))).sum)

  def unit: Vect = this * (1 / magnitude)

  def dot(that: Vect): Double = (this * that).seq.sum

  def angle(that: Vect): Double = math.acos {
    (this dot that) / (this.magnitude * that.magnitude)
  }

  def isZero: Boolean = this.seq forall (_ == 0)

  def isParallelTo(that: Vect): Boolean = {
    if (this.isZero || that.isZero) true else (this angle that) ~= (math.Pi, precision)
  }

  def isOrthogonalTo(that: Vect): Boolean = {
    if (this.isZero || that.isZero) true else (this dot that) ~= (0, precision)
  }

  override def toString: String = seq.mkString("<[", ", ", "]>")

  private val precision = 1e-14

  private implicit class DoubleLike(x: Double) {

    def ~=(y: Double, precision: Double): Boolean = {
      (x - y).abs < precision
    }
  }

  override def iterator: Iterator[Double] = seq.iterator
}

object Vect {

  def of(seq: Double*): Vect = Vect(seq)

  def fill(len: Int)(elem: Double): Vect = Vect(1 to len map (_ => elem))
}
