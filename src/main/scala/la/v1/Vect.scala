package la.v1

case class Vect(seq: Seq[Double]) {

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

  override def toString: String = seq.mkString("<[", ", ", "]>")
}

object Vect {

  def of(seq: Double*): Vect = Vect(seq)

  def fill(len: Int)(elem: Double): Vect = Vect(Iterator.fill(len)(elem).toSeq)
}
