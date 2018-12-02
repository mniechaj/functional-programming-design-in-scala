package calculator

object Polynomial {
  import scala.math._
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal {
    pow(b(), 2) - (4 * (a() * c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    if (delta() < 0) Set.empty
    else {
      val x1 = (-b() - sqrt(delta())) / (2 * a())
      val x2 = (-b() + sqrt(delta())) / (2 * a())
      Set(x1, x2)
    }
  }
}
