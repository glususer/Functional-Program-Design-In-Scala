package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b()*b()-4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    Signal {
      val a1 = (-b() + math.sqrt(delta())) / 2 * a()
      val a2 = (-b() - math.sqrt(delta())) / 2 * a()
      (Set(a1, a2))
    }
  }
}
