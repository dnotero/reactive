package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Signal {
    	val _a = a()
    	val _b = b()
    	val _c = c()

    	_b * _b - 4 * _a * _c
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
    	val delta = computeDelta(a, b, c)
    	val firstRoot = (-b() + math.sqrt(delta())) / (2 * a())
    	val secondRoot = (-b() - math.sqrt(delta())) / (2 * a())

    	if(delta() < 0) Set()
    	else Set(firstRoot, secondRoot)
    }
  }
}
