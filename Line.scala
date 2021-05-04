class Line(ps:Array[Point]) {
  // Create variables to be read only
  private val _ma : Double = Util.cov(ps.map(_.x), ps.map(_.y)) / Util.variance(ps.map(_.x))
  private val _mb : Double = (ps.map(_.y).sum / ps.length) - (a * (ps.map(_.x).sum / ps.length))

  // Create getters (no setters!)
  def a : Double = _ma
  def b : Double = _mb

  // Class functions
  def f(x : Double) : Double = {a * x + b}
  def dist(x : Point) : Double = {math.abs(f(x.x) - x.y)}
}
