package week2.frp

object Var {
  def apply[T](expr: => T) = new Var(expr)
}
class Var[T](expr: => T) extends Signal[T](expr) {
  override def update(expr: => T): Unit = super.update(expr)
}
