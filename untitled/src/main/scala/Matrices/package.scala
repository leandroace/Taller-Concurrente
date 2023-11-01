import scala.util.Random
package object Matrices {
  val random = new Random()
  type Matrix = Vector[Vector[Int]]

  def vectorAlAzar(long: Int, vals: Int): Vector[Int] = {
    val v = Vector.fill(long) {
      random.nextInt(vals)
    }
    v
  }

  def matrixAlAzar(long: Int, vals: Int): Matrix = {
    val v = Vector.fill(long, long) {
      random.nextInt(vals)
    }
    v
  }

  def transpuesta(m: Matrix): Matrix = {
    val l = m.length
    Vector.tabulate(l, l)((i, j) => m(j)(i))
  }

  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map { case (i, j) => i * j }.sum
  }

  def  multMatriz(m1: Matrix, m2: Matrix): Matrix = {
    val result = Vector.tabulate(m1.length) {
      i => Vector.tabulate(transpuesta(m2).length) {
        j => prodPunto(m1(i), transpuesta(m2)(j)) }
    }
    result
  }
}
