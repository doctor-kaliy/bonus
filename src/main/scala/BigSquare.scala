class BigSquare(i: Square, j: Square, k: Square, l: Square) {
  val squares: Vector[Square] = Vector(i, j, k, l)
  def isValid: Boolean = squares.zip(3 to 0 by -1).map(a => a._1.get(a._2)).sum == 10 &&
    Vector((0, 1), (1, 3), (3, 2), (2, 0))
      .map({case(x, y) => squares(x).get(y) + squares(y).get(x) <= 10})
      .reduce(_ && _)

  def get(id: Integer): Square = squares(id)
  def deepGet(i: Integer, j: Integer): Int = get(i).get(j)

  override def equals(obj: Any): Boolean = obj != null && obj.getClass == this.getClass &&
    (0 to 3).map(i => squares(i).equals(obj.asInstanceOf[BigSquare].squares(i))).reduce(_ && _)
  override def hashCode(): Int = squares.hashCode()
  override def toString: String = squares.mkString(", ")
}