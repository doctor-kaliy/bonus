class Square(corners: Vector[Int]) {
  val getCorners: Vector[Int] = corners
  def get(id: Int): Int = corners(id)
  def rotateClockwise(n: Int) = new Square((1 to n).foldLeft(corners)((acc, _) =>
    Vector(acc(1),
      acc(3),
      acc(0),
      acc(2))))
  override def toString: String = corners.mkString(" ")
  override def hashCode(): Int = corners.hashCode()
  override def equals(obj: Any): Boolean = obj != null && obj.getClass == this.getClass &&
    (0 to 3).map(i => getCorners(i).equals(obj.asInstanceOf[Square].getCorners(i))).reduce(_ && _)
}