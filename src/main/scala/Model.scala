class Model(i: BigSquare, j: BigSquare, k: BigSquare, l: BigSquare) {
  val data: Vector[Square] = Vector(i.get(0), i.get(1), j.get(0), j.get(1), k.get(0), k.get(1),
    j.get(2), j.get(3), k.get(2), k.get(3), l.get(2), l.get(3))
  override def toString: String = data.mkString("\n")
  override def equals(obj: Any): Boolean = obj != null && obj.getClass == this.getClass &&
    (0 to 11).map(i => data(i).equals(obj.asInstanceOf[Model].data(i))).reduce(_ && _)
  override def hashCode(): Int = data.toSeq.hashCode()
}
