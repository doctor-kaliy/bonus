import scala.io.StdIn

object ScalaApp {
  def main(args: Array[String]): Unit = {
    val n = 12
    val squares = (for (id <- 0 until n) yield
      (new Square(StdIn.readLine().split("[ ]+").map(_.toInt).toVector), id)).toSet

    val bigSquares =
      for ((i, i_id) <- squares; (j, j_id) <- squares; (k, k_id) <- squares; (l, l_id) <- squares;
           sq = new BigSquare(i, j, k, l) if Set(i_id, j_id, k_id, l_id).size == 4 && sq.isValid) yield
        (sq, (squares -- Set((i, i_id), (j, j_id), (k, k_id), (l, l_id))).toVector.map(_._1))

    @scala.annotation.tailrec
    def removedPair(vector: Vector[Square], id1: Int, id2: Int): Vector[Square] =
      if (id1 < id2)
        vector.slice(0, id1) ++ vector.slice(id1 + 1, id2) ++ vector.slice(id2 + 1, vector.length)
      else
        removedPair(vector, id2, id1)

    def findOuterPart(bigSquares: Set[((BigSquare, Vector[Square]), Vector[Square])],
                      sideFirst: Int, sideSecond: Int, rotations: Int)
    : Set[((BigSquare, Vector[Square]), Vector[Square])] = {
      for (((center, outer), rest) <- bigSquares; i <- 0 until rest.length;
           j <- 0 until rest.length;
           bigSquare = new BigSquare(rest(i).rotateClockwise(rotations),
             rest(j).rotateClockwise(rotations),
             center.get(sideFirst).rotateClockwise(rotations),
             center.get(sideSecond).rotateClockwise(rotations))
           if i != j && bigSquare.isValid)
        yield ((center, outer.appended(rest(i)).appended(rest(j))), removedPair(rest, i, j))
    }

    val models: Set[(BigSquare, Vector[Square])] =
      findOuterPart(findOuterPart(findOuterPart(findOuterPart(bigSquares.map
        { case (center, rest) => ((center, Vector()), rest) }, 0, 1, 0),
        1, 3, 1), 3, 2, 2),
        2, 0, 3)
        .map{case ((center, outer), _) =>
          (center, Vector(0, 1, 3, 5, 7, 6, 4, 2).zip(outer)
            .sorted((a: (Int, Square), b: (Int, Square)) =>
              if (a._1 < b._1) -1 else if (a._1 == b._1) 0 else 1).map(_._2))}

    print((for ((center, outer) <- models;
                i = new BigSquare(outer(0), outer(1), center.get(0), center.get(1));
                j = new BigSquare(outer(2), center.get(0), outer(4), center.get(2));
                k = new BigSquare(center.get(1), outer(3), center.get(3), outer(5));
                l = new BigSquare(center.get(2), center.get(3), outer(6), outer(7))
                if Array(i, j, k, l).map(_.isValid).reduce(_ && _) &&
                  i.deepGet(0, 2) + center.deepGet(0, 0) + j.deepGet(0, 1) <= 10 &&
                  i.deepGet(1, 3) + center.deepGet(1, 1) + k.deepGet(1, 0) <= 10 &&
                  l.deepGet(2, 0) + center.deepGet(2, 2) + j.deepGet(2, 3) <= 10 &&
                  k.deepGet(3, 2) + center.deepGet(3, 3) + l.deepGet(3, 1) <= 10
                ) yield
      new Model(i, j, k, l)).mkString("\n\n"))
  }
}