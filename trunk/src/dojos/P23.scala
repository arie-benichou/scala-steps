package dojos

object P23 {

  /**
   * injection implicite de l'objet Random :
   * - permet de fournir un seed
   * - évite d'avoir à créer un nouvel objet Random à chaque appel de randomSelect
   */
  implicit val Random = new util.Random

  def randomSelect[T](n: Int, ls: List[T])(implicit random: util.Random): List[T] = {
    @inline
    @annotation.tailrec
    def randomSelect(n: Int, ls: List[T], length: Int, selection: List[T]): List[T] = {
      if (n * length == 0) selection
      else {
        val (others, selected) =
          ls.splitAt(random.nextInt(length)) match {
            case (left, selected :: right) => (left ::: right, selected)
            case _ => throw new NoSuchElementException
          }
        randomSelect(n - 1, others, length - 1, selected :: selection)
      }
    }
    if (n > 0) randomSelect(n, ls, ls.length, Nil) else Nil
  }

  def main(args: Array[String]) = {

    import org.scalacheck.Gen
    import org.scalacheck.Gen.choose
    import org.scalacheck.Prop._

    val p1 = forAll((k: Int) => randomSelect(k, Nil) == Nil).check
    val p2 = forAll((ls: List[Int]) => (randomSelect(0, ls) == Nil)).check
    val p3 = forAll((k: Int) => (k < 0) ==> (randomSelect(k, List(Some)) == randomSelect(0, List(Some)))).check
    val p4 = forAll((k: Int) => (k > 0) ==> (randomSelect(k, List(Some)) == List(Some))).check

    val lowerBound = 2
    val upperBound = 100

    val p5 = forAll(Gen.choose(lowerBound, upperBound))(
      n => {
        val ls = (1 to n).toList
        val k = Random.nextInt(n) + 1
        val output = randomSelect(k, ls)
        if (output.length == k && output.removeDuplicates == output && output.forall(e => (e >= 1 && e <= n))) {
          var i = 0; while (randomSelect(k, ls) == output && i < 10) i += 1;
          if (i == 10) false else true
        } else false
      }).check

    //all(p1, p2, p3, p4, p5).check

  }

}