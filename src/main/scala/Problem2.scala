import cats.implicits._

object Problem2 extends App{
  case class Promotion(code: String, notCombinableWith: Seq[String])
  case class PromotionCombo(promotionCodes: Seq[String])

  //  Change it up and do it in a locally mutable way
  //  I did basic result alphabetical sorting, assuming Promo code can be an arbitrary String (that means sorting breaks for P's with several digits)

  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = if (allPromotions.isEmpty) Seq() else {

    import scala.collection.{ mutable => mut }

    def addIfNoSuperset[A](to: mut.Set[Set[A]], what: Set[A]): Unit =
      if (to.forall(what.diff(_).nonEmpty))
        to.add(what)

    val codes = allPromotions.map(_.code)
    val combos = mut.Set(codes.toSet)
    val exclusivityPairsSet = mut.Set.empty[(String, String)]

    allPromotions.foreach{
      case Promotion(code, notCombinableWith) => notCombinableWith.foreach(x =>
        if (!exclusivityPairsSet.contains(code -> x) && !exclusivityPairsSet.contains(x -> code))
          exclusivityPairsSet.add(code -> x)
      )
    }

    val exclusivityPairs = mut.Queue.from(exclusivityPairsSet)

    while (exclusivityPairs.nonEmpty){
      val (one, other) = exclusivityPairs.dequeue()
      combos.foreach { st =>
        if (st.contains(one) && st.contains(other)){
          combos.remove(st)
          addIfNoSuperset(combos, st - one)
          addIfNoSuperset(combos, st - other)
        }
      }
    }

    combos.map(_.toSeq.sorted).toSeq.sorted.map(PromotionCombo)
  }

  //in production env I'd probably change the signatures to create lookup maps like HashMap[code, [setID]] and HashMap[setID, Set]
  //this way this function would run in O(1), not O(n)
  def combinablePromotions(promotionCode: String,
                           allPromotions: Seq[Promotion]): Seq[PromotionCombo] =
    allCombinablePromotions(allPromotions).filter(_.promotionCodes.contains(promotionCode))

  val testInput = Seq(
    Promotion("P1", Seq("P3")),
    Promotion("P2", Seq("P4", "P5")),
    Promotion("P3", Seq("P1")),
    Promotion("P4", Seq("P2")) ,
    Promotion("P5", Seq("P2"))
  )

  println(allCombinablePromotions(testInput))
  assert(allCombinablePromotions(testInput) == Seq(PromotionCombo(List("P1", "P2")), PromotionCombo(List("P1", "P4", "P5")), PromotionCombo(List("P2", "P3")), PromotionCombo(List("P3", "P4", "P5"))))

  val testInput2 = Seq()
  assert(allCombinablePromotions(testInput2) == Seq())

  val testInput3 = Seq(Promotion("P1", Seq("P3")), Promotion("P3", Seq("P1")))
  assert(allCombinablePromotions(testInput3) == Seq(PromotionCombo(Seq("P1")), PromotionCombo(Seq("P3"))))

  val testInput4 = Seq(Promotion("P1", Seq()), Promotion("P3", Seq()))
  assert(allCombinablePromotions(testInput4) == Seq(PromotionCombo(Seq("P1", "P3"))))

  val testInput5 = Seq(Promotion("P1", Seq()), Promotion("P3", Seq()), Promotion("P4", Seq()))
  assert(allCombinablePromotions(testInput5) == Seq(PromotionCombo(Seq("P1", "P3", "P4"))))

  val testInput6 = Seq(Promotion("P1", Seq()), Promotion("P3", Seq("P4")), Promotion("P4", Seq("P3")))
  assert(allCombinablePromotions(testInput6) == Seq(PromotionCombo(Seq("P1", "P3")),PromotionCombo(Seq("P1", "P4"))))

  val testInput7 = Seq(Promotion("P1", Seq()), Promotion("P3", Seq("P4")), Promotion("P4", Seq("P3")))
  assert(allCombinablePromotions(testInput7) == Seq(PromotionCombo(Seq("P1", "P3")),PromotionCombo(Seq("P1", "P4"))))

  val testInput8 = Seq(Promotion("P1", Seq()), Promotion("P3", Seq()), Promotion("P4", Seq("P3")))
  assert(allCombinablePromotions(testInput8) == Seq(PromotionCombo(Seq("P1", "P3")),PromotionCombo(Seq("P1", "P4"))))

  val testInput9 = Seq(Promotion("P1", Seq("P2", "P3")), Promotion("P2", Seq("P1", "P3")), Promotion("P3", Seq("P1", "P2")))
  assert(allCombinablePromotions(testInput9) == Seq(PromotionCombo(Seq("P1")),PromotionCombo(Seq("P2")),PromotionCombo(Seq("P3"))))

  val testInput10 = Seq(Promotion("P1", Seq()), Promotion("P2", Seq("P1", "P3")), Promotion("P3", Seq("P1", "P2")))
  assert(allCombinablePromotions(testInput10) == Seq(PromotionCombo(Seq("P1")),PromotionCombo(Seq("P2")),PromotionCombo(Seq("P3"))))

  val testInput11 = Seq(Promotion("P1", Seq("P2")), Promotion("P2", Seq("P3")), Promotion("P3", Seq("P1")))
  assert(allCombinablePromotions(testInput11) == Seq(PromotionCombo(Seq("P1")),PromotionCombo(Seq("P2")),PromotionCombo(Seq("P3"))))

  val testInput12 = Seq(Promotion("P1", Seq("P2")), Promotion("P2", Seq("P3")), Promotion("P3", Seq("P1")))
  assert(allCombinablePromotions(testInput12) == Seq(PromotionCombo(Seq("P1")),PromotionCombo(Seq("P2")),PromotionCombo(Seq("P3"))))

  val testInput13 = Seq(Promotion("P1", Seq("P2")), Promotion("P2", Seq("P3")), Promotion("P3", Seq("P1")))
  assert(allCombinablePromotions(testInput13) == Seq(PromotionCombo(Seq("P1")),PromotionCombo(Seq("P2")),PromotionCombo(Seq("P3"))))


  val test2 = Seq(
    Promotion("P1", Seq("P3")),
    Promotion("P2", Seq("P4", "P5", "P6", "P7")),
    Promotion("P3", Seq("P1")),
    Promotion("P4", Seq("P2")) ,
    Promotion("P5", Seq("P2")),
    Promotion("P6", Seq("P2", "P7")),
    Promotion("P7", Seq("P2", "P6")),
    Promotion("P8", Seq("P9")),
    Promotion("P9", Seq("P8")),
    Promotion("P10", Seq()),
    Promotion("P11", Seq()),
    Promotion("P12", Seq()),
    Promotion("P13", Seq("P14")),
    Promotion("P14", Seq("P13")),
    Promotion("P15", Seq("P16")),
    Promotion("P16", Seq("P15")),
    Promotion("P17", Seq("P18")),
    Promotion("P18", Seq("P17")),
  )
  allCombinablePromotions(test2)

  println(combinablePromotions("P1", testInput))
  assert(combinablePromotions("P1", testInput) == Seq(PromotionCombo(List("P1", "P2")), PromotionCombo(List("P1", "P4", "P5"))))

  println(combinablePromotions("P3", testInput))
  assert(combinablePromotions("P3", testInput) == Seq(PromotionCombo(List("P2", "P3")), PromotionCombo(List("P3", "P4", "P5"))))

}
