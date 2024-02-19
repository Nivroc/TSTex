
object Problem1 extends App {

  case class Rate(rateCode: String, rateGroup: String)

  case class CabinPrice(cabinCode: String,
                        rateCode: String,
                        price: BigDecimal)

  case class BestGroupPrice(cabinCode: String,
                            rateCode: String,
                            price: BigDecimal,
                            rateGroup: String)

  def getBestGroupPrices(rates: Seq[Rate],
                          prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {
    val rateMap = rates.map(rate => rate.rateCode -> rate.rateGroup).toMap
    prices.collect { case CabinPrice(cabinCode, rateCode, price) if rateMap.isDefinedAt(rateCode) =>
                            BestGroupPrice(cabinCode, rateCode, price, rateMap(rateCode)) }
          .groupMapReduce
            { case BestGroupPrice(cabinCode, _, _, rateGroup) => cabinCode -> rateGroup }
            { case BestGroupPrice(_, rateCode, price, _) => rateCode -> price }
            {
              case (p1@(_, price1), (_, price2)) if price1 <= price2 => p1
              case (_, p2) => p2
            }
          .map{ case ((cc, rg), (rc, prc)) => BestGroupPrice(cc, rc, prc, rg) }
          .toSeq
  }


  val rates = Seq(Rate("M1", "Military"),
    Rate("M2", "Military"),
    Rate("S1", "Senior"),
    Rate("S2", "Senior"))

  val prices = Seq(
    CabinPrice("CA", "M1", 200.00),
    CabinPrice("CA", "M2", 250.00),
    CabinPrice("CA", "S1", 225.00),
    CabinPrice("CA", "S2", 260.00),
    CabinPrice("CB", "M1", 230.00),
    CabinPrice("CB", "M2", 260.00),
    CabinPrice("CB", "S1", 245.00),
    CabinPrice("CB", "S2", 270.00)
  )

  val expectedResult = Seq(
    BestGroupPrice("CA", "S1", 225.00, "Senior"),
    BestGroupPrice("CB", "S1", 245.00, "Senior"),
    BestGroupPrice("CA", "M1", 200.00, "Military"),
    BestGroupPrice("CB", "M1", 230.00, "Military")
  )

  println(getBestGroupPrices(rates, prices))

  assert(getBestGroupPrices(rates, prices) == expectedResult)

  assert(getBestGroupPrices(Seq(), Seq()) == Seq())

  assert(getBestGroupPrices(Seq(Rate("M1", "Military")), Seq(CabinPrice("CA", "M1", 200.00)))
    == Seq(BestGroupPrice("CA", "M1", 200.00, "Military")))

  assert(getBestGroupPrices(Seq(Rate("M1", "Military")), Seq(CabinPrice("CA", "M1", 200.00), CabinPrice("CA", "M1", 150.00), CabinPrice("CA", "M1", 225.00)))
    == Seq(BestGroupPrice("CA", "M1", 150.00, "Military")))

  assert(getBestGroupPrices(Seq(Rate("M1", "Military")), Seq(CabinPrice("CA", "M1", 0), CabinPrice("CA", "M2", 0), CabinPrice("CA", "S1", 0)))
    == Seq(BestGroupPrice("CA", "M1", 0, "Military")))

  assert(getBestGroupPrices(Seq(Rate("M1", "Military")), Seq(CabinPrice("CA", "M1", 0), CabinPrice("CB", "M1", 0)))
    == Seq(BestGroupPrice("CA", "M1", 0, "Military"), BestGroupPrice("CB", "M1", 0, "Military")))

  assert(getBestGroupPrices(Seq(Rate("M1", "Military"), Rate("M2", "Military")), Seq(CabinPrice("CA", "M2", 0)))
    == Seq(BestGroupPrice("CA", "M2", 0, "Military")))

  assert(getBestGroupPrices(Seq(Rate("M1", "Military"), Rate("M2", "Military")), Seq(CabinPrice("CA", "M1", 1), CabinPrice("CB", "M2", 2)))
    == Seq(BestGroupPrice("CA", "M1", 1, "Military"),BestGroupPrice("CB", "M2", 2, "Military")))

  assert(getBestGroupPrices(Seq(Rate("M1", "Military"), Rate("M2", "Military")), Seq(CabinPrice("CA", "M1", 1), CabinPrice("CB", "M2", 2)))
    == Seq(BestGroupPrice("CA", "M1", 1, "Military"),BestGroupPrice("CB", "M2", 2, "Military")))

  assert(getBestGroupPrices(Seq(Rate("M1", "Military"), Rate("M3", "Military")), Seq(CabinPrice("CA", "M1", 1), CabinPrice("CB", "M1", 2)))
    == Seq(BestGroupPrice("CA", "M1", 1, "Military"),BestGroupPrice("CB", "M1", 2, "Military")))

  assert(getBestGroupPrices(Seq(Rate("M1", "Military")), Seq(CabinPrice("CA", "M3", 1), CabinPrice("CB", "M3", 2))) == Seq())

  assert(getBestGroupPrices(Seq(), Seq(CabinPrice("CA", "S3", 1), CabinPrice("CB", "S3", 2))) == Seq())

  assert(getBestGroupPrices(Seq(Rate("M1", "Military")), Seq()) == Seq())

  assert(getBestGroupPrices(Seq(Rate("M1", "Military")), Seq(CabinPrice("CA", "M1", 1), CabinPrice("CB", "M3", 2)))
    == Seq(BestGroupPrice("CA", "M1", 1, "Military")))

  assert(getBestGroupPrices(Seq(Rate("M1", "Military"), Rate("S1", "Senior")), Seq(CabinPrice("CA", "M1", 1), CabinPrice("CB", "S1", 2)))
    == Seq(BestGroupPrice("CB", "S1", 2, "Senior"), BestGroupPrice("CA", "M1", 1, "Military")))

  assert(getBestGroupPrices(Seq(Rate("M1", "Military"), Rate("S1", "Senior")), Seq(CabinPrice("CA", "M1", 2), CabinPrice("CB", "S1", 1)))
    == Seq(BestGroupPrice("CB", "S1", 1, "Senior"), BestGroupPrice("CA", "M1", 2, "Military")))

}
