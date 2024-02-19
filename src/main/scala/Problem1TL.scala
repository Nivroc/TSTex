import HighPrioBoolImplicits._
import Price._
import Ord._
import RateCode._
import RateGroup._
import Comp._
import HigherPrioPriceLessThan._
import CabinCode._
import HighPrioMinCabinPriceForImplicits._
import HighPrioCabinCodeExists._
import HighPrioRateGroupExists._
import ExtractCabinCodes._
import AppendBestPrices._
import ExtractRateGroups._
import FilterOutNotFound.summonFilterNotFound
import FindRateGroup.summonFRG
import GetBestGroupPrices._
import HuntForSpecificCode.summonHuntSpecific
import MinCabinPriceFor.minCabinPriceFor
import HighPrioFilterNotFound._


/* If you've opened this first please proceed to Problem1.scala or Problem2.scala for regular runtime solutions and come back later

   What this is: treat this with a bit of humor, this is a full typelevel solution of Problem 1 with no runtime needed.
   This is not meant anywhere near production and is only a showcase of Scala type system.

   How to run: No need to run, correctness can be checked in the IDE or REPL with :t command

   Details and limitations:
   - No shapeless or derivation libraries used, code is self-sufficient
   - Price is a natural number from 0 to 19, 20 should be treated as absolute maximum price that could never occur. That
     means the initial test case has been altered to keep the pricing order, but with natural numbers. Done for simplicity,
     performance and file size.
   - HashCode computation would be a PhD-worth of a task so this approach is in essence a big nested loop(but with types)
   - Higher arity type families are fixed in their family constructor as it's much easier to debug and read for a solution this size
   - I tried to make naming and formatting convenient and readable, but it could use some more work

   Below only test cases for the main function. More granular test cases can be found at the end of file in AdditionalTests object
 */

object Problem1Typelevel {
  //Seq(Rate("M1", "Military"), Rate("M2", "Military"), Rate("S1", "Senior"), Rate("S2", "Senior"))
  type Rts = ConsRates[Rate[M1.type, Military.type],
                ConsRates[Rate[M2.type, Military.type],
                  ConsRates[Rate[S1.type, Senior.type],
                    ConsRates[Rate[S2.type, Senior.type],
                      NoRates.type]]]]
  //Seq(CabinPrice("CA", "M1", 200.00),CabinPrice("CA", "M2", 250.00),CabinPrice("CA", "S1", 225.00),CabinPrice("CA", "S2", 260.00),CabinPrice("CB", "M1", 230.00),CabinPrice("CB", "M2", 260.00),CabinPrice("CB", "S1", 245.00),CabinPrice("CB", "S2", 270.00))
  //200 = _2
  //225 = _3
  //230 = _4
  //245 = _5
  //250 = _6
  //260 = _7
  //270 = _8
  type Prs = ConsPrices[CabinPrice[CA.type, M1.type, _2], //CabinPrice("CA", "M1", 200.00)
               ConsPrices[CabinPrice[CA.type, M2.type, _6], //CabinPrice("CA", "M2", 250.00)
                 ConsPrices[CabinPrice[CA.type, S1.type, _3], //CabinPrice("CA", "S1", 225.00)
                   ConsPrices[CabinPrice[CA.type, S2.type, _7], //CabinPrice("CA", "S2", 260.00)
                     ConsPrices[CabinPrice[CB.type, M1.type, _4], //CabinPrice("CB", "M1", 230.00)
                       ConsPrices[CabinPrice[CB.type, M2.type, _7], //CabinPrice("CB", "M2", 260.00)
                         ConsPrices[CabinPrice[CB.type, S1.type, _5], //CabinPrice("CB", "S1", 245.00)
                           ConsPrices[CabinPrice[CB.type, S2.type, _8], //CabinPrice("CB", "S2", 270.00)
                             NoPrices.type]]]]]]]]

  //BestGroupPrice(CA, M1, 200.00, Military)
  //BestGroupPrice(CA, S1, 225.00, Senior)
  //BestGroupPrice(CB, M1, 230.00, Military)
  //BestGroupPrice(CB, S1, 245.00, Senior)
  type ExpectedResult = ConsBestPrices[BestGroupPrice[CabinCode.CA.type, RateCode.S1.type, _3, RateGroup.Senior.type],
                        ConsBestPrices[BestGroupPrice[CabinCode.CA.type, RateCode.M1.type, _2, RateGroup.Military.type],
                        ConsBestPrices[BestGroupPrice[CabinCode.CB.type, RateCode.S1.type, _5, RateGroup.Senior.type],
                        ConsBestPrices[BestGroupPrice[CabinCode.CB.type, RateCode.M1.type, _4, RateGroup.Military.type],
                        NoBestPrices.type]]]]

  //!!!HERE IS THE SAMPLE SOLUTION!!!

  val checkMyTypeInReplOrInHere = getBestGroupPrices[Rts, Prs] // REPL: :t Problem1Typelevel.checkMyTypeInReplOrInHere

  val typeCheck: ExpectedResult = getBestGroupPrices[Rts, Prs] // same typecheck in a different way

  implicitly[GetBestGroupPrices.Aux[Rts, Prs, ExpectedResult]] // proof using implicits

  //Some additional cases
  val test1: NoBestPrices.type = getBestGroupPrices[NoRates.type, NoPrices.type]
  val test2: NoBestPrices.type = getBestGroupPrices[ConsRates[Rate[M1.type, Military.type], NoRates.type], NoPrices.type]
  val test3: NoBestPrices.type = getBestGroupPrices[NoRates.type, ConsPrices[CabinPrice[CA.type, M1.type, _3], NoPrices.type]]

  val test4: ConsBestPrices[BestGroupPrice[CabinCode.CA.type, RateCode.M1.type, _3, RateGroup.Military.type], NoBestPrices.type] =
    getBestGroupPrices[ConsRates[Rate[M1.type, Military.type], NoRates.type], ConsPrices[CabinPrice[CA.type, M1.type, _3], NoPrices.type]]

  val test5: NoBestPrices.type = getBestGroupPrices[ConsRates[Rate[M1.type, Military.type], NoRates.type], ConsPrices[CabinPrice[CA.type, M2.type, _3], NoPrices.type]]

  val test6: ConsBestPrices[BestGroupPrice[CabinCode.CA.type, RateCode.M1.type, _3, RateGroup.Military.type], NoBestPrices.type] =
    getBestGroupPrices[ConsRates[Rate[M1.type, Military.type], NoRates.type], ConsPrices[CabinPrice[CA.type, M1.type, _5], ConsPrices[CabinPrice[CA.type, M1.type, _3], NoPrices.type]]]

  val test7: ConsBestPrices[BestGroupPrice[CabinCode.CA.type, RateCode.M2.type, _3, RateGroup.Military.type], NoBestPrices.type] =
    getBestGroupPrices[ConsRates[Rate[M2.type, Military.type], ConsRates[Rate[M1.type, Military.type], NoRates.type]], ConsPrices[CabinPrice[CA.type, M1.type, _5], ConsPrices[CabinPrice[CA.type, M2.type, _3], NoPrices.type]]]

  val test8: ConsBestPrices[BestGroupPrice[CabinCode.CA.type, RateCode.S1.type, _3, RateGroup.Senior.type], ConsBestPrices[BestGroupPrice[CabinCode.CA.type, RateCode.M1.type, _5, RateGroup.Military.type], NoBestPrices.type]] =
    getBestGroupPrices[ConsRates[Rate[M1.type, Military.type], ConsRates[Rate[S1.type, Senior.type], NoRates.type]], ConsPrices[CabinPrice[CA.type, M1.type, _5], ConsPrices[CabinPrice[CA.type, S1.type, _3], NoPrices.type]]]

  //In IntelliJ you can just write
  val test9 = getBestGroupPrices[ConsRates[Rate[M1.type, Military.type], ConsRates[Rate[S1.type, Senior.type], NoRates.type]], ConsPrices[CabinPrice[CA.type, M1.type, _5], ConsPrices[CabinPrice[CA.type, S1.type, _3], NoPrices.type]]]
  //and add type annotation via "Show Context Actions -> Add type annotation" to calculate the result
}



trait GetBestGroupPrices[Rts <: Rates, Prs <: CabinPrices]{ type Out <: BestPrices }
object GetBestGroupPrices{
  type Aux[Rts <: Rates, Prs <: CabinPrices, Result <: BestPrices] = GetBestGroupPrices[Rts, Prs]{ type Out = Result }

  implicit def solution[Rts <: Rates, Prs <: CabinPrices, UniqueCodes <: CabinCodes, UniqueRGs <: RateGroups, Result <: BestPrices, FilterResult <: BestPrices]
    (implicit UniqueCodes: ExtractCabinCodes.Aux[Prs, UniqueCodes],
              UniqueRateGroups: ExtractRateGroups.Aux[Rts, UniqueRGs],
              BestPrices: HuntPrices.Aux[UniqueCodes, UniqueRGs, Prs, Rts, Result],
              Filter: FilterOutNotFound.Aux[Result, FilterResult]
    ): Aux[Rts, Prs, FilterResult] = ???

  def getBestGroupPrices[Rts <: Rates, Prs <: CabinPrices](implicit getBestGroupPrices: GetBestGroupPrices[Rts, Prs]): getBestGroupPrices.Out = ???

}

trait FilterOutNotFound[Input <: BestPrices]{ type Out <: BestPrices }
object FilterOutNotFound{
  type Aux[Input <: BestPrices, Result <: BestPrices] = FilterOutNotFound[Input]{ type Out = Result }

  def summonFilterNotFound[Input <: BestPrices](implicit F: FilterOutNotFound[Input]): F.Out = ???
}

trait LowPrioFilterNotFound{
  implicit def filterDrop[
    CC <: CabinCode, RC <: RateCode, P <: Price, RG <: RateGroup, Result <: BestPrices,
    Tail <: BestPrices, TailRec <: BestPrices,
    Res1 <: BBool, Res2 <: BBool, Res3 <: BBool
  ](
     implicit RCIsNULL: TyEq.Aux0[RC, NULLRC.type, Res1],
     PriceNotFound: TyEq.Aux0[P, MaximumPossiblePrice, Res2],
     OneOf: Or.Aux0[Res1, Res2, Res3],
     FilterCondEv: Res3 =:= BTrue.type,
     FilterRestOfList: FilterOutNotFound.Aux[Tail, TailRec]
   ): FilterOutNotFound.Aux[ConsBestPrices[BestGroupPrice[CC, RC, P, RG], Tail], TailRec] = ???
}

trait MiddlePrioFilterNotFound extends LowPrioFilterNotFound{
  implicit def filterTake[
    CC <: CabinCode, RC <: RateCode, P <: Price, RG <: RateGroup, Result <: BestPrices,
    Tail <: BestPrices, TailRec <: BestPrices, TailResult <: BestPrices,
    Res1 <: BBool, Res2 <: BBool, Res3 <: BBool
  ](
     implicit RCIsNULL: TyEq.Aux0[RC, NULLRC.type, Res1],
     PriceNotFound: TyEq.Aux0[P, MaximumPossiblePrice, Res2],
     OneOf: Or.Aux0[Res1, Res2, Res3],
     FilterCondEv: Res3 =:= BFalse.type,
     FilterRestOfList: FilterOutNotFound.Aux[Tail, TailRec],
     Take: AppendBestPrices.Aux[ConsBestPrices[BestGroupPrice[CC, RC, P, RG], NoBestPrices.type], TailRec, TailResult]
   ): FilterOutNotFound.Aux[ConsBestPrices[BestGroupPrice[CC, RC, P, RG], Tail], TailResult] = ???

}

object HighPrioFilterNotFound extends MiddlePrioFilterNotFound {
  implicit val filterEnd: FilterOutNotFound.Aux[NoBestPrices.type, NoBestPrices.type] = ???
}

trait HuntPrices[UniqueCodes <: CabinCodes, UniqueRGs <: RateGroups, Prs <: CabinPrices]{ type Out <: BestPrices}
object HuntPrices{
  type Aux[UniqueCodes <: CabinCodes, UniqueRGs <: RateGroups, Prs <: CabinPrices, Rts <: Rates, Result <: BestPrices] = HuntPrices[UniqueCodes, UniqueRGs, Prs]{ type Out = Result }

  implicit def huntGo[
    UniqueCodes <: CabinCodes, UniqueRGs <: RateGroups, Prs <: CabinPrices, Rts <: Rates,
    Result <: BestPrices, CC <: CabinCode, Tail <: CabinCodes, ResultForCode <: BestPrices, TailRec <: BestPrices
  ](
      implicit This: HuntForSpecificCode.Aux[CC, UniqueRGs, Prs, Rts, ResultForCode],
               Recurse: HuntPrices.Aux[Tail, UniqueRGs, Prs, Rts, TailRec],
               Append: AppendBestPrices.Aux[ResultForCode, TailRec, Result]
    ): Aux[ConsCabinCodes[CC, Tail], UniqueRGs, Prs, Rts, Result] = ???

  implicit def huntEnd[UniqueRGs <: RateGroups, Prs <: CabinPrices, Rts <: Rates]: Aux[NoCabinCodes.type, UniqueRGs, Prs, Rts, NoBestPrices.type ] = ???

  def summonHunt[UniqueCodes <: CabinCodes, UniqueRGs <: RateGroups, Prs <: CabinPrices](implicit H: HuntPrices[UniqueCodes, UniqueRGs, Prs]): H.Out = ???
}

trait AppendBestPrices[A <: BestPrices, B <: BestPrices]{type Out <: BestPrices}
object AppendBestPrices{
  type Aux[A <: BestPrices, B <: BestPrices, Result <: BestPrices] = AppendBestPrices[A, B]{type Out = Result}

  implicit def goABP[BP <: BestGroupPrice[_, _, _, _], Tail1 <: BestPrices, Tail2 <: BestPrices, Res <: BestPrices](
        implicit Recurse: AppendBestPrices.Aux[Tail1, ConsBestPrices[BP, Tail2], Res]
    ): AppendBestPrices.Aux[ConsBestPrices[BP, Tail1], Tail2, Res] = ???

  implicit def goABPEndLeft[Tail <: BestPrices]: AppendBestPrices.Aux[NoBestPrices.type, Tail, Tail] = ???

  def appendBestPrices[A <: BestPrices, B <: BestPrices](implicit Ap: AppendBestPrices[A, B]): Ap.Out = ???
}

trait HuntForSpecificCode[CC <: CabinCode, UniqueRGs <: RateGroups, P <: CabinPrices, Rts <: Rates]{type Out <: BestPrices}
object HuntForSpecificCode{
  type Aux[CC <: CabinCode, UniqueRGs <: RateGroups, P <: CabinPrices, Rts <: Rates, Result <: BestPrices] =
    HuntForSpecificCode[CC, UniqueRGs, P, Rts]{type Out = Result}

  implicit def goHSC[
    CC <: CabinCode, RG <: RateGroup, Tail <: RateGroups, ResRC <: RateCode, ResP <: Price,
    TailRec <: BestPrices, P <: CabinPrices, Rts <: Rates, Result <: BestPrices, Best <: BestGroupPrice[_, _, _, _]
  ](
      implicit SearchForTheBestPrice: MinCabinPriceFor.Aux[CC, RG, MaximumPossiblePrice, NULLRC.type, P, Rts, Best],
               Recurse: HuntForSpecificCode.Aux[CC, Tail, P, Rts, TailRec],
               Append: AppendBestPrices.Aux[ConsBestPrices[Best, NoBestPrices.type], TailRec, Result]
    ): Aux[CC, ConsRateGroups[RG, Tail], P, Rts, Result] = ???

  implicit def endHSC[CC <: CabinCode, P <: CabinPrices, Rts <: Rates]: Aux[CC, NoRateGroups.type, P, Rts, NoBestPrices.type] = ???

  def summonHuntSpecific[CC <: CabinCode, UniqueRGs <: RateGroups, P <: CabinPrices, Rts <: Rates](implicit H:HuntForSpecificCode[CC, UniqueRGs, P, Rts]): H.Out = ???
}

trait MinCabinPriceFor[CC <: CabinCode, RG <: RateGroup, P <: Price, CurrentRC <: RateCode, Prices <: CabinPrices, RateGrps <: Rates]{ type Out <: BestGroupPrice[_, _, _, _] }
object MinCabinPriceFor {
  type Aux[CC <: CabinCode, RG <: RateGroup, P <: Price, CurrentRC <: RateCode, Prices <: CabinPrices, RateGrps <: Rates, Result <: BestGroupPrice[_, _, _, _]] =
    MinCabinPriceFor[CC, RG, P, CurrentRC, Prices, RateGrps]{ type Out = Result }

  def minCabinPriceFor[CC <: CabinCode, RG <: RateGroup, P <: Price, CurrentRC <: RateCode, Prices <: CabinPrices, Rts <: Rates](implicit MCP: MinCabinPriceFor[CC, RG, P, CurrentRC, Prices, Rts]): MCP.Out = ???

}

trait LowPrioMinCabinPriceForImplicits{
  implicit def mcpfNext[
    CC <: CabinCode, RG <: RateGroup, Rts <: Rates, Tail <: CabinPrices,
    CurrentLowestPrice <: Price, CurrentRC <: RateCode,
    CandCCode <: CabinCode, CandRCode <: RateCode, PriceCandidate <: Price,
    Result <: BestGroupPrice[_, _, _, _]
  ](implicit
    resultInTail: MinCabinPriceFor.Aux[CC, RG, CurrentLowestPrice, CurrentRC, Tail, Rts, Result]
   ): MinCabinPriceFor.Aux[CC, RG, CurrentLowestPrice, CurrentRC, ConsPrices[CabinPrice[CandCCode,CandRCode,PriceCandidate], Tail], Rts, Result] = ???
}

trait MiddlePrioMinCabinPriceForImplicits extends LowPrioMinCabinPriceForImplicits{
  implicit def mcpfTake[
    CC <: CabinCode, RG <: RateGroup, Rts <: Rates, Tail <: CabinPrices,
    CurrentLowestPrice <: Price, CurrentRC <: RateCode,
    PriceCandidate <: Price, CandRGroup <: RateGroup, CandCCode <: CabinCode, CandRCode <: RateCode,
    CabinCodeGood <: BBool, RateGroupGood <: BBool, BothChecksGood <: BBool, Taking <: BBool, PriceIsLower <: BBool,
    Result <: BestGroupPrice[_, _, _, _]
  ](
     implicit
     candidateCabinCodeCheck: TyEq.Aux0[CC, CandCCode, CabinCodeGood],
     findRateGroup: FindRateGroup.Aux[CandRCode, Rts, CandRGroup],
     candidateRateGroupCheck: TyEq.Aux0[RG, CandRGroup, RateGroupGood],
     bothChecksGood: And.Aux0[CabinCodeGood, RateGroupGood, BothChecksGood],
     candidatePriceIsLower: PriceLessThan.Aux0[PriceCandidate, CurrentLowestPrice, PriceIsLower],
     oneOrTheOther: And.Aux0[BothChecksGood, PriceIsLower, Taking],
     goingToTakeEv: Taking =:= BTrue.type,
     resultInTail: MinCabinPriceFor.Aux[CC, RG, PriceCandidate, CandRCode, Tail, Rts, Result]
   ): MinCabinPriceFor.Aux[CC, RG, CurrentLowestPrice, CurrentRC, ConsPrices[CabinPrice[CandCCode,CandRCode,PriceCandidate], Tail], Rts, Result] = ???

}

object HighPrioMinCabinPriceForImplicits extends MiddlePrioMinCabinPriceForImplicits {
  implicit def mcpfEnd[
    CC <: CabinCode, RG <: RateGroup, Rts <: Rates, Tail <: CabinPrices,
    CurrentRC <: RateCode, CurrentLowestPrice <: Price,
    CabinCodeGood <: BBool, RateGroupGood <: BBool, BothChecksGood <: BBool, Taking <: BBool, PriceIsLower <: BBool,
    Result <: BestGroupPrice[_, _, _, _]
  ]: MinCabinPriceFor.Aux[CC, RG, CurrentLowestPrice, CurrentRC, NoPrices.type , Rts, BestGroupPrice[CC, CurrentRC, CurrentLowestPrice, RG]] = ???
}


trait FindRateGroup[RCode <: RateCode, Rts <: Rates]{type Out <: RateGroup}
object FindRateGroup{
  type Aux[RCode <: RateCode, Rts <: Rates, Result <: RateGroup] = FindRateGroup[RCode, Rts]{type Out = Result}

  implicit def foundRG[RCode <: RateCode, RG <: RateGroup, RC <: RateCode, Tail <: Rates, Res <: BBool](
     implicit yes: TyEq.Aux0[RCode, RC, Res],
              ev: Res =:= BTrue.type
     ): Aux[RCode, ConsRates[Rate[RC, RG], Tail], RG] = ???

  implicit def keepSearchRG[RCode <: RateCode, RG <: RateGroup, RC <: RateCode, Tail <: Rates, Res <: BBool, ResRG <: RateGroup](
     implicit yes: TyEq.Aux0[RCode, RC, Res],
              ev: Res =:= BFalse.type,
              Recurse: FindRateGroup.Aux[RCode, Tail, ResRG]
     ): Aux[RCode, ConsRates[Rate[RC, RG], Tail], ResRG] = ???

  def summonFRG[RCode <: RateCode, Rts <: Rates](implicit findRateGroup: FindRateGroup[RCode, Rts]): findRateGroup.Out = ???
}


///////////////
trait CabinCodeExists[CC <: CabinCode, Codes <: CabinCodes]{type Out <: BBool}
object CabinCodeExists{
  type Aux[CC <: CabinCode, Codes <: CabinCodes, Result <: BBool] =  CabinCodeExists[CC, Codes]{type Out = Result}
  def existsIn[CC <: CabinCode, Codes <: CabinCodes]( implicit TC: CabinCodeExists[CC, Codes]): TC.Out = ???
}
trait LowPrioCabinCodeExists{
  implicit def takeEICC[CC <: CabinCode, Tail <: CabinCodes]: CabinCodeExists.Aux[CC, ConsCabinCodes[CC, Tail], BTrue.type ] = ???
}
trait MiddlePrioCabinCodeExists extends LowPrioCabinCodeExists{
  implicit def nextEICC[CC <: CabinCode, Tail <: CabinCodes, CCC <: CabinCode, Res <: BBool, Res2 <: BBool](
    implicit eq: TyEq.Aux0[CC, CCC, Res], ev: Res =:= BFalse.type, recurse: CabinCodeExists.Aux[CC, Tail, Res2]
  ): CabinCodeExists.Aux[CC, ConsCabinCodes[CCC, Tail], Res2] = ???
}
object HighPrioCabinCodeExists extends MiddlePrioCabinCodeExists{
  implicit def endEICC[CC <: CabinCode]: CabinCodeExists.Aux[CC, NoCabinCodes.type, BFalse.type ] = ???
}

trait CabinCodes
object NoCabinCodes extends CabinCodes
class ConsCabinCodes[R <: CabinCode, Tail <: CabinCodes] extends CabinCodes

trait ExtractCabinCodes[Prices <: CabinPrices]{type Out <: CabinCodes}
object ExtractCabinCodes{
  type Aux[Prices <: CabinPrices, Result <: CabinCodes] = ExtractCabinCodes[Prices]{ type Out = Result }
  implicit def endECC: ExtractCabinCodes.Aux[NoPrices.type, NoCabinCodes.type] = ???

  implicit def takeECC[CC <: CabinCode, RC <: RateCode, P <: Price, Tail <: CabinPrices, TailCC <: CabinCodes, Res <: BBool]
    (implicit recurse: ExtractCabinCodes.Aux[Tail, TailCC], ev: CabinCodeExists.Aux[CC, TailCC, Res], ev2: Res =:= BFalse.type)
    : ExtractCabinCodes.Aux[ConsPrices[CabinPrice[CC, RC, P], Tail], ConsCabinCodes[CC, TailCC]] = ???

  implicit def nextECC[CC <: CabinCode, RC <: RateCode, P <: Price, Tail <: CabinPrices, TailCC <: CabinCodes, Res <: BBool]
    (implicit recurse: ExtractCabinCodes.Aux[Tail, TailCC], ev: CabinCodeExists.Aux[CC, TailCC, Res], ev2: Res =:= BTrue.type  )
    : ExtractCabinCodes.Aux[ConsPrices[CabinPrice[CC, RC, P], Tail], TailCC] = ???

  def extractCabinCodes[Prices <: CabinPrices](implicit calc: ExtractCabinCodes[Prices]): calc.Out = ???
}

//////////////////////
trait RateGroupExists[CC <: RateGroup, Codes <: RateGroups]{type Out <: BBool}
object RateGroupExists{
  type Aux[CC <: RateGroup, Codes <: RateGroups, Result <: BBool] = RateGroupExists[CC, Codes]{type Out = Result}
  def RGexistsIn[CC <: RateGroup, Codes <: RateGroups]( implicit TC: RateGroupExists[CC, Codes]): TC.Out = ???
}
trait LowPrioRateGroupExists{
  implicit def takeRGE[CC <: RateGroup, Tail <: RateGroups]: RateGroupExists.Aux[CC, ConsRateGroups[CC, Tail], BTrue.type ] = ???
}
trait MiddlePrioRateGroupExists extends LowPrioRateGroupExists{
  implicit def nextRGE[CC <: RateGroup, Tail <: RateGroups, CCC <: RateGroup, Res <: BBool, Res2 <: BBool](
    implicit eq: TyEq.Aux0[CC, CCC, Res], ev: Res =:= BFalse.type, recurse: RateGroupExists.Aux[CC, Tail, Res2]
  ): RateGroupExists.Aux[CC, ConsRateGroups[CCC, Tail], Res2] = ???
}
object HighPrioRateGroupExists extends MiddlePrioRateGroupExists{
  implicit def endRGE[CC <: RateGroup]: RateGroupExists.Aux[CC, NoRateGroups.type, BFalse.type ] = ???
}

trait RateGroups
object NoRateGroups extends RateGroups
class ConsRateGroups[R <: RateGroup, Tail <: RateGroups] extends RateGroups

trait ExtractRateGroups[Rts <: Rates]{type Out <: RateGroups}
object ExtractRateGroups{
  type Aux[Rts <: Rates, Result <: RateGroups] = ExtractRateGroups[Rts]{ type Out = Result }
  implicit def endERG: ExtractRateGroups.Aux[NoRates.type, NoRateGroups.type] = ???

  implicit def takeERG[RG <: RateGroup, RC <: RateCode, Tail <: Rates, TailCC <: RateGroups, Res <: BBool]
    (implicit recurse: ExtractRateGroups.Aux[Tail, TailCC],
              ev: RateGroupExists.Aux[RG, TailCC, Res],
              ev2: Res =:= BFalse.type)
    : ExtractRateGroups.Aux[ConsRates[Rate[RC, RG], Tail], ConsRateGroups[RG, TailCC]] = ???

  implicit def nextERG[RG <: RateGroup, RC <: RateCode, Tail <: Rates, TailCC <: RateGroups, Res <: BBool]
  (implicit recurse: ExtractRateGroups.Aux[Tail, TailCC],
            ev: RateGroupExists.Aux[RG, TailCC, Res],
            ev2: Res =:= BTrue.type)
    : ExtractRateGroups.Aux[ConsRates[Rate[RC, RG], Tail], TailCC] = ???

  def extractRateGroups[Rts <: Rates](implicit calc: ExtractRateGroups[Rts]): calc.Out = ???

}

//Domain modelling
//case class Rate(rateCode: String, rateGroup: String)
class Rate[RC <: RateCode, RG <: RateGroup]{
  type RCode = RC
  type RGroup = RC
}
object Rate {
  def apply[RC <: RateCode, RG <: RateGroup]: Rate[RC, RG] = new Rate[RC, RG]
}

//case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)
class CabinPrice[CC <: CabinCode, RC <: RateCode, P <: Price]{
  type RCode = RC
  type Pr = P
}
object CabinPrice {
  def apply[CC <: CabinCode, RC <: RateCode, P <: Price]: CabinPrice[CC, RC, P] = new CabinPrice[CC, RC, P]
}

//case class BestGroupPrice(cabinCode: String, rateCode: String, price: BigDecimal, rateGroup: String)
class BestGroupPrice[CC <: CabinCode, RC <: RateCode, P <: Price, RG <: RateGroup]
object BestGroupPrice {
  def apply[CC <: CabinCode, RC <: RateCode, P <: Price, RG <: RateGroup]: BestGroupPrice[CC, RC, P, RG] = new BestGroupPrice[CC, RC, P, RG]
}

trait Rates
object NoRates extends Rates
class ConsRates[R <: Rate[_, _], Tail <: Rates] extends Rates

trait CabinPrices {
  type append[CC <: CabinCode, RC <: RateCode, P <: Price] <: CabinPrices
}
object NoPrices extends CabinPrices {
  override type append[CC <: CabinCode, RC <: RateCode, P <: Price] = ConsPrices[CabinPrice[CC, RC, P], NoPrices.type]
}
class ConsPrices[R <: CabinPrice[_, _, _], Tail <: CabinPrices] extends CabinPrices {
  override type append[CC <: CabinCode, RC <: RateCode, P <: Price] = ConsPrices[CabinPrice[CC, RC, P], this.type]
  type head = R
  type tail = Tail
}

trait BestPrices
object NoBestPrices extends BestPrices
class ConsBestPrices[R <: BestGroupPrice[_, _, _, _], Tail <: BestPrices] extends BestPrices

sealed trait RateCode
object RateCode {
  case object M1 extends RateCode
  case object M2 extends RateCode
  case object S1 extends RateCode
  case object S2 extends RateCode
  case object NULLRC extends RateCode
}

sealed trait CabinCode
object CabinCode {
  case object CA extends CabinCode
  case object CB extends CabinCode
  case object NULLCC extends CabinCode
}

sealed trait RateGroup
object RateGroup {
  case object Military extends RateGroup
  case object Senior extends RateGroup
  case object NULLRG extends RateGroup
}

trait Price
trait Succ[A <: Price] extends Price
object Price {
  trait Z extends Price
  type _1 = Succ[Z]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]
  type _6 = Succ[_5]
  type _7 = Succ[_6]
  type _8 = Succ[_7]
  type _9 = Succ[_8]
  type _10 = Succ[_9]
  type _11 = Succ[_10]
  type _12 = Succ[_11]
  type _13 = Succ[_12]
  type _14 = Succ[_13]
  type _15 = Succ[_14]
  type _16 = Succ[_15]
  type _17 = Succ[_16]
  type _18 = Succ[_17]
  type _19 = Succ[_18]
  type MaximumPossiblePrice = Succ[_19]
}

//Helper Type Functions
trait PriceLessThan[A <: Price, B <: Price] { type Out <: BBool }
object PriceLessThan{
  type Aux0[A <: Price, B <: Price, C <: BBool] = PriceLessThan[A, B] {type Out = C}
}
trait LowerPrioPriceLessThan{
  implicit def pltfalse1[A <: Price, B <: Price, Res <: Ord](implicit C: Comp.Aux[A, B, Ord.GT], ev: Res =:= Ord.GT): PriceLessThan.Aux0[A, B, BFalse.type] = ???
}
object HigherPrioPriceLessThan extends LowerPrioPriceLessThan {
  implicit def pltfalse2[A <: Price, B <: Price, Res <: Ord](implicit C: Comp.Aux[A, B, Res], ev: Res =:= Ord.EQ): PriceLessThan.Aux0[A, B, BFalse.type] = ???
  implicit def plttrue[A <: Price, B <: Price, Res <: Ord](implicit C: Comp.Aux[A, B, Res], ev: Res =:= Ord.LT): PriceLessThan.Aux0[A, B, BTrue.type] = ???
}

sealed trait Ord{
  type moreSpecific[B <: Ord] <: Ord
}
object Ord {
  trait GT extends Ord{
    override type moreSpecific[A <: Ord] = this.type
  }
  trait LT extends Ord{
    override type moreSpecific[A <: Ord] = this.type
  }
  trait EQ extends Ord{
    override type moreSpecific[A <: Ord] = A
  }
}

trait Comp[A, B]{ type Out <: Ord }
object Comp{
  type Aux[A, B, C <: Ord] = Comp[A, B] { type Out = C }
  implicit def zeroCase0: Comp.Aux[Z, Z, EQ] = ???
  implicit def zeroCase1[A <: Price]: Comp.Aux[Z, A, LT] = ???
  implicit def zeroCase2[A <: Price]: Comp.Aux[A, Z, GT] = ???
  implicit def succCase[A <: Price, B <: Price, O <: Ord](implicit comp: Comp.Aux[A, B, O]): Comp.Aux[Succ[A], Succ[B], O] = ???
}

trait BBool {
  type Not <: BBool
}
object BTrue extends BBool {
  override type Not = BFalse.type
}
object BFalse extends BBool{
  override type Not = BTrue.type
}
trait And[A <: BBool, B <: BBool] {type Out <: BBool}
object And {
  type Aux0[A <: BBool, B <: BBool, C <: BBool] = And[A, B]{type Out = C}
}
trait Or[A <: BBool, B <: BBool] {type Out <: BBool}
object Or {
  type Aux0[A <: BBool, B <: BBool, C <: BBool] = Or[A, B]{type Out = C}
}
trait TyEq[A, B]{type Out <: BBool}
object TyEq {
  type Aux0[A, B, C <: BBool] = TyEq[A, B] {type Out = C}
}
trait LowPrioBoolImplicits {
  implicit def elseCase1[A <: BBool, B <: BBool]: And.Aux0[A, B, BFalse.type] = ???
  implicit def elseCase2[A <: BBool, B <: BBool]: Or.Aux0[A, B, BTrue.type] = ???
  implicit def tyNotEq[A, B]: TyEq.Aux0[A, B, BFalse.type] = ???
}
object HighPrioBoolImplicits extends LowPrioBoolImplicits{
  implicit def trueCase[A <: BTrue.type, B <: BTrue.type]: And.Aux0[A, B, BTrue.type]  = ???
  implicit def flsCase[A <: BFalse.type, B <: BFalse.type]: Or.Aux0[A, B, BFalse.type]  = ???
  implicit def tyEq[A, B](implicit ev: A =:= B): TyEq.Aux0[A, B, BTrue.type] = ???
}

object AdditionalTests {

  implicitly[And.Aux0[BTrue.type, BFalse.type, BFalse.type]]

  implicitly[PriceLessThan.Aux0[Z, Z, BFalse.type]]
  implicitly[PriceLessThan.Aux0[Succ[Z], Succ[Z], BFalse.type]]
  implicitly[PriceLessThan.Aux0[Z, Succ[Z], BTrue.type]]
  implicitly[PriceLessThan.Aux0[Succ[Z], Succ[Z], BFalse.type]]

  implicitly[Comp.Aux[Succ[Succ[Z]], Succ[Succ[Z]], Ord.EQ]]
  implicitly[Comp.Aux[Succ[Z], Succ[Z], Ord.EQ]]
  implicitly[Comp.Aux[Z, Succ[Z], Ord.LT]]
  implicitly[Comp.Aux[Succ[Z], Z, Ord.GT]]

  implicitly[CabinCodeExists.Aux[CA.type, ConsCabinCodes[CA.type, NoCabinCodes.type], BTrue.type]]
  implicitly[CabinCodeExists.Aux[CA.type, ConsCabinCodes[CB.type, ConsCabinCodes[CA.type, NoCabinCodes.type]], BTrue.type]]
  implicitly[CabinCodeExists.Aux[CA.type, ConsCabinCodes[CB.type, NoCabinCodes.type], BFalse.type]]
  type Prs = ConsPrices[CabinPrice[CA.type, M1.type, _2], //CabinPrice("CA", "M1", 200.00)
    ConsPrices[CabinPrice[CA.type, M2.type, _6], //CabinPrice("CA", "M2", 250.00)
      ConsPrices[CabinPrice[CA.type, S1.type, _3], //CabinPrice("CA", "S1", 225.00)
        ConsPrices[CabinPrice[CA.type, S2.type, _7], //CabinPrice("CA", "S2", 260.00)
          ConsPrices[CabinPrice[CB.type, M1.type, _4], //CabinPrice("CB", "M1", 230.00)
            ConsPrices[CabinPrice[CB.type, M2.type, _7], //CabinPrice("CB", "M2", 260.00)
              ConsPrices[CabinPrice[CB.type, S1.type, _5], //CabinPrice("CB", "S1", 245.00)
                ConsPrices[CabinPrice[CB.type, S2.type, _8], //CabinPrice("CB", "S2", 270.00)
                  NoPrices.type]]]]]]]]


  type prices133 = ConsPrices[CabinPrice[CA.type, M2.type, _3], NoPrices.type]
  type prices15 = ConsPrices[CabinPrice[CB.type, M2.type, _3], ConsPrices[CabinPrice[CA.type, M2.type, _3], NoPrices.type]]

  implicitly[ExtractCabinCodes.Aux[NoPrices.type, NoCabinCodes.type]]
  implicitly[ExtractCabinCodes.Aux[prices133, ConsCabinCodes[CA.type, NoCabinCodes.type]]]
  implicitly[ExtractCabinCodes.Aux[prices15, ConsCabinCodes[CB.type, ConsCabinCodes[CA.type, NoCabinCodes.type]]]]
  type prices14 = ConsPrices[CabinPrice[CB.type, M2.type, _3],ConsPrices[CabinPrice[CA.type, M2.type, _3], NoPrices.type]]
  implicitly[ExtractCabinCodes.Aux[NoPrices.type, NoCabinCodes.type]]
  implicitly[ExtractCabinCodes.Aux[prices14, ConsCabinCodes[CB.type, ConsCabinCodes[CA.type, NoCabinCodes.type]]]]
  val checkMyType1 = extractCabinCodes[Prs]
  type prices1333 = ConsPrices[CabinPrice[CA.type, M2.type, _3],ConsPrices[CabinPrice[CA.type, M2.type, _3], NoPrices.type]]
  val checkMyType2 = extractCabinCodes[prices1333]

  val aa = appendBestPrices[NoBestPrices.type, NoBestPrices.type]
  val bb = appendBestPrices[ConsBestPrices[BestGroupPrice[CA.type, M1.type, _2, Military.type], NoBestPrices.type], NoBestPrices.type]
  val cc = appendBestPrices[ConsBestPrices[BestGroupPrice[CA.type, M1.type, _2, Military.type], NoBestPrices.type], ConsBestPrices[BestGroupPrice[CB.type, M2.type, _5, Senior.type], NoBestPrices.type]]

  type Prs2 = ConsPrices[CabinPrice[CA.type, M1.type, _2], //CabinPrice("CA", "M1", 200.00)
    ConsPrices[CabinPrice[CA.type, M2.type, _6], //CabinPrice("CA", "M2", 250.00)
      ConsPrices[CabinPrice[CA.type, S1.type, _3], //CabinPrice("CA", "S1", 225.00)
        ConsPrices[CabinPrice[CA.type, S2.type, _7], //CabinPrice("CA", "S2", 260.00)
          NoPrices.type]]]]
  type Rts2 = ConsRates[Rate[M1.type, Military.type],
    ConsRates[Rate[M2.type, Military.type],
      ConsRates[Rate[S1.type, Senior.type],
        ConsRates[Rate[S2.type, Senior.type],
          NoRates.type]]]]

  type Prs3 = ConsPrices[CabinPrice[CA.type, M1.type, _2], //CabinPrice("CA", "M1", 200.00)
    ConsPrices[CabinPrice[CA.type, S1.type, _3], //CabinPrice("CA", "S1", 225.00)
      NoPrices.type]]
  type Rts3 = ConsRates[Rate[M1.type, Military.type],
    ConsRates[Rate[S1.type, Senior.type],
      NoRates.type]]

  val a = getBestGroupPrices[Rts3, Prs3]
  val b = getBestGroupPrices[Rts2, Prs2]

  type Prs5 = ConsPrices[CabinPrice[CA.type, M1.type, _2], //CabinPrice("CA", "M1", 200.00)
    ConsPrices[CabinPrice[CB.type, M1.type, _4],
      NoPrices.type]]
  type Rts5 = ConsRates[Rate[S1.type, Senior.type], ConsRates[Rate[M1.type, Military.type], NoRates.type]]
  val c = getBestGroupPrices[Rts5, Prs5]

  //val hunt = summonHunt[ExtractCabinCodes[Prs3].Out, ExtractRateGroups[Rts3], Prs, Rts]
  type RtsF = ConsRates[Rate[M1.type, Military.type],
    ConsRates[Rate[M2.type, Military.type],
      ConsRates[Rate[S1.type, Senior.type],
        ConsRates[Rate[S2.type, Senior.type],
          NoRates.type]]]]

  val tstFRG1CheckMyType = summonFRG[S1.type, RtsF]
  val tstFRG1CheckMyType2 = summonFRG[S2.type, RtsF]
  val tstFRG1CheckMyType3 = summonFRG[M1.type, RtsF]
  val tstFRG1CheckMyType4 = summonFRG[M2.type, RtsF]

  type pricesM = ConsPrices[CabinPrice[CA.type, M1.type, _2], NoPrices.type]
  type prices2M = ConsPrices[CabinPrice[CA.type, M2.type, _5], ConsPrices[CabinPrice[CA.type, M1.type, _2], NoPrices.type]]
  type prices3M = ConsPrices[CabinPrice[CA.type, M1.type, _1],ConsPrices[CabinPrice[CA.type, M2.type, _5], ConsPrices[CabinPrice[CA.type, M1.type, _2], NoPrices.type]]]
  type prices4M = ConsPrices[CabinPrice[CA.type, S1.type, _4],ConsPrices[CabinPrice[CA.type, M1.type, _1],ConsPrices[CabinPrice[CA.type, S1.type, _3], ConsPrices[CabinPrice[CA.type, M1.type, _2], NoPrices.type]]]]
  type ratesM = ConsRates[Rate[M2.type, Military.type], ConsRates[Rate[M1.type, Military.type], NoRates.type]]
  type rates2M = ConsRates[Rate[S1.type, Senior.type], ConsRates[Rate[M2.type, Military.type], ConsRates[Rate[M1.type, Military.type], NoRates.type]]]
  type Prs3M = ConsPrices[CabinPrice[CA.type, M1.type, _2], //CabinPrice("CA", "M1", 200.00)
    ConsPrices[CabinPrice[CA.type, S1.type, _3], //CabinPrice("CA", "S1", 225.00)
      NoPrices.type]]
  type Rts3M = ConsRates[Rate[M1.type, Military.type],
    ConsRates[Rate[S1.type, Senior.type],
      NoRates.type]]
  val aM = minCabinPriceFor[CA.type, Military.type, MaximumPossiblePrice, NULLRC.type, pricesM, ratesM]
  val bM = minCabinPriceFor[CA.type, Military.type, MaximumPossiblePrice, NULLRC.type, prices2M, ratesM]
  val cM = minCabinPriceFor[CA.type, Military.type, MaximumPossiblePrice, NULLRC.type, prices3M, ratesM]
  val dM = minCabinPriceFor[CA.type, Military.type, MaximumPossiblePrice, NULLRC.type, prices4M, rates2M]
  val ddM = minCabinPriceFor[CA.type, Senior.type, MaximumPossiblePrice, NULLRC.type, prices4M, rates2M]
  val dddM = minCabinPriceFor[CA.type, Senior.type, MaximumPossiblePrice, NULLRC.type, Prs3M, Rts3M]
  val ddddM = minCabinPriceFor[CA.type, Military.type, MaximumPossiblePrice, NULLRC.type, Prs3M, Rts3M]
  val dddd2M = minCabinPriceFor[CA.type, Military.type, MaximumPossiblePrice, M1.type, Prs3M, Rts3M]
  val dddddM = minCabinPriceFor[CA.type, NULLRG.type, MaximumPossiblePrice, NULLRC.type, Prs3M, Rts3M]


  type Prs3H = ConsPrices[CabinPrice[CB.type, M1.type, _2], //CabinPrice("CA", "M1", 200.00)
    ConsPrices[CabinPrice[CA.type, S1.type, _3], //CabinPrice("CA", "S1", 225.00)
      NoPrices.type]]
  type Rts3H = ConsRates[Rate[M1.type, Military.type],
    ConsRates[Rate[S1.type, Senior.type],
      NoRates.type]]

  type Prs4H = ConsPrices[CabinPrice[CA.type, M1.type, _2], NoPrices.type]
  type Rts4H = ConsRates[Rate[M1.type, Military.type], NoRates.type]
  val aH = summonHuntSpecific[CA.type, ConsRateGroups[Senior.type, NoRateGroups.type], Prs4H, Rts4H]
  val bH = summonHuntSpecific[CA.type, ConsRateGroups[Senior.type, NoRateGroups.type], Prs3H, Rts3H]
  type Prs5H = ConsPrices[CabinPrice[CB.type, M1.type, _2], ConsPrices[CabinPrice[CA.type, M1.type, _2], NoPrices.type]]
  type Rts5H = ConsRates[Rate[M1.type, Military.type], NoRates.type]
  val aaH = summonHuntSpecific[CA.type, ConsRateGroups[Senior.type, NoRateGroups.type], Prs4H, Rts4H]
  val baH = summonHuntSpecific[CA.type, ConsRateGroups[Military.type, NoRateGroups.type], Prs3H, Rts3H]
  val cbH = summonHuntSpecific[CB.type, ConsRateGroups[Military.type, NoRateGroups.type], Prs3H, Rts3H]

  val testFNF1 = summonFilterNotFound[NoBestPrices.type]
  val testFNF2 = summonFilterNotFound[ConsBestPrices[BestGroupPrice[CabinCode.CA.type, RateCode.S1.type, _3, RateGroup.Senior.type], NoBestPrices.type]]

}
