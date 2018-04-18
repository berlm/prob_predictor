
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck._
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import prob_predictor._
import probability_monad.Distribution

import scala.math.abs
import scala.util.Random

trait CommonSuite {
  val precisionThreshold = 1e-2
  val equalPrecisionThreshold = 1e-4

  val doubleGenerator = Gen.choose[Double](-1E10, 1E10)
  val cntSamplesGenerator = Gen.choose[Int](1, 100).suchThat(_ > 0)
  implicit lazy val arbDouble: Arbitrary[Double] = Arbitrary(doubleGenerator)
}


class NormalPredictorSuite extends FunSuite with Checkers with CommonSuite {

  import DistributionOps._

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 10)


  test("constant history createByPreviousData") {
    def prop(cnt: Int, x: Double): Prop = {
      val res = NormalPredictor.createByPreviousData(Array.fill(cnt)(x))
      res ~== NormalDistribution(x, 0.0, cnt)
    }

    check(prop(1, 1.7056235756310583E-4))

    check(forAll(cntSamplesGenerator) { cnt =>
      forAll { (x: Double) =>
        prop(cnt, x)
      }
    })
  }

  test("check mean and variance createByPreviousData") {
    check(forAll(cntSamplesGenerator) { cnt =>
      forAll { (x: Double, y: Double) =>
        val res = NormalPredictor.createByPreviousData(Array.fill(cnt)(x) ++ Array.fill(cnt)(y))
        val trueMean = (x + y) / 2
        val trueSigma = abs(x - y) / 2
        res ~== NormalDistribution(trueMean, trueSigma, cnt * 2)
      }
    })
  }


  test("feed new element is equivalent to feeding whole sequence originally") {
    def prop(history: List[Double], x: Double) = {
      val feeded = NormalPredictor.createByPreviousData(history).feedNext(x)
      val original = NormalPredictor.createByPreviousData(x :: history)
      feeded ~== original
    }

    check({
      forAll { (history: List[Double], x: Double) =>
        history.nonEmpty ==> prop(history, x)
      }

    })
  }


}

class NormalDistributionSuite extends FunSuite with Checkers with CommonSuite {

  import FloatOps._

  Random.setSeed(1)
  val samples = 30000

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1)


  def assertMean(D: Distribution[Double], mean: Double): Prop = {
    val res = D.sample(samples).sum / samples
    (res ~= mean) :| s"$res !~= $mean"
  }

  test("Normal Distribution mean and variance") {
    check(forAll { (mu: Double, sigma: Double) =>
      (sigma >= 0) ==> {
        val D = NormalDistribution(mu = mu, sigma = sigma)
        assertMean(D, mu) && assertMean(D.map(x => x * x), sigma * sigma + mu * mu)
      }
    })
  }


  //  test("Normal Distribution ksTest") {
  //    check(forAll { (mu: Double, sigma: Double) =>
  //      (sigma >= 0) ==> {
  //        val D = NormalDistribution(mu = mu, sigma = sigma)
  //        (Distribution.ksTest(D, Distribution.normal * sigma + (mu - 0.4), samples) > 0.1) :| "for different distributions, value must be > 0.01" &&
  //          (Distribution.ksTest(D, Distribution.normal * sigma + (mu - 0.5), samples) < 0.1) :| "for same distributions, value must be < 0.1"
  //      }
  //    })
  //  }


}


object FloatOps extends CommonSuite {

  /** Long floating comparison: assert(Double ~= 1.7). */
  implicit class DoubleOps(val self: Double) extends AnyVal {
    def ~=(that: Double): Boolean =
      if (abs(that) <= 1) abs(self - that) < precisionThreshold
      else abs((self / that) - 1) < precisionThreshold
  }

  implicit class DoubleEqualOps(val self: Double) extends AnyVal {
    def ~==(that: Double): Boolean =
      if (abs(that) <= 1) abs(self - that) < equalPrecisionThreshold
      else abs((self / that) - 1) < equalPrecisionThreshold
  }

}


object DistributionOps extends CommonSuite {

  import FloatOps._

  implicit class DistributionEqualOps(val self: Distribution[Double]) extends AnyVal {
    def ~==(that: NormalDistribution): Prop =
      self match {
        case NormalDistribution(mu, sigma, cntSamples) =>
          (cntSamples == that.cntSamples) :| s"$cntSamples == ${that.cntSamples}" &&
            (mu ~== that.mu) :| s"$mu ~== ${that.mu}" &&
            (sigma >= 0.0) :| s"$sigma >= 0.0"
          (sigma ~== that.sigma) :| s"$sigma ~== ${that.sigma}"
        case _ => false :| s"doesn't match"
      }
  }

}
