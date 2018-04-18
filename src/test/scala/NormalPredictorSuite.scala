import FloatOps._
import prob_predictor._
import org.scalacheck.Arbitrary._
import org.scalacheck._
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import probability_monad.Distribution

import scala.math.abs
import scala.util.Random


class NormalPredictorSuite extends FunSuite with PropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 10)

  private val cntSamplesGenerator = Gen.choose[Int](1, 100).suchThat(_ > 0)

  test("constant history createByPreviousData") {
    forAll(cntSamplesGenerator) { cnt =>
      forAll { (x: Double) =>
        NormalPredictor.createByPreviousData(Array.fill(cnt)(x)) ==
          NormalDistribution(
            mu = x,
            sigma = 0,
            cntSamples = cnt
          )
      }
    }
  }

  test("check mean and variance createByPreviousData") {
    forAll(cntSamplesGenerator) { cnt =>
      forAll { (x: Double, y: Double) =>
        val res = NormalPredictor.createByPreviousData(Array.fill(cnt)(x) ++ Array.fill(cnt)(y))
        val trueMean = (x + y) / 2
        val trueSigma = Math.pow((x - y) / 2, 2) / (2 * cnt - 1)
        res match {
          case NormalDistribution(mu, sigma, cntSamples) =>
            mu == trueMean & (sigma ~= trueSigma) & (cntSamples == cnt * 2)
          case _ => false
        }
      }
    }
  }

  test("feed new element is equivalent to feeding whole sequence originally") {
    forAll { (history: List[Double], x: Double) =>
      whenever(history.nonEmpty) {
        NormalPredictor.createByPreviousData(history).feedNext(x) ==
          NormalPredictor.createByPreviousData(x :: history)
      }
    }
  }


}

class NormalDistributionSuite extends FunSuite with PropertyChecks {
  Random.setSeed(1)
  val samples = 30000

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1)


  def assertMean(D: Distribution[Double], mean: Double): Boolean = {
    val res = D.sample(samples).sum / samples
    res ~= mean
  }

  test("Normal Distribution mean and variance") {
    forAll { (mu: Double, sigma: Double) =>
      whenever(sigma >= 0) {
        val D = NormalDistribution(mu = mu, sigma = sigma)
        assertMean(D, mu) & assertMean((D - mu).map(x => x * x), sigma * sigma)
      }
    }
  }

}


object FloatOps {
  private val precisionThreshold = 1e-2
  private val equalPrecisionThreshold = 1e-4

  /** Long floating comparison: assert(double ~= 1.7). */
  implicit class DoubleOps(val self: Double) extends AnyVal {
    def ~=(that: Double): Boolean =
      if (abs(that) <= 1) abs(self - that) < precisionThreshold
      else abs((self / that) - 1) < precisionThreshold
  }

  implicit class DoubleEqualOps(val self: Double) extends AnyVal {
    def ==(that: Double): Boolean =
      if (abs(that) <= 1) abs(self - that) < equalPrecisionThreshold
      else abs((self / that) - 1) < equalPrecisionThreshold
  }

}

