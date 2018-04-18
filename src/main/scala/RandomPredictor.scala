package prob_predictor

import Math._
import probability_monad.Distribution.normal
import probability_monad._

trait RandomPredictor[A] {
  def createByPreviousData(previous: Seq[A]): Distribution[A]
}

trait Recomputable[A] extends Distribution[A] {
  def feedNext(x: A): Recomputable[A]
}

case class NormalDistribution(mu: Double, sigma: Double, cntSamples: Int = 0) extends Distribution[Double] with Recomputable[Double] {
  def get: Double = {
    val z = normal.get
    z * sigma + mu
  }

  def feedNext(x: Double): NormalDistribution = {
    val newCntSamples = cntSamples + 1
    val newMu = ((mu * cntSamples) + x) / newCntSamples
    val squareSum = pow(sigma, 2) + pow(mu, 2)
    val newSquareSum = squareSum + pow(x, 2)
    val newSigma = (newSquareSum - pow(newMu, 2)) / newCntSamples
    NormalDistribution(mu = newMu, sigma = newSigma, cntSamples = newCntSamples)
  }
}


object NormalPredictor extends RandomPredictor[Double] {
  def createByPreviousData(previous: Seq[Double]): Recomputable[Double] = {
    val length = previous.length
    require(length > 0)
    val mu = previous.sum / length
    // length-1 for unbiased variance (see https://en.wikipedia.org/wiki/Normal_distribution#Sample_variance)
    val sigma = Math.sqrt(previous.map(x => (x - mu) * (x - mu)).sum / Math.max(1, length - 1))
    NormalDistribution(mu = mu, sigma = sigma, cntSamples = length)
  }
}