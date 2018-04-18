# Distributions based on history

This repo is built on top of https://github.com/jliszka/probability-monad.
It allows to create distributions of a random variable based on previous observations
of this variable and an assumed type of distributions (i.e. Normal, etc.).
At the moment, there is only one type of distributions available - Normal.

It's also possible to feed new observations to adjust parameters of a distribution.

# Examples

Here is how you can create Normal Distributions based on previous history
(i.e. mean and variance are computed automatically)

```scala
import prob_predictor.NormalPredictor
import probability_monad.Distribution

val high = Seq(80.1, 79.3, 77.3, 69.3)
val low = Seq(45.1, 43.1, 43.3, 60.4)
val sunrise = Seq(70.2, 69.4, 70.4, 61.4)
val sunset = Seq(77.4, 50.4, 59.4, 66.1)

// Normal distributions per time period based on history
val highD = NormalPredictor.createByPreviousData(high)
val lowD = NormalPredictor.createByPreviousData(low)
val sunriseD = NormalPredictor.createByPreviousData(sunrise)
val sunsetD = NormalPredictor.createByPreviousData(sunset)
```

To get predicted temperature for certain time of the day, just run e.g.
```scala
highD.sample(10)
lowD.sample(10)
```

If you have new observations of a variable you can 'feed' them to make
distribution more precise as if you originally had this measurement in the history.

```scala

val newHighD = highD.feedNext(177.7)
newHighD.sample(10)
```

If you want iterate over sequence of distributions, e.g. 4 morning temperatures, then 4 afternoon temperatures,
it can be done as follows:

```scala
def iterate[A](loop: Int, d: Distribution[A], others: Distribution[A]*): Distribution[List[A]] = {
  others.foldLeft(d.repeat(loop)) {
    case (acc, other) => acc.zipWith(other.repeat(loop)) {
      case x: (List[A], List[A]) => x._1 ::: x._2
    }
  }
}

// Distribution of one-day measurements, that is,
// given we have 4 measurements per time frame,
// these are predicted temperatures for the two following days
// each in its own list
val daysD = iterate(4, lowD, highD, sunsetD, sunriseD)
daysD.sample(2)

// !Notice that we can't get a plain distribution which
// will iterate over different distribution because it's not a random variable as it depends on step

// unpacked list of temperatures for the two following days
val plain = for {
  dayT <- daysD.sample(2)
  t <- dayT
} yield t
```

