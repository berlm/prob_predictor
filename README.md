# prob_predictor

This repo is built on the top of https://github.com/jliszka/probability-monad.
It allows to create distributions of a random variable based on previous observations
of this variable and an assumed type of distributions (i.e. Normal, etc.).
At the moment, there is only one type of distributions available - Normal.

It's also possible to feed new observations to adjust parameters of a distribution.