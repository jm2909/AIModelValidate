package validate_model.regression_test

object metrics {

  def rmse(args: Vector[Array[String]]): Double = {

    val sums = args.drop(1).map(i => (i(0).toString.split(",")(0).toFloat - i(0).toString.split(",")(1).toFloat)
    ).map(x => x * x).reduce(_ + _)

    val rmse = Math.sqrt(sums / args.size -1)
    rmse
  }
  def mape(args: Vector[Array[String]]): Double = {
    1.2
  }
  def mad(args: Vector[Array[String]]): Double = {

    val sums = args.drop(1).map(i => (i(0).toString.split(",")(0).toFloat - i(0).toString.split(",")(1).toFloat)
    ).map(x=> Math.abs(x)).reduce(_ + _)
    //val mad = sums / (args.drop(1).size.toFloat)
    sums
  }

}
