package validate_model.classification_test

object classification_test  {

  def get_class_test_res(file: Vector[Array[String]]) : Array[Float] = {
    val outarray = file.drop(1).map(x=> Array( x(0).split(",")(0), x(0).split(
      ",")(1).toString).map(x=> getres(x)))
    val true_positive_index = outarray.map(x=> checks(x,1,1))
    val total_true_positive = true_positive_index.reduce(_+_)

    val true_negative_index = outarray.map(x=> checks(x,0,0))
    val total_true_negative = true_negative_index.reduce(_+_)

    val false_negative_index = outarray.map(x=> checks(x,1,0))
    val total_false_negative = false_negative_index.reduce(_+_)

    val false_positive_index = outarray.map(x=> checks(x,0,1))
    val total_false_positive = false_positive_index.reduce(_+_)

    val precison_rate = precision(total_true_positive,total_false_positive).toFloat
    val recall_rate = recall(total_true_positive,total_false_negative).toFloat
    val fscore = f1score(precison_rate,recall_rate).toFloat
    Array(precison_rate,recall_rate,fscore)


  }
  def getres(value: String, threshold: Double = 0.5):Int={
    val vars = value.toFloat
    if(vars > threshold){1} else {0}
  }
  def checks(args: Array[Int],actual:Int,Predicted: Int):Int= if(
    args(0) == actual & args(1) == Predicted){1}else {0}
  def checks(args: Array[Int],actual:Int):Int= if(args(0) == actual){1}else {0}

  def precision(total_true_pos: Int,total_false_positive : Int): Float = {
    100*total_true_pos/(total_false_positive + total_true_pos)
  }
  def recall(total_true_pos: Int,total_false_negative : Int): Float = {
    100*total_true_pos/(total_false_negative + total_true_pos)
  }
  def f1score(precision_val:Double, Recall : Double): Double = {
    2*precision_val*Recall/(precision_val+Recall)
  }
}
