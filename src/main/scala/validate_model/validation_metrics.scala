package validate_model
import regression_test.metrics._
import read_csv.read._
import conditions._
import classification_test.classification_test._

object validation_metrics {

  def main(args: Array[String]) : Unit={
    val pt = readData("testdata/test_class.csv")
    var test_stat = Map[String, Double]()
    val tag =  pt.drop(1).map(x=> x(0).split(",")(0)).distinct
    val status = check(tag)
    if (status == "Regression Problem"){
      val mad2 = mad(pt)
      val rmse2 = rmse(pt)
      // Move to a new location
      test_stat = test_stat.updated("RMSE",Math.floor(rmse2))
      test_stat = test_stat.updated("MAD",Math.floor(mad2))
    } else {
      val class_res = get_class_test_res(pt)
      class_res.map(println)
      test_stat = test_stat.updated("Precision",class_res(0))
      test_stat = test_stat.updated("Recall",class_res(1))
      test_stat = test_stat.updated("FScore",class_res(2))
    }
    print(test_stat)

  }

}
