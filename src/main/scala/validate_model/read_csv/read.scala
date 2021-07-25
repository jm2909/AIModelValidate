package validate_model.read_csv

object read {

  def readData(fileName: String): Vector[Array[String]] = {
    val line = scala.io.Source.fromFile(fileName).getLines().toVector
    val values = line.map(_.split(" "))
    values
  }


}
