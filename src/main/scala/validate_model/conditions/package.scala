package validate_model

package object conditions {

  def check(tag:Vector[String]):String = {
    val ret = tag.size match{
        case 2 => "Classification Problem"
        case _ => "Regression Problem"
    }
    ret
  }

}
