import scala.util.Random

sealed trait SnowmanRoll

object SnowmanRoll {
  case object CarrotNose extends SnowmanRoll
  case object Arm extends SnowmanRoll
  case object ButtonOrEye extends SnowmanRoll
  case object Hat extends SnowmanRoll
  case object Head extends SnowmanRoll
  case object Body extends SnowmanRoll

  def random() = Random.nextInt(6) match {
      case 0 => CarrotNose
      case 1 => Arm
      case 2 => ButtonOrEye
      case 3 => Hat
      case 4 => Head
      case 5 => Body
    }
}