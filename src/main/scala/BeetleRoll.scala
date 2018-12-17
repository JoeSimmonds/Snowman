sealed trait BeetleRoll

object BeetleRoll {
  case object Eye extends BeetleRoll
  case object Feeler extends BeetleRoll
  case object Leg extends BeetleRoll
  case object Wing extends BeetleRoll
  case object Head extends BeetleRoll
  case object Body extends BeetleRoll

  implicit def asRoll(i : Int) : BeetleRoll = {
    i match {
        case 1 => Eye
        case 2 => Feeler
        case 3 => Leg
        case 4 => Wing
        case 5 => Head
        case 6 => Body
    }
  }
}
