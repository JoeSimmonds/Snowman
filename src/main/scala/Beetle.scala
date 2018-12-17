import scala.util.Random

case class Beetle(body: Boolean, head: Boolean, legs: Int, wings: Int, feelers: Int, eyes: Int, history : Seq[BeetleRoll]) {
  def ++ (roll : BeetleRoll) : Beetle =  {
    ((this, roll) match {
      case (Beetle(_, true, _, _, _, e, _), BeetleRoll.Eye) if e < 2       => copy(eyes = e + 1)
      case (Beetle(_, true, _, _, f, _, _), BeetleRoll.Feeler) if f < 2    => copy(feelers = f + 1)
      case (Beetle(true, _, l, _, _, _, _), BeetleRoll.Leg) if l < 6       => copy(legs = l + 1)
      case (Beetle(true, _, _, w, _, _, _), BeetleRoll.Wing) if w < 2      => copy(wings = w + 1)
      case (Beetle(true, false, _, _, _, _, _), BeetleRoll.Head)           => copy(head = true)
      case (Beetle(false, _, _, _, _, _, _), BeetleRoll.Body)              => copy(body = true)
      case _                                                               => this
    }).copy(history = history :+ roll)
  }

  def isComplete() : Boolean = {
    this match {
      case Beetle(true, true, 6, 2, 2, 2, _) => true
      case _ => false
    }
  }
}

object Beetle {
  def init() = new Beetle(false, false, 0, 0, 0, 0, Seq.empty)

  implicit val beetleIsDrivable : DrivableEvidence[Beetle] = new DrivableEvidence[Beetle] {
    override def isComplete(d: Beetle): Boolean = d.isComplete()
    override def roll(d: Beetle): Beetle = d ++ (Random.nextInt(6) +1)
    override def countOfRolls(d: Beetle): Int = d.history.size
  }
}

