import DrivableEvidence._

object App {

  def run[T](init : () => T, count : Int)(implicit drivableEvidence: DrivableEvidence[T]) = {
    (1 to count)
      .map(_ => init())
      .map(_.runToCompletion())
      .groupBy(_.countOfRolls())
      .mapValues(_.size)
  }

  def main(args: Array[String]): Unit = {

    val totalRuns = 1000000
    val snowmanResults = run(Snowman.empty, totalRuns)
    val beetleResults = run(Beetle.init, totalRuns)

    val max : Int = math.max(
      snowmanResults.maxBy(_._1)._1,
      beetleResults.maxBy(_._1)._1)

    for (idx <- 1 to max)
      println(s"$idx, ${snowmanResults.getOrElse(idx, 0)}, ${beetleResults.getOrElse(idx, 0)}")
  }
}
