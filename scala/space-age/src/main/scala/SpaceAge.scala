import scala.math.BigDecimal

object SpaceAge {
  val SECONDS_ON_EARTH = 31557600
  val MERCURY_RATIO = 0.2408467
  val VENUS_RATIO = 0.61519726
  val MARS_RATIO = 1.8808158
  val JUPITER_RATIO = 11.862615
  val SATURN_RATIO = 29.447498
  val URANUS_RATIO = 84.016846
  val NEPTUNE_RATIO = 164.79132

  case class Age(seconds: Long, onEarth: Double, onMercury: Double,
    onVenus: Double, onMars: Double, onJupiter: Double, onSaturn: Double,
    onUranus: Double, onNeptune: Double)

  def apply(sec: Long): Age = {
    val onEarth = BigDecimal(sec.toDouble / SECONDS_ON_EARTH)

    Age(
      seconds = sec,
      onEarth = toDouble(onEarth),
      onMercury = toDouble(onEarth / MERCURY_RATIO),
      onVenus = toDouble(onEarth / VENUS_RATIO),
      onMars = toDouble(onEarth / MARS_RATIO),
      onJupiter = toDouble(onEarth / JUPITER_RATIO),
      onSaturn = toDouble(onEarth / SATURN_RATIO),
      onUranus = toDouble(onEarth / URANUS_RATIO),
      onNeptune = toDouble(onEarth / NEPTUNE_RATIO)
    )
  }

  private def toDouble(dec: BigDecimal): Double =
    dec.setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
}
