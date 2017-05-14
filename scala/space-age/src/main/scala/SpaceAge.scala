case class SpaceAge (val seconds: Long) {
  val SECONDS_ON_EARTH = 31557600
  val MERCURY_RATIO = 0.2408467
  val VENUS_RATIO = 0.61519726
  val MARS_RATIO = 1.8808158
  val JUPITER_RATIO = 11.862615
  val SATURN_RATIO = 29.447498
  val URANUS_RATIO = 84.016846
  val NEPTUNE_RATIO = 164.79132

  lazy val EARTH_AGE = seconds.toDouble / SECONDS_ON_EARTH

  private def round(dec: Double) =
    Math.round(dec * 100).toDouble / 100.0

  def onEarth = round(EARTH_AGE)
  def onMercury = round(EARTH_AGE / MERCURY_RATIO)
  def onVenus = round(EARTH_AGE / VENUS_RATIO)
  def onMars = round(EARTH_AGE / MARS_RATIO)
  def onJupiter = round(EARTH_AGE / JUPITER_RATIO)
  def onSaturn = round(EARTH_AGE / SATURN_RATIO)
  def onUranus = round(EARTH_AGE / URANUS_RATIO)
  def onNeptune = round(EARTH_AGE / NEPTUNE_RATIO)
}
