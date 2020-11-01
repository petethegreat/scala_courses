package observatory
import Visualization.predictTemperature
import scala.collection

/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
    val (gridLonMin, gridLonMax) = (-180, 179)
    val (gridLatMin, gridLatMax) = (-89, 90)
    val debug = true
    val DEFAULT_TEMPERATURE: Temperature = -999



// dunno, this might be handy
  def locationToGridLocation(loc:Location): GridLocation = {
    val glat:Int = loc.lat.round.toInt
    val glon:Int = loc.lon.round.toInt
    GridLocation(glat, glon)
  }


  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    if (debug )  println("generating grid")


    val gridLocs = for (
      glat <- gridLatMin to gridLatMax;
      glon <- gridLonMin to gridLonMax
    ) yield GridLocation(glat, glon)

    val tempCache = gridLocs.map(
      x => x -> predictTemperature(temperatures, Location(x.lat.toFloat,x.lon.toFloat))).toMap

    def lookup(gl:GridLocation): Temperature = tempCache.getOrElse(gl,DEFAULT_TEMPERATURE)
    return lookup

  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {

    val Ntempss = temperaturess.size.toFloat

    val gridLocs = for (
      glat <- gridLatMin to gridLatMax;
      glon <- gridLonMin to gridLonMax
    ) yield GridLocation(glat, glon)

    val averages = collection.mutable.Map[GridLocation,Temperature]() ++ gridLocs.map(x => x -> 0.0).toMap

    for (
      temps <- temperaturess;
      looker = makeGrid(temps);
      gl <- gridLocs
    ) { averages(gl) += looker(gl)/Ntempss}

    def averagelookup(gl: GridLocation) = averages.getOrElse(gl,DEFAULT_TEMPERATURE)
    return averagelookup
  }


  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {

    val currentTemps = makeGrid(temperatures)
    
    def lookup(gl: GridLocation) = currentTemps(gl) - normals(gl)
    return lookup

  }


}
