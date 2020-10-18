package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import math.{atan,log, exp, Pi, pow, sinh}
import Visualization.{interpolateColor, predictTemperature,getDefaultColours}
/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
    val TILEFUDGE = 0.0 // offset used when going from tile location to lat/lon
  val TWOPI256 = 2.0*Pi/256.0
  val Npix = 256 // number pixels in a tile - fixed, don't play with this
  val (zMin, zMax) = (2,2) // z values to use in generateTiles
  val (yearMin, yearMax) = (2015, 2015)
  val SCALE = 2 // scale factor for images



  def tileLocation(tile: Tile): Location = {
    // take tile, and return lat/lon (presumable at centre)
    val nn = pow(2.0,tile.zoom)
//    val lat = 2.0*atan(exp(Pi - (tile.y + TILEFUDGE)*TWOPI256*pow(2.0,-tile.zoom))) - Pi/2.0
//    val lon = (tile.x + TILEFUDGE)*TWOPI256*pow(2.0,-tile.zoom) - Pi
    val lat = atan(sinh(Pi - 2.0*Pi/nn*tile.y))
    val lon = Pi*(2.0*tile.x/nn - 1.0)
    return Location(lat.toDegrees,lon.toDegrees)
  }
//  https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Subtiles
  
  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def GetSubTiles(tile: Tile, scalefac:Int = 1) = {
    // divide a tile by increasing zoom by 8, return the subtiles (256 by 256)
    val location_indices = (for (jy <- 0 until 256 by scalefac; ix <- 0 until 256 by scalefac) yield (ix,jy))
    val (ix_offset, jy_offset) = (256*tile.zoom*tile.x, 256*tile.zoom*tile.y)

    val dividedTiles = location_indices.map(xy => Tile(xy._1 + ix_offset,xy._2 + jy_offset, tile.zoom +8))
    (location_indices, dividedTiles)

  }
  def GetSubTileLocations(tile:Tile, scalefac:Int = 1) = {
//    get the subtile locations
    // divide the given tile into 256 subtiles, by increasing zoom by 8
    // if scalefac is not 1, produce fewer distinct locations

//    val scalefac = 1 // resolution - increase to 2 or 4 to skip colour determination at certain pixels
//    zoom level of 8 and npix of 256 are fixed here

    val (location_indices, dividedTiles) = GetSubTiles(tile, scalefac)
    val locations = dividedTiles.map(tileLocation(_))
    (location_indices,locations)
  }


  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {

    val (st_indices, st_locs) = GetSubTileLocations(tile,SCALE)

    // this should be done in parallel
    val indexed_colours = st_indices.zip(st_locs).par.map {
      case (indices, loc) => (indices, interpolateColor(colors,predictTemperature(temperatures, loc)) )}

    // output
    val alpha = 127
    val out = new Array[Pixel](Npix*Npix)
    // add a little loop here, or define a function to write adjacent pixels if scalefac > 1
    for (
      (indices, cc) <- indexed_colours;
      ii_scale <- 0 until SCALE;
      jj_scale <- 0 until SCALE;
      ix = indices._1 + ii_scale;
      jy = indices._2 + jj_scale;
      if (ix < Npix) ;
      if (jy < Npix)
    ) {out(ix + Npix*jy) = Pixel(cc.red, cc.green, cc.blue, alpha) }
    // indexed_colours.foreach {case (ij,cc) => out(ij._1 + Npix*ij._2) = Pixel(cc.red, cc.green, cc.blue, alpha)}
    // scalefac
    Image(Npix,Npix,out)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    val yearTiles = for {
      z <- zMin to zMax
      ix <- 0 until pow(2,z).toInt
      jy <- 0 until pow(2,z).toInt
      yearData <- yearlyData} yield (yearData,Tile(ix,jy,z))
    yearTiles.map(x => generateImage(x._1._1,x._2,x._1._2))


  }
  def writeImages():Unit = {

    val years = for (y <- yearMin to yearMax) yield y

    println("interaction.writeImages - extracting data")
    val yearTemps = years.map(x => (
      x,
      Extraction.locationYearlyAverageRecords(
        Extraction.locateTemperatures(x, "/stations.csv", s"/${x}.csv"))))
    println(s"interaction.writeImages - writing tile images")
    println(s"z: ${zMin} -${zMax} ")
    println(s"year: ${yearMin} -${yearMax} ")
    val nimages = (zMin to zMax).map(x => math.pow(2,2*x).toInt).sum * years.size
    println(s"${nimages} images")

    def generateTheImage(yy:Year, tt:Tile, dd:Iterable[(Location, Temperature)]) = {
      val colourMap = getDefaultColours()
      val image = tile(dd, colourMap, tt)
      val outPath = s"target/temperatures/${yy}/${tt.zoom}/${tt.x}-${tt.y}.png"
      val outFile = new java.io.File(outPath)
      outFile.getParentFile.mkdirs
      image.output(outFile)
      println(s"wrote ${outPath}")
    }
    // do the writing
    generateTiles(yearTemps, generateTheImage)
  }

  def getData2015(): Unit = {

    val year = 2015
    println(s"extracting location/temperatures for {year}")
    val temps = Extraction.locateTemperatures(year, "/stations.csv", s"/${year}.csv")
    println(s"averaging temperatures for {year}")
    val averages = Extraction.locationYearlyAverageRecords(temps)
    println("Done!")
    averages.take(20).foreach(println)
  }
}
