package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import math.{atan,log,exp,Pi,pow}
import Visualization.{interpolateColor, predictTemperature}
/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
    val TILEFUDGE = 0.5 // offset used when going from tile location to lat/lon
  val TWOPI256 = 2.0*Pi/256.0
  val Npix = 256 // number pixels in a tile - fixed, don't play with this


  def tileLocation(tile: Tile): Location = {
    // take tile, and return lat/lon (presumable at centre)
    val lat = 2.0*atan(exp(Pi - (tile.y + TILEFUDGE)*TWOPI256*pow(2.0,-tile.zoom))) - Pi/2.0
    val lon = (tile.x + TILEFUDGE)*TWOPI256*pow(2.0,-tile.zoom) - Pi
    return Location(lat,lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */

  def GetSubTileLocations(tile:Tile, scalefac:Int = 1) = {
//    get the subtile locations
    // divide the given tile into 256 subtiles, by increasing zoom by 8
    // if scalefac is not 1, produce fewer distinct locations

//    val scalefac = 1 // resolution - increase to 2 or 4 to skip colour determination at certain pixels

    val location_indices = (for (ix <- 0 until Npix by scalefac; jy <- 0 until Npix by scalefac) yield (ix,jy))
    val (ix_offset, jy_offset) = (Npix*tile.zoom*tile.x, Npix*tile.zoom*tile.y)

    val dividedTiles = location_indices.map(xy => Tile(xy._1 + ix_offset,xy._2 + jy_offset, tile.zoom +8))
    val locations = dividedTiles.map(tileLocation)
    (location_indices,locations)
  }


  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {

    val scale = 1
    val (st_indices, st_locs) = GetSubTileLocations(tile,scale)

    // this should be done in parallel
    val indexed_colours = st_indices.zip(st_locs).par.map {
      case (indices, loc) => (indices, interpolateColor(colors,predictTemperature(temperatures, loc)) )}

    // output
    val alpha = 127
    val out = new Array[Pixel](Npix*Npix)
    // add a little loop here, or define a function to write adjacent pixels if scalefac > 1
    indexed_colours.foreach {case (ij,cc) => out(ij._1 + Npix*ij._2) = Pixel(cc.red, cc.green, cc.blue, alpha)}
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
    ???
  }

}
