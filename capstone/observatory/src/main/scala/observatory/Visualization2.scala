package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */

  val DEFAULT_COLORMAP = Seq(
    (7.0,  Color(0,0,0)),
    (4.0,  Color(255,0,0)),
    (2.0,  Color(255,255,0)),
    (0.0,  Color(255,255,255)),
    (-2.0, Color(0,255,255)),
    (-7.0, Color(0,0,255)),
  )

  val NPIX = 256
  val SCALE = 1



  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    d00*(1.0 - point.x)*(1.0 - point.y) +
      d01*(1.0 - point.x )*point.y +
      d10*point.x*(1.0 - point.y) +
      d11*point.x*point.y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {
    ???
  }

 //  def gridTile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
 //
 // // get the locations in the tile (Scale)
 // // map the location to 4 gridpoints, and the CellPoint
 // // interpolate to get the estimated temperature
 // // map temperature to color
 //
 //
 //    val (st_indices, st_locs) = GetSubTileLocations(tile,SCALE)
 //
 //    // this should be done in parallel
 //    val indexed_colours = st_indices.zip(st_locs).par.map {
 //      case (indices, loc) => (indices, interpolateColor(colors,predictTemperature(temperatures, loc)) )}
 //
 //    // output
 //    val alpha = 127
 //    val out = new Array[Pixel](Npix*Npix)
 //    // add a little loop here, or define a function to write adjacent pixels if scalefac > 1
 //    for (
 //      (indices, cc) <- indexed_colours;
 //      ii_scale <- 0 until SCALE;
 //      jj_scale <- 0 until SCALE;
 //      ix = indices._1 + ii_scale;
 //      jy = indices._2 + jj_scale;
 //      if (ix < Npix) ;
 //      if (jy < Npix)
 //    ) {out(ix + Npix*jy) = Pixel(cc.red, cc.green, cc.blue, alpha) }
 //    // indexed_colours.foreach {case (ij,cc) => out(ij._1 + Npix*ij._2) = Pixel(cc.red, cc.green, cc.blue, alpha)}
 //    // scalefac
 //    Image(Npix,Npix,out)
 //  }

}
