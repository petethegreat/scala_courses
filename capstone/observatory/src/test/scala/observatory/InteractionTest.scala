package observatory

import scala.collection.concurrent.TrieMap
import org.junit.Assert._
import org.junit.Test

trait InteractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("interactive visualization", 3) _

  @Test def `interaction: check GetSubTileLocations` = {
    val startTile = Tile(0,0,1)
    val otherStarts = List(
      Tile(startTile.x +1, startTile.y, startTile.zoom),
      Tile(startTile.x, startTile.y +1, startTile.zoom),
      Tile(startTile.x +1, startTile.y +1, startTile.zoom)
      ).map(x => (x,Interaction.tileLocation(x)))

    println("other corners:")
    otherStarts.foreach(x => println(s"${x._1} : ${x._2}"))
    val (indices,tiles) = Interaction.GetSubTiles(startTile,1)
    val startloc = Interaction.tileLocation(startTile) // top left
    println(s"startTile = $startTile, startLoc = ${startloc}")
    val cornerTiles = List(
      tiles(0),tiles(255),tiles(256*255),tiles(256*256 -1)
    )
    val cornerLocs = cornerTiles.map(x => (x,Interaction.tileLocation(x)))
    println("corner locations:")
    cornerLocs.foreach(x => println(s"${x._1} : ${x._2}"))

  }

  @Test def `interaction: check tileLocation` = {
    val startTiles = List(Tile(0,0,2), Tile(1,0,2), Tile(2,0,2), Tile(3,0,2),
      Tile(0,0,2), Tile(0,1,2), Tile(0,2,2), Tile(0,3,2), Tile(3,3,2))
    val locs = startTiles.map(x => (x,Interaction.tileLocation(x),Interaction.tileLocation(Interaction.GetSubTiles(x,scalefac=1)._2(0))))
    locs.foreach(println)

//    locations are in radians
  }
  @Test def `interaction: check alttileLocation` = {
    val startTiles = List(Tile(0,0,2), Tile(1,0,2), Tile(2,0,2), Tile(3,0,2),
      Tile(0,0,2), Tile(0,1,2), Tile(0,2,2), Tile(0,3,2), Tile(3,3,2))
    val locs = startTiles.map(x => (x,Interaction.tileLocation(x),Interaction.altTileLocation(Interaction.GetSubTiles(x,scalefac=1)._2(0))))
    locs.foreach(println)

//    locations are in radians
  }

}
