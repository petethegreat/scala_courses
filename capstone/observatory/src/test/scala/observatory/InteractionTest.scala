package observatory

import scala.collection.concurrent.TrieMap
import org.junit.Assert._
import org.junit.Test

trait InteractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("interactive visualization", 3) _

  @Test def `interaction: check GetSubTileLocations` = {
    val startTile = Tile(0,0,0)
    val (indices,tiles) = Interaction.GetSubTileLocations(startTile)
    tiles.take(10).foreach(println)
  }

  @Test def `interaction: check tileLocation` = {
    val startTiles = List(Tile(0,0,2), Tile(0,1,2), Tile(1,0,2), Tile(1,1,2))
    val locs = startTiles.map(x => (x,Interaction.tileLocation(x)))
    locs.foreach(println)

//    locations are in radians
  }

}
