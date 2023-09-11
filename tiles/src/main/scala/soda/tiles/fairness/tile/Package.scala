package soda.tiles.fairness.tile

/*
 * This package contains classes to model the tiles.
 */

import   soda.tiles.fairness.tool.Actor
import   soda.tiles.fairness.tool.Assignment
import   soda.tiles.fairness.tool.Measure
import   soda.tiles.fairness.tool.MeasureZero_
import   soda.tiles.fairness.tool.Outcome
import   soda.tiles.fairness.tool.Resource
import   soda.tiles.fairness.tool.TileMessage
import   soda.tiles.fairness.tool.TileMessageBuilder_
import   soda.tiles.fairness.tool.TilePair
import   soda.tiles.fairness.tool.TilePair_

trait Package

trait AllActorPairTile
{

  def apply (message : TileMessage [Boolean] ) : TileMessage [Seq [TilePair [Actor, Actor] ] ] =
    TileMessageBuilder_ () .build (message .context) (message .outcome) (
      ( (message .outcome) .assignments)
        .map ( assignment => assignment .actor)
        .distinct
        .sorted
        .map ( actor => TilePair_ (actor , actor) )
    )

}

case class AllActorPairTile_ () extends AllActorPairTile


trait AllActorTile
{

  def apply (message : TileMessage [Boolean] ) : TileMessage [Seq [Actor] ] =
    TileMessageBuilder_ () .build (message .context) (message .outcome) (
      ( (message .outcome) .assignments)
        .map ( assignment => assignment .actor)
        .distinct
        .sorted
    )

}

case class AllActorTile_ () extends AllActorTile


trait AllEqual1Tile
{

  def apply (message : TileMessage [Seq [Measure] ] ) (list : Seq [Measure] ) : Boolean =
    list match  {
      case Nil => true
      case (x :: xs) => xs .forall ( e => x == e)
    }

}

case class AllEqual1Tile_ () extends AllEqual1Tile


trait AllEqualTile
{

  lazy val all_equal_1_tile = AllEqual1Tile_ ()

  def apply (message : TileMessage [Seq [Measure] ] ) : TileMessage [Boolean] =
    TileMessageBuilder_ () .build (message .context) (message .outcome) (
      all_equal_1_tile .apply (message) (message .contents)
    )

}

case class AllEqualTile_ () extends AllEqualTile


trait AtLeastTile
{

  def apply (message : TileMessage [Seq [TilePair [Measure, Measure] ] ] )
    : TileMessage [Boolean] =
    TileMessageBuilder_ () .build (message .context) (message .outcome) (
      ( (message .contents)
        .map ( pair => (pair .fst .compareTo (pair .snd) >= 0 ) )
        .forall ( e => e)
      )
    )

}

case class AtLeastTile_ () extends AtLeastTile


trait AttributePTile
{

  def   p : Actor => Measure

  def apply (message : TileMessage [Seq [Actor] ] ) : TileMessage [Seq [Measure] ] =
    TileMessageBuilder_ () .build (message .context) (message .outcome) (
      ( (message .contents)
        .map ( actor => p (actor) ) )
    )

}

case class AttributePTile_ (p : Actor => Measure) extends AttributePTile


trait EqualityTile
{

  def   sigma : Measure => Measure => Measure
  def   p_utility : Resource => Measure

  lazy val all_equal_tile = AllEqualTile_ ()

  lazy val received_sigma_p_tile = ReceivedSigmaPTile_ (sigma , p_utility)

  lazy val all_actor_tile = AllActorTile_ ()

  def apply (message : TileMessage [Boolean] ) : TileMessage [Boolean] =
    all_equal_tile .apply (
      received_sigma_p_tile .apply (
        all_actor_tile .apply (message)
      )
    )

}

case class EqualityTile_ (sigma : Measure => Measure => Measure, p_utility : Resource => Measure) extends EqualityTile


trait EquityTile
{

  def   sigma : Measure => Measure => Measure
  def   p0_need : Actor => Measure
  def   p1_utility : Resource => Measure

  lazy val at_least_tile = AtLeastTile_ ()

  lazy val received_sigma_p_tile = ReceivedSigmaPTile_ (sigma , p1_utility)

  lazy val needed_p_tile = NeededPTile_ (p0_need)

  lazy val all_actor_pair_tile = AllActorPairTile_ ()

  lazy val unzip_fst_tile = UnzipPairFstTile_ ()

  lazy val unzip_snd_tile = UnzipPairSndTile_ ()

  lazy val zip_tile = ZipTile_ ()

  def get_branch_0 (message : TileMessage [Seq [TilePair [Actor, Actor] ] ] )
      : TileMessage [Seq [Measure] ] =
    received_sigma_p_tile .apply (unzip_fst_tile .apply (message) )

  def get_branch_1 (message : TileMessage [Seq [TilePair [Actor, Actor] ] ] )
      : TileMessage [Seq [Measure] ] =
    needed_p_tile .apply (unzip_snd_tile .apply (message) )

  def zip_branches (message : TileMessage [Seq [TilePair [Actor, Actor] ] ] )
      : TileMessage [Seq [TilePair [Measure, Measure] ] ] =
    zip_tile .apply (get_branch_0 (message) ) (get_branch_1 (message) )

  def apply (message : TileMessage [Boolean] ) : TileMessage [Boolean] =
    at_least_tile .apply (
      zip_branches (
        all_actor_pair_tile .apply (message)
      )
    )

}

case class EquityTile_ (sigma : Measure => Measure => Measure, p0_need : Actor => Measure, p1_utility : Resource => Measure) extends EquityTile


trait NeededPTile
{

  def   p : Actor => Measure

  def apply (message : TileMessage [Seq [Actor] ] ) : TileMessage [Seq [Measure] ] =
    AttributePTile_ (p) .apply (message)

}

case class NeededPTile_ (p : Actor => Measure) extends NeededPTile


trait ReceivedSigmaPTile
{

  def   sigma : Measure => Measure => Measure
  def   p : Resource => Measure

  private def _sigma2 (m0 : Measure , m1 : Measure) : Measure =
    sigma (m0) (m1)

  private lazy val _measure_zero : Measure = MeasureZero_ ()

  def get_assignment (assignments : Seq [Assignment] ) (a : Actor) : Option [Assignment] =
    assignments . find ( assignment => (assignment .actor) == a)

  def get_measure (outcome : Outcome) (a : Actor) : Measure =
    (get_assignment (outcome .assignments) (a) )
      .map ( assignment => p (assignment .resource) )
      .foldLeft (_measure_zero) (_sigma2)

  def apply (message : TileMessage [Seq [Actor] ] ) : TileMessage [Seq [Measure] ] =
    TileMessageBuilder_ () .build (message .context) (message .outcome) (
      ( (message .contents)
        .map ( actor => get_measure (message .outcome) (actor) ) )
    )

}

case class ReceivedSigmaPTile_ (sigma : Measure => Measure => Measure, p : Resource => Measure) extends ReceivedSigmaPTile


trait UnzipPairFstTile
{

  def unzip_fst_list [A , B ] (list : Seq [TilePair [A, B] ] ) : Seq [A] =
    list .map ( pair => pair .fst)

  def apply [A , B ] (message : TileMessage [Seq [TilePair [A, B] ] ] )
      : TileMessage [Seq [A] ] =
    TileMessageBuilder_ () .build (message .context) (message .outcome) (
      unzip_fst_list (message .contents)
    )

}

case class UnzipPairFstTile_ () extends UnzipPairFstTile

trait UnzipPairSndTile
{

  def unzip_snd_list [A , B ] (list : Seq [TilePair [A, B] ] ) : Seq [B] =
    list .map ( pair => pair .snd)

  def apply [A , B ] (message : TileMessage [Seq [TilePair [A, B] ] ] )
      : TileMessage [Seq [B] ] =
    TileMessageBuilder_ () .build (message .context) (message .outcome) (
      unzip_snd_list (message .contents)
    )

}

case class UnzipPairSndTile_ () extends UnzipPairSndTile


trait ZipTile
{

  def zip_lists [A , B ] (list0 : Seq [A] ) (list1 : Seq [B] )
      : Seq [TilePair [A, B] ] =
    Tuple2 (list0 , list1) match  {
      case Tuple2 (Nil , x) => Nil
      case Tuple2 ( (x :: xs) , Nil) => Nil
      case Tuple2 ( (x :: xs) , (y :: ys) ) =>
        Seq (TilePair_ (x, y) ) .++ (zip_lists (xs) (ys) )
    }

  def apply [A , B ] (message0 : TileMessage [Seq [A] ] )
      (message1 : TileMessage [Seq [B] ] ) : TileMessage [Seq [TilePair [A, B] ] ] =
    TileMessageBuilder_ () .build (message0 .context) (message0 .outcome) (
      zip_lists (message0 .contents) (message1 .contents)
    )

}

case class ZipTile_ () extends ZipTile

