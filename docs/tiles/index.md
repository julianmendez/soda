


## Tiles

**Tiles** is a framework to create formal configurations of constraints.

The fairness tiles are defined in [package tile](https://github.com/julianmendez/soda/tree/master/tiles/src/main/scala/soda/tiles/fairness/tile).

Some of the implemented fairness tiles are:

| Tile                                                 | Class |
|:-----------------------------------------------------|:------|
| all-actor <sub>*(a)*</sub>                           | [AllActorTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/AllActorTile.soda)       |
| <sub>*(a)*</sub> received-&Sigma;-p <sub>*(m)*</sub> | [ReceivedSigmaPTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/ReceivedSigmaPTile.soda) |
| <sub>*(m)*</sub> all-equal <sub>*b*</sub>            | [AllEqualTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/AllEqualTile.soda)       |
| <sub>*(a)*</sub> needed-p <sub>*(m)*</sub>           | [NeededPTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/NeededPTile.soda)        |
| <sub>*(m0, m1)*</sub> at-least <sub>*b*</sub>        | [AtLeastTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/AtLeastTile.soda)        |
| equality <sub>*b*</sub>                              | [EqualityTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/EqualityTile.soda)       |
| equity <sub>*b*</sub>                                | [EquityTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/EquityTile.soda)         |


A specific scenario is given as an example in [ScenarioExample](https://github.com/julianmendez/soda/blob/master/tiles/src/test/scala/soda/tiles/fairness/tile/ScenarioExample.soda).
This scenario is used to test the equality tile with [EqualityTileSpec](https://github.com/julianmendez/soda/blob/master/tiles/src/test/scala/soda/tiles/fairness/tile/EqualityTileSpec.soda) and the equity tile with [EquityTileSpec](https://github.com/julianmendez/soda/blob/master/tiles/src/test/scala/soda/tiles/fairness/tile/EquityTileSpec.soda).


