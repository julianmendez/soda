
<head>
  <script src="https://cdnjs.cloudflare.com/ajax/libs/mermaid/9.4.3/mermaid.min.js"> </script>
</head>


## Tiles

**Tiles** is a framework to create formal configurations of constraints. Its classes are
written in [Soda](https://julianmendez.github.io/soda) and grouped in packages translated to
[Scala](https://scala-lang.org).

The fairness tiles are defined in
[package tile](
https://github.com/julianmendez/soda/tree/master/tiles/src/main/scala/soda/tiles/fairness/tile
) [(Scala translation)](
https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/Package.scala
) and they use entities and other tools defined in
[package tool](
https://github.com/julianmendez/soda/tree/master/tiles/src/main/scala/soda/tiles/fairness/tool
) [(Scala translation)](
https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tool/Package.scala
).


### Resource Allocation Scenarios

These are some of the implemented fairness tiles for resource allocation scenarios:

| Tile                                                 | Class                                                                                                                                        |
|:-----------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------|
| all-actor <sub>*(a)*</sub>                           | [AllActorTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/AllActorTile.soda)             |
| <sub>*(a)*</sub> received-&Sigma;-p <sub>*(m)*</sub> | [ReceivedSigmaPTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/ReceivedSigmaPTile.soda) |
| <sub>*(m)*</sub> all-equal <sub>*b*</sub>            | [AllEqualTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/AllEqualTile.soda)             |
| <sub>*(a)*</sub> needed-p <sub>*(m)*</sub>           | [NeededPTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/NeededPTile.soda)               |
| <sub>*(m0, m1)*</sub> at-least <sub>*b*</sub>        | [AtLeastTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/AtLeastTile.soda)               |
| equality <sub>*b*</sub>                              | [EqualityTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/EqualityTile.soda)             |
| equity <sub>*b*</sub>                                | [EquityTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/EquityTile.soda)                 |

A specific scenario is given as an example in [ResourceAllocationScenarioExample](https://github.com/julianmendez/soda/blob/master/tiles/src/test/scala/soda/tiles/fairness/tile/ResourceAllocationScenarioExample.soda).
This scenario is used to test the equality tile with [EqualityTileSpec](https://github.com/julianmendez/soda/blob/master/tiles/src/test/scala/soda/tiles/fairness/tile/EqualityTileSpec.soda) and the equity tile with [EquityTileSpec](https://github.com/julianmendez/soda/blob/master/tiles/src/test/scala/soda/tiles/fairness/tile/EquityTileSpec.soda).


#### Equality

```mermaid
graph LR;
all-actor --- received-Σ-p;
received-Σ-p --- all-equal;
```


#### Equity

```mermaid
graph LR;
all-actor --- received-Σ-p;
all-actor --- needed-p;
received-Σ-p --- at-least;
needed-p ---  at-least;
```


### Scoring Scenarios

These are some of the implemented fairness tiles for scoring scenarios:

| Tile                                             | Class                                                                                                                                        |
|:-------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------|
| all-actor <sub>*(a0, a1, a2)*</sub>              | [AllActorTripleTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/AllActorTripleTile.soda) |
| <sub>*(a)*</sub> prediction-p <sub>*(m)*</sub>   | [PredictionPTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/PredictionPTile.soda)       |
| <sub>*(a)*</sub> result-p <sub>*(m)*</sub>       | [ResultPTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/ResultPTile.soda)               |
| <sub>*(m0, m1)*</sub> false-pos <sub>*(m)*</sub> | [FalsePosTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/FalsePosTile.soda)             |
| <sub>*(a)*</sub> with-p <sub>*(m)*</sub>         | [WithPTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/WithPTile.soda)                   |
| <sub>*(m0, m1)*</sub> correlation <sub>*m*</sub> | [CorrelationTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/CorrelationTile.soda)       |
| <sub>*m*</sub> decision <sub>*b*</sub>           | [DecisionTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/DecisionTile.soda)             |
| unbiasedness <sub>*b*</sub>                      | [UnbiasednessTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/UnbiasednessTile.soda)     |

A specific scenario is given as an example in [ScoringScenarioExample](https://github.com/julianmendez/soda/blob/master/tiles/src/test/scala/soda/tiles/fairness/tile/ScoringScenarioExample.soda).
This scenario is used to test the scoring scenario tile (unbiasedness with respect to false positives) with
[UnbiasednessTileSpec](https://github.com/julianmendez/soda/blob/master/tiles/src/test/scala/soda/tiles/fairness/tile/UnbiasednessTileSpec.soda).


#### Unbiasedness

```mermaid
graph LR;
all-actor --- prediction-p;
all-actor --- result-p;
all-actor --- with-p;
prediction-p --- false-pos;
result-p --- false-pos;
with-p --- correlation;
false-pos --- correlation;
correlation --- decision;
```


### Auxiliary Tiles

The auxiliary tiles are used in the construction of other tiles. Some of the auxiliary tiles
are:

| Tile                                                                     | Class                                                                                                                                 |
|:-------------------------------------------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------------|
| <sub>*(a)*</sub> attribute-p <sub>*(m)*</sub>                            | [AttributePTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/AttributePTile.soda)  |
| <sub>*(m0, m1)*</sub> &Sigma;-p <sub>*(m)*</sub>                         | [SigmaTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/FalsePosTile.soda)         |
| <sub>*(&alpha;0) (&alpha;1)*</sub> zip <sub>*(&alpha;0, &alpha;1)*</sub> | [ZipTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/ZipTile.soda)                |
| <sub>*(&alpha;0, &alpha;1)*</sub> unzip-0 <sub>*(&alpha;0)*</sub>        | [UnzipPairFstTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/UnzipPairTile.soda) |
| <sub>*(&alpha;0, &alpha;1)*</sub> unzip-1 <sub>*(&alpha;1)*</sub>        | [UnzipPairSndTile](https://github.com/julianmendez/soda/blob/master/tiles/src/main/scala/soda/tiles/fairness/tile/UnzipPairTile.soda) |

<script>
  window.mermaid.init(undefined, document.querySelectorAll('.language-mermaid'));
</script>


