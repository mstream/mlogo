module Examples (Example(..), examplesByTitle) where

import Prelude

import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Symbol (class IsSymbol, reflectSymbol)
import Heterogeneous.Folding (class FoldingWithIndex)
import Heterogeneous.Folding as Heterogeneous
import MLogo.Parsing (Expression(..))
import Type.Proxy (Proxy)

newtype Example = Example
  { ast ∷ Array Expression
  , source ∷ String
  }

data ToMap = ToMap

instance
  ( IsSymbol sym
  ) ⇒
  FoldingWithIndex
    ToMap
    (Proxy sym)
    (Map String Example)
    Example
    (Map String Example) where
  foldingWithIndex ToMap prop acc val =
    Map.insert (reflectSymbol prop) val acc

examplesByTitle ∷ Map String Example
examplesByTitle =
  Heterogeneous.hfoldlWithIndex
    ToMap
    (Map.empty ∷ Map String Example)
    { "Bullring": Example
        { ast:
            [ ForBlock
                { binder: "i"
                , initialValue: 0
                , step: 1
                , terminalValue: 1002
                }
                ( List.fromFoldable
                    [ ProcedureCall "fd"
                        (List.fromFoldable [ IntegerLiteral 8 ])
                    , ProcedureCall "seth"
                        ( List.fromFoldable
                            [ SubExpression $ Division
                                ( Multiplication
                                    (IntegerLiteral 360)
                                    ( SubExpression $ ProcedureCall
                                        "power"
                                        ( List.fromFoldable
                                            [ ValueReference "i"
                                            , IntegerLiteral 3
                                            ]
                                        )
                                    )
                                )
                                (IntegerLiteral 1002)
                            ]
                        )
                    ]
                )
            ]
        , source:
            "for [i 0 1002] [fd 8 seth (360 * (power :i 3) / 1002)]"
        }
    , "Dahlia, by David Eisenstat, U.S. (14 words)": Example
        { ast:
            [ RepeatBlock
                (IntegerLiteral 8)
                ( List.fromFoldable
                    [ ProcedureCall
                        "rt"
                        (List.fromFoldable [ IntegerLiteral 45 ])
                    , RepeatBlock
                        (IntegerLiteral 6)
                        ( List.fromFoldable
                            [ RepeatBlock
                                (IntegerLiteral 90)
                                ( List.fromFoldable
                                    [ ProcedureCall
                                        "fd"
                                        ( List.fromFoldable
                                            [ IntegerLiteral 2 ]
                                        )
                                    , ProcedureCall
                                        "rt"
                                        ( List.fromFoldable
                                            [ IntegerLiteral 2 ]
                                        )
                                    ]
                                )
                            , ProcedureCall
                                "rt"
                                ( List.fromFoldable
                                    [ IntegerLiteral 90 ]
                                )
                            ]
                        )
                    ]
                )
            ]
        , source:
            "repeat 8 [rt 45 repeat 6 [repeat 90 [fd 2 rt 2] rt 90]]"
        }
    , "Growing Scrolls variation 4, by M.H. Elhefni, Egypt (13 words)":
        Example
          { ast:
              [ ForBlock
                  { binder: "i"
                  , initialValue: 1
                  , step: 2
                  , terminalValue: 18
                  }
                  ( List.fromFoldable
                      [ RepeatBlock
                          (IntegerLiteral 720)
                          ( List.fromFoldable
                              [ ProcedureCall
                                  "fd"
                                  ( List.fromFoldable
                                      [ ValueReference "i" ]
                                  )
                              , ProcedureCall
                                  "rt"
                                  ( List.fromFoldable
                                      [ ProcedureCall "repcount" Nil ]
                                  )
                              ]
                          )
                      , ProcedureCall
                          "lt"
                          (List.fromFoldable [ IntegerLiteral 45 ])
                      ]
                  )
              ]
          , source:
              "for [i 1 18 2] [repeat 720 [fd :i rt repcount] lt 45]"
          }
    , "Hairy Star": Example
        { ast:
            [ ForBlock
                { binder: "i"
                , initialValue: 0
                , step: 1
                , terminalValue: 4700
                }
                ( List.fromFoldable
                    [ ProcedureCall
                        "fd"
                        (List.fromFoldable [ IntegerLiteral 10 ])
                    , ProcedureCall
                        "rt"
                        ( List.fromFoldable
                            [ SubExpression $ Multiplication
                                (IntegerLiteral 180)
                                ( ProcedureCall
                                    "sin"
                                    ( List.fromFoldable
                                        [ SubExpression $ Multiplication
                                            (ValueReference "i")
                                            (ValueReference "i")
                                        ]
                                    )
                                )
                            ]
                        )
                    ]
                )
            ]
        , source: "for [i 0 4700] [fd 10 rt (180 * sin (:i * :i))]"
        }
    , "Jaggy Star": Example
        { ast:
            [ ForBlock
                { binder: "i"
                , initialValue: 0
                , step: 1
                , terminalValue: 2200
                }
                ( List.fromFoldable
                    [ ProcedureCall
                        "fd"
                        ( List.fromFoldable
                            [ SubExpression $ Multiplication
                                (IntegerLiteral 25)
                                ( ProcedureCall "sin"
                                    ( List.fromFoldable
                                        [ ValueReference "i" ]
                                    )
                                )
                            ]
                        )
                    , ProcedureCall
                        "rt"
                        ( List.fromFoldable
                            [ SubExpression $ Multiplication
                                (ValueReference "i")
                                (ValueReference "i")
                            ]
                        )
                    ]
                )
            ]
        , source: "for [i 0 2200] [fd (25 * sin :i) rt (:i * :i)]"
        }
    , "Layers, by Alessio Plebe, Italy (15 words)": Example
        { ast:
            [ ForBlock
                { binder: "i"
                , initialValue: 0
                , step: 1
                , terminalValue: 420
                }
                ( List.fromFoldable
                    [ ProcedureCall "seth"
                        (List.fromFoldable [ ValueReference "i" ])
                    , RepeatBlock
                        (ValueReference "i")
                        ( List.fromFoldable
                            [ ProcedureCall
                                "fd"
                                (List.fromFoldable [ IntegerLiteral 2 ])
                            , ProcedureCall
                                "rt"
                                ( List.fromFoldable
                                    [ IntegerLiteral 1 ]
                                )
                            ]
                        )
                    , ProcedureCall "pu" Nil
                    , ProcedureCall "home" Nil
                    , ProcedureCall "pd" Nil
                    ]
                )
            ]
        , source:
            "for [i 0 420] [seth :i repeat :i [fd 2 rt 1] pu home pd]"
        }
    , "Octa-star Spiral, by M.H. Elhefni, Egypt": Example
        { ast:
            [ ForBlock
                { binder: "l"
                , initialValue: 0
                , step: 4
                , terminalValue: 120
                }
                ( List.fromFoldable
                    [ RepeatBlock
                        (IntegerLiteral 8)
                        ( List.fromFoldable
                            [ ProcedureCall
                                "fd"
                                ( List.fromFoldable
                                    [ ValueReference "l" ]
                                )
                            , ProcedureCall
                                "rt"
                                ( List.fromFoldable
                                    [ IntegerLiteral 135 ]
                                )
                            ]
                        )
                    , ProcedureCall
                        "fd"
                        (List.fromFoldable [ ValueReference "l" ])
                    , ProcedureCall
                        "rt"
                        (List.fromFoldable [ IntegerLiteral 30 ])
                    ]
                )
            ]
        , source:
            "for [l 0 120 4] [repeat 8 [fd :l rt 135] fd :l rt 30]"
        }
    , "Penta-octagon, by M.H. Elhefni, Egypt (15 words)": Example
        { ast:
            [ ForBlock
                { binder: "l"
                , initialValue: 10
                , step: 5
                , terminalValue: 80
                }
                ( List.fromFoldable
                    [ RepeatBlock
                        (IntegerLiteral 5)
                        ( List.fromFoldable
                            [ RepeatBlock (IntegerLiteral 8)
                                ( List.fromFoldable
                                    [ ProcedureCall "fd"
                                        ( List.fromFoldable
                                            [ ValueReference "l" ]
                                        )
                                    , ProcedureCall "rt"
                                        ( List.fromFoldable
                                            [ IntegerLiteral 45 ]
                                        )
                                    ]
                                )
                            , ProcedureCall "rt"
                                ( List.fromFoldable
                                    [ IntegerLiteral 72 ]
                                )
                            ]
                        )
                    ]
                )
            ]
        , source:
            "for [l 10 80 5] [repeat 5 [repeat 8 [fd :l rt 45] rt 72]]"
        }
    , "Slalom Scrolls": Example
        { ast:
            [ ForBlock
                { binder: "i"
                , initialValue: 0
                , step: 1
                , terminalValue: 2000
                }
                ( List.fromFoldable
                    [ ProcedureCall "fd"
                        (List.fromFoldable [ IntegerLiteral 5 ])
                    , ProcedureCall "rt"
                        ( List.fromFoldable
                            [ SubExpression $ Multiplication
                                (IntegerLiteral 90)
                                ( ProcedureCall "sin"
                                    ( List.fromFoldable
                                        [ ValueReference "i" ]
                                    )
                                )
                            ]
                        )
                    ]
                )
            ]
        , source: "for [i 0 2000] [fd 5 rt (90 * sin :i)]"
        }
    }

