module Examples (Example(..), examplesByTitle) where

import Prelude

import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Symbol (class IsSymbol, reflectSymbol)
import Heterogeneous.Folding (class FoldingWithIndex)
import Heterogeneous.Folding as Heterogeneous
import MLogo.Parsing.Expression (Expression(..))
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
    { "Bird's Wings, by Olga Tuzova, Russia (15 words)": Example
        { ast:
            [ RepeatBlock
                (IntegerLiteral 360)
                ( List.fromFoldable
                    [ ProcedureCall "setx"
                        ( List.fromFoldable
                            [ Multiplication
                                (IntegerLiteral 200)
                                ( ProcedureCall "sin"
                                    ( List.fromFoldable
                                        [ ProcedureCall "repcount" Nil ]
                                    )
                                )
                            ]
                        )
                    , ProcedureCall "sety"
                        ( List.fromFoldable
                            [ Multiplication
                                (ProcedureCall "xcor" Nil)
                                ( ProcedureCall "cos"
                                    ( List.fromFoldable
                                        [ Multiplication
                                            (IntegerLiteral 2)
                                            ( ProcedureCall "repcount"
                                                Nil
                                            )
                                        ]
                                    )
                                )
                            ]
                        )
                    , ProcedureCall "home" Nil
                    ]
                )
            ]
        , source:
            "repeat 360 [setx 200 * (sin repcount) sety xcor * (cos 2 * repcount) home]"
        }
    , "Bullring": Example
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
                            [ Division
                                ( Multiplication
                                    (IntegerLiteral 360)
                                    ( ProcedureCall
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
    , "Butterfly, by Olga Tuzova, Russia (15 words)": Example
        { ast:
            [ RepeatBlock
                (IntegerLiteral 360)
                ( List.fromFoldable
                    [ ProcedureCall "setx"
                        ( List.fromFoldable
                            [ Multiplication
                                (IntegerLiteral 200)
                                ( ProcedureCall "sin"
                                    ( List.fromFoldable
                                        [ Multiplication
                                            (IntegerLiteral 2)
                                            ( ProcedureCall "repcount"
                                                Nil
                                            )
                                        ]
                                    )
                                )
                            ]
                        )
                    , ProcedureCall "sety"
                        ( List.fromFoldable
                            [ Multiplication
                                (ProcedureCall "xcor" Nil)
                                ( ProcedureCall "cos"
                                    ( List.fromFoldable
                                        [ ProcedureCall "repcount" Nil ]
                                    )
                                )
                            ]
                        )
                    , ProcedureCall "home" Nil
                    ]
                )
            ]
        , source:
            "repeat 360 [setx 200 * (sin 2 * repcount) sety xcor * (cos repcount) home]"
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
    , "Fish, by Olga Tuzova, Russia (15 words)": Example
        { ast:
            [ RepeatBlock
                (IntegerLiteral 360)
                ( List.fromFoldable
                    [ ProcedureCall "setx"
                        ( List.fromFoldable
                            [ Multiplication
                                (IntegerLiteral 200)
                                ( ProcedureCall "cos"
                                    ( List.fromFoldable
                                        [ Multiplication
                                            (IntegerLiteral 2)
                                            ( ProcedureCall "repcount"
                                                Nil
                                            )
                                        ]
                                    )
                                )
                            ]
                        )
                    , ProcedureCall "sety"
                        ( List.fromFoldable
                            [ Multiplication
                                (ProcedureCall "xcor" Nil)
                                ( ProcedureCall "cos"
                                    ( List.fromFoldable
                                        [ ProcedureCall "repcount" Nil ]
                                    )
                                )
                            ]
                        )
                    , ProcedureCall "home" Nil
                    ]
                )
            ]
        , source:
            "repeat 360 [setx 200 * (cos 2 * repcount) sety xcor * (cos repcount) home]"
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
                            [ Multiplication
                                (IntegerLiteral 180)
                                ( ProcedureCall
                                    "sin"
                                    ( List.fromFoldable
                                        [ Multiplication
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
                            [ Multiplication
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
                            [ Multiplication
                                (ValueReference "i")
                                (ValueReference "i")
                            ]
                        )
                    ]
                )
            ]
        , source: "for [i 0 2200] [fd (25 * sin :i) rt (:i * :i)]"
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
    , "Sine Wave, by Shachar Katz, Israel (15 words)": Example
        { ast:
            [ ProcedureCall "sety"
                (List.fromFoldable [ IntegerLiteral 1000 ])
            , ProcedureCall "home" Nil
            , ProcedureCall "setx"
                (List.fromFoldable [ IntegerLiteral 1000 ])
            , ForBlock
                { binder: "x"
                , initialValue: -180
                , step: 1
                , terminalValue: 180
                }
                ( List.fromFoldable
                    [ ProcedureCall "setxy"
                        ( List.fromFoldable
                            [ ValueReference "x"
                            , Multiplication
                                (IntegerLiteral 70)
                                ( ProcedureCall "sin"
                                    ( List.fromFoldable
                                        [ ValueReference "x" ]
                                    )
                                )
                            ]
                        )
                    ]
                )
            ]
        , source:
            "sety 1000 home setx 1000 for [x -180 180] [setxy :x 70 * sin :x]"
        }
    , "Slalom Scrolls":
        Example
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
                              [ Multiplication
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
    , "Smiling Fish, by Yehuda Katz, Israel (15 words)": Example
        { ast:
            [ ProcedureCall "pu" Nil
            , ProcedureCall "setx"
                (List.fromFoldable [ IntegerLiteral (-157) ])
            , ProcedureCall "pd" Nil
            , ForBlock
                { binder: "t"
                , initialValue: -315
                , step: 1
                , terminalValue: 315
                }
                ( List.fromFoldable
                    [ ProcedureCall "setxy"
                        ( List.fromFoldable
                            [ Multiplication
                                (ValueReference "t")
                                ( ProcedureCall
                                    "sin"
                                    ( List.fromFoldable
                                        [ ValueReference "t" ]
                                    )
                                )
                            , Multiplication
                                (ValueReference "t")
                                ( ProcedureCall "cos"
                                    ( List.fromFoldable
                                        [ Multiplication
                                            (IntegerLiteral 2)
                                            (ValueReference "t")
                                        ]
                                    )
                                )
                            ]
                        )
                    ]
                )
            ]
        , source:
            "pu setx -157 pd for [t -315 315] [setxy :t * sin :t :t * cos 2 * :t]"
        }
    }

