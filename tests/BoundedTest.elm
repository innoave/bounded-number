module BoundedTest exposing (suite)

import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (float, int)
import Number.Bounded as Bounded
import Test exposing (..)


suite : Test
suite =
    describe "A bounded number"
        [ defineBounds
        , incrementDecrement
        ]


defineBounds : Test
defineBounds =
    describe "is created by defining its bounds"
        [ fuzz2 int int "between two int values" <|
            \int1 int2 ->
                Bounded.between int1 int2
                    |> Expect.all
                        [ Expect.equal (min int1 int2) << Bounded.min
                        , Expect.equal (max int1 int2) << Bounded.max
                        ]
        , fuzz2 float float "between two float values" <|
            \float1 float2 ->
                Bounded.between float1 float2
                    |> Expect.all
                        [ Expect.within (Relative 0.0001) (min float1 float2) << Bounded.min
                        , Expect.within (Relative 0.0001) (max float1 float2) << Bounded.max
                        ]
        , fuzz2 int int "the value of a newly created bounded number is always equal the lower bound" <|
            \int1 int2 ->
                Bounded.between int1 int2
                    |> Expect.equal (min int1 int2)
                    << Bounded.value
        , fuzz int "lower and upper bound of same value is possible" <|
            \bound ->
                Bounded.between bound bound
                    |> Expect.all
                        [ Expect.equal bound << Bounded.min
                        , Expect.equal bound << Bounded.max
                        , Expect.equal bound << Bounded.value
                        ]
        ]


incrementDecrement : Test
incrementDecrement =
    describe "increment or decrement the value of a bounded number within its defined bounds"
        []
