module BoundedTest exposing (suite)

import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float, int)
import Json.Decode
import Json.Encode
import Number.Bounded as Bounded exposing (Bounded)
import Random
import Shrink
import Test exposing (..)


anyBoundedInt : Fuzzer (Bounded Int)
anyBoundedInt =
    Fuzz.custom
        (Random.pair (Random.int -430 0) (Random.int 1 420)
            |> Random.andThen
                (\( b1, b2 ) ->
                    Random.int b1 b2
                        |> Random.map
                            (\v ->
                                Bounded.between b1 b2
                                    |> Bounded.set v
                            )
                )
        )
        Shrink.noShrink


suite : Test
suite =
    describe "A bounded number"
        [ defineBounds
        , incrementDecrement
        , update
        , jsonEncodingDecoding
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
        , test "between 42 -43 creates a bounded number with min -43 and max 42" <|
            \_ ->
                Bounded.between 42 -43
                    |> Expect.all
                        [ Expect.equal -43 << Bounded.min
                        , Expect.equal 42 << Bounded.max
                        ]
        ]


incrementDecrement : Test
incrementDecrement =
    describe "increment or decrement a bounded number never results in a value beyond its bounds"
        [ describe "incBy clips the result to max bound"
            [ test "41 [-43, 42] incremented by 1 gives 42" <|
                \_ ->
                    Bounded.between -43 42
                        |> Bounded.set 41
                        |> Bounded.incBy 1
                        |> Bounded.value
                        |> Expect.equal 42
            , test "42 [-43, 42] incremented by 1 gives 42" <|
                \_ ->
                    Bounded.between -43 42
                        |> Bounded.set 42
                        |> Bounded.incBy 1
                        |> Bounded.value
                        |> Expect.equal 42
            , fuzz2 anyBoundedInt int "incBy any number never results in a value that is greater than max bound" <|
                \bounded num ->
                    Bounded.incBy num bounded
                        |> Bounded.value
                        |> Expect.atMost (Bounded.max bounded)
            ]
        , describe "decBy clips the result to min bound"
            [ test "-42 [-43, 42] decremented by 1 gives -43" <|
                \_ ->
                    Bounded.between -43 42
                        |> Bounded.set -42
                        |> Bounded.decBy 1
                        |> Bounded.value
                        |> Expect.equal -43
            , test "-43 [-43, 42] decremented by 1 gives -43" <|
                \_ ->
                    Bounded.between -43 42
                        |> Bounded.set -43
                        |> Bounded.decBy 1
                        |> Bounded.value
                        |> Expect.equal -43
            , fuzz2 anyBoundedInt int "decBy any number never results in a value that is less than min bound" <|
                \bounded num ->
                    Bounded.decBy num bounded
                        |> Bounded.value
                        |> Expect.atLeast (Bounded.min bounded)
            ]
        , describe "tryIncBy does not return a result if it would overrun max bound"
            [ test "try increment 41 [-43, 42] by 1 gives just 42" <|
                \_ ->
                    Bounded.between -43 42
                        |> Bounded.set 41
                        |> Bounded.tryIncBy 1
                        |> Maybe.map Bounded.value
                        |> Expect.equal (Just 42)
            , test "try increment 42 [-43, 42] by 1 gives nothing" <|
                \_ ->
                    Bounded.between -43 42
                        |> Bounded.set 42
                        |> Bounded.tryIncBy 1
                        |> Maybe.map Bounded.value
                        |> Expect.equal Nothing
            ]
        , describe "tryDecBy does not return a result if it would overrun min bound"
            [ test "try decrement -42 [-43, 42] by 1 gives just -43" <|
                \_ ->
                    Bounded.between -43 42
                        |> Bounded.set -42
                        |> Bounded.tryDecBy 1
                        |> Maybe.map Bounded.value
                        |> Expect.equal (Just -43)
            , test "try decrement -43 [-43, 42] by 1 gives nothing" <|
                \_ ->
                    Bounded.between -43 42
                        |> Bounded.set -43
                        |> Bounded.tryDecBy 1
                        |> Maybe.map Bounded.value
                        |> Expect.equal Nothing
            ]
        ]


update : Test
update =
    describe "updating a bounded number never results in a value beyond its bounds"
        [ describe "set clips the result to its bounds"
            [ fuzz2 anyBoundedInt int "set a bounded number to any value never results in a value out of its bounds" <|
                \bounded newValue ->
                    Bounded.set newValue bounded
                        |> Bounded.value
                        |> Expect.all
                            [ Expect.atMost (Bounded.max bounded)
                            , Expect.atLeast (Bounded.min bounded)
                            ]
            ]
        , describe "trySet does not return a result if it would be out its bounds"
            [ test "try set x [-43, 42] to 42 gives just 42" <|
                \_ ->
                    Bounded.between -43 42
                        |> Bounded.trySet 42
                        |> Maybe.map Bounded.value
                        |> Expect.equal (Just 42)
            , test "trySet x [-43, 42] to 43 gives nothing" <|
                \_ ->
                    Bounded.between -43 42
                        |> Bounded.trySet 43
                        |> Maybe.map Bounded.value
                        |> Expect.equal Nothing
            , test "try set x [-43, 42] to -43 gives just -43" <|
                \_ ->
                    Bounded.between -43 42
                        |> Bounded.trySet -43
                        |> Maybe.map Bounded.value
                        |> Expect.equal (Just -43)
            , test "trySet x [-43, 42] to -44 gives nothing" <|
                \_ ->
                    Bounded.between -43 42
                        |> Bounded.trySet -44
                        |> Maybe.map Bounded.value
                        |> Expect.equal Nothing
            ]
        ]


jsonEncodingDecoding : Test
jsonEncodingDecoding =
    describe "we have a default JSON representation"
        [ describe "encode a bounded value into a JSON value"
            [ test "encode 5 [-43, 42] as JSON string" <|
                \_ ->
                    Bounded.between -43 42
                        |> Bounded.set 5
                        |> Bounded.encode Json.Encode.int
                        |> Json.Encode.encode 4
                        |> Expect.equal """{
    "min": -43,
    "max": 42,
    "value": 5
}"""
            ]
        , describe "decode a bounded value from a JSON string"
            [ test "decode -17 [-43, 42] from JSON string" <|
                \_ ->
                    let
                        bounded17 =
                            Bounded.between -43 42
                                |> Bounded.set -17
                    in
                    Json.Decode.decodeString
                        (Bounded.decoder Json.Decode.int)
                        """{ "min": -43, "max": 42, "value": -17 }"""
                        |> Expect.equal (Ok bounded17)
            ]
        ]
