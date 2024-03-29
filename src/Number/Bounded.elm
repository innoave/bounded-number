module Number.Bounded exposing
    ( Bounded
    , between
    , inc, dec, tryInc, tryDec, incBy, decBy, tryIncBy, tryDecBy
    , set, trySet, map, tryMap
    , value, min, max
    , decoder, encode
    )

{-| A number bounded between a minimum and a maximum.

Once the bounds are set the value of a bounded number can not get greater than its max bound
neither can it get lower than its min bound.


# Bounded numbers

@docs Bounded


# Define bounds

@docs between


# Increment / Decrement

@docs inc, dec, tryInc, tryDec, incBy, decBy, tryIncBy, tryDecBy


# Update

@docs set, trySet, map, tryMap


# Query

@docs value, min, max


# JSON encoding / decoding

@docs decoder, encode

-}

import Json.Decode as JD
import Json.Encode as JE


{-| An opaque type that defines a number that is guaranteed to be between a given min and max bound
(inclusive).
-}
type Bounded number
    = Bounded { min_ : number, max_ : number, value_ : number }


{-| Construct a new bounded number by giving it a lower and a upper bound (inclusive). The value
will be initialized as the provided min. The min will always be the lower value, regardless of in
which order you provide the arguments.
-}
between : number -> number -> Bounded number
between a b =
    if a < b then
        Bounded { min_ = a, max_ = b, value_ = a }

    else
        Bounded { min_ = b, max_ = a, value_ = b }


{-| Increment the bounded number by 1 and clip it to the max bound.

If the resulting value is greater than the max bound the actual value is the max bound.

-}
inc : Bounded number -> Bounded number
inc bounded =
    incBy 1 bounded


{-| Decrement the bounded number by 1 and clip it to the min bound.

If the resulting value is less than the min bound the actual value is the min bound.

-}
dec : Bounded number -> Bounded number
dec bounded =
    decBy 1 bounded


{-| Try to increment the bounded number by 1. If the resulting value is greater than the max bound
`Nothing` is returned, otherwise `Just` the result is returned.
-}
tryInc : Bounded number -> Maybe (Bounded number)
tryInc bounded =
    tryIncBy 1 bounded


{-| Try to decrement the bounded number by 1. If the resulting value is less than the min bound
`Nothing` is returned, otherwise `Just` the result is returned.
-}
tryDec : Bounded number -> Maybe (Bounded number)
tryDec bounded =
    tryDecBy 1 bounded


{-| Increment the bounded number by the given number and clip it to the max bound.

If the resulting value is greater than the max bound the actual value is the max bound.

-}
incBy : number -> Bounded number -> Bounded number
incBy number bounded =
    set (value bounded + number) bounded


{-| Decrement the bounded number by the given number and clip it to the min bound.

If the resulting value is less than the min bound the actual value is the min bound.

-}
decBy : number -> Bounded number -> Bounded number
decBy number bounded =
    set (value bounded - number) bounded


{-| Try to increment the bounded number by the given number. If the resulting value is greater than
the max bound `Nothing` is returned, otherwise `Just` the result is returned.
-}
tryIncBy : number -> Bounded number -> Maybe (Bounded number)
tryIncBy number bounded =
    trySet (value bounded + number) bounded


{-| Try to decrement the bounded number by the given number. If the resulting value is less than the
min bound `Nothing` is returned, otherwise `Just` the result is returned.
-}
tryDecBy : number -> Bounded number -> Maybe (Bounded number)
tryDecBy number bounded =
    trySet (value bounded - number) bounded


{-| Set the value manually. If you try to set a value greater than the max bound, the actual value
will be the max bound. Likewise, if you try to set a value less than the min bound, the actual value
is the min bound.
-}
set : number -> Bounded number -> Bounded number
set val (Bounded bounded) =
    Bounded
        { bounded
            | value_ = clamp bounded.min_ bounded.max_ val
        }


{-| Try to set the value manually. The returned `Maybe` indicates whether the given value is between
the min and max bounds. If yes the new value is returned.

If you try to set a value that is greater than the upper bound or lower than the lower bound
`Nothing` is returned. Only if the value is between the min and max bound (inclusive) the new
`Bounded` value is returned.

-}
trySet : number -> Bounded number -> Maybe (Bounded number)
trySet val (Bounded bounded) =
    if bounded.min_ <= val && val <= bounded.max_ then
        Just <| Bounded { bounded | value_ = val }

    else
        Nothing


{-| Transforms a `Bounded` value with the given function. If the value returned by the given
function is greater than the max bound, it will clip at the max. Likewise, if the value returned by
the given function is less than the min bound, it will clip at the min.
-}
map : (number -> number) -> Bounded number -> Bounded number
map mapFn bounded =
    set (mapFn (value bounded)) bounded


{-| Try set transform a `Bounded` value with the given function. The returned `Maybe` indicates
whether the result of the transformation is within the lower and upper bounds. If yes the
transformed value is returned.

If the value returned by the given function is above the upper bound or below the lower bound
`Nothing` is returned. Only if the value returned by the function is between the min and max bound
(inclusive) the transformed `Bounded` value is returned.

-}
tryMap : (number -> number) -> Bounded number -> Maybe (Bounded number)
tryMap mapFn bounded =
    trySet (mapFn (value bounded)) bounded


{-| Get the value
-}
value : Bounded number -> number
value (Bounded { value_ }) =
    value_


{-| Get the lower bound
-}
min : Bounded number -> number
min (Bounded { min_ }) =
    min_


{-| Get the upper bound
-}
max : Bounded number -> number
max (Bounded { max_ }) =
    max_


{-| Get a JSON decoder for bounded numbers. You specify a JSON decoder for either ints or floats
so that this function knows which number type it should parse.

The returned decoder expects that the JSON representation of a bounded number is a JSON object
containing 3 number fields: "min", "max" and "value".

When using int values the JSON might look like this:

    { "min": 1
    , "max": 15
    , "value": 3
    }

The JSON value of a bounded float value might look like this:

    { "min": 0
    , "max": 1
    , "value": 0.5
    }

-}
decoder : JD.Decoder number -> JD.Decoder (Bounded number)
decoder numberDecoder =
    JD.map3 (\min_ max_ value_ -> Bounded { min_ = min_, max_ = max_, value_ = value_ })
        (JD.field "min" numberDecoder)
        (JD.field "max" numberDecoder)
        (JD.field "value" numberDecoder)


{-| Encode a bounded number as a JSON value. You specify a JSON encoder for either ints or floats
so that this function knows which number type to use.

The returned JSON value is a JSON object with 3 number fields: "min", "max" and "value".

-}
encode : (number -> JE.Value) -> Bounded number -> JE.Value
encode numberEncoder (Bounded { min_, max_, value_ }) =
    JE.object
        [ ( "min", numberEncoder min_ )
        , ( "max", numberEncoder max_ )
        , ( "value", numberEncoder value_ )
        ]
