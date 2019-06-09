module Number.Bounded exposing
    ( Bounded
    , between
    , set, trySet, map, tryMap
    , inc, dec, tryInc, tryDec, incBy, decBy, tryIncBy, tryDecBy
    , value, min, max
    , decoder, encode
    )

{-| A number bounded between a minimum and a maximum.

@docs Bounded

@docs between

@docs set, trySet, map, tryMap

@docs inc, dec, tryInc, tryDec, incBy, decBy, tryIncBy, tryDecBy

@docs value, min, max

-}

import Json.Decode as JD
import Json.Encode as JE


{-| An opaque type that defines a number that is guaranteed to be between a given min and max bound
(inclusive).
-}
type Bounded number
    = Bounded { min_ : number, max_ : number, value_ : number }


{-| Construct a new bounded value by giving it a lower and a upper bound (inclusive). The value
will be initialized as the provided min. The min will always be the lower value, regardless of in
which order you provide the arguments.
-}
between : number -> number -> Bounded number
between a b =
    if a < b then
        Bounded { min_ = a, max_ = b, value_ = a }

    else
        Bounded { min_ = b, max_ = a, value_ = b }


decoder : JD.Decoder number -> JD.Decoder (Bounded number)
decoder numberDecoder =
    JD.map3 (\min_ max_ value_ -> Bounded { min_ = min_, max_ = max_, value_ = value_ })
        (JD.field "min" numberDecoder)
        (JD.field "max" numberDecoder)
        (JD.field "value" numberDecoder)


encode : (number -> JE.Value) -> Bounded number -> JE.Value
encode numberEncoder (Bounded { min_, max_, value_ }) =
    JE.object
        [ ( "min", numberEncoder min_ )
        , ( "max", numberEncoder max_ )
        , ( "value", numberEncoder value_ )
        ]


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


inc : Bounded number -> Bounded number
inc bounded =
    incBy 1 bounded


dec : Bounded number -> Bounded number
dec bounded =
    decBy 1 bounded


tryInc : Bounded number -> Maybe (Bounded number)
tryInc bounded =
    tryIncBy 1 bounded


tryDec : Bounded number -> Maybe (Bounded number)
tryDec bounded =
    tryDecBy 1 bounded


incBy : number -> Bounded number -> Bounded number
incBy number bounded =
    set (value bounded + number) bounded


decBy : number -> Bounded number -> Bounded number
decBy number bounded =
    set (value bounded - number) bounded


tryIncBy : number -> Bounded number -> Maybe (Bounded number)
tryIncBy number bounded =
    trySet (value bounded + number) bounded


tryDecBy : number -> Bounded number -> Maybe (Bounded number)
tryDecBy number bounded =
    trySet (value bounded - number) bounded


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
max (Bounded { min_ }) =
    min_
