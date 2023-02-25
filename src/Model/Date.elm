module Model.Date exposing (Date, Month(..), compare, compareMonth, full, month, monthToString, monthsBetween, monthsBetweenMonths, offsetMonths, onlyYear, view, year)

import Html exposing (Html, text)
import Model.Util exposing (chainCompare)
import Html exposing (div)


type Date
    = Date { year : Int, month : Maybe Month }


year : Date -> Int
year (Date d) =
    d.year


month : Date -> Maybe Month
month (Date d) =
    d.month


full : Int -> Month -> Date
full y m =
    Date { year = y, month = Just m }


onlyYear : Int -> Date
onlyYear y =
    Date { year = y, month = Nothing }


{-| Given two `Date`s it returns the number of months between the two dates as an **absolute** value.
The month fields are handled as follows:

  - If both are present (`Just`), they are included normally in the calculation
  - If both are missing (`Nothing`), the number of years between the two dates is calculated
  - Otherwise the result is undefined (`Nothing`)

```
    monthsBetween (full 2020 Jan) (full 2020 Feb) --> Just 1
    monthsBetween (full 2020 Mar) (full 2020 Jan) --> Just 2
    monthsBetween (full 2020 Jan) (full 2020 Dec) --> Just 11

    monthsBetween (full 2020 Jan) (full 2021 Feb) --> Just 13
    monthsBetween (full 2021 Jan) (full 2020 Feb) --> Just 11

    monthsBetween (onlyYear 2020) (full 2021 Jan) --> Nothing
    monthsBetween (full 2020 Jan) (onlyYear 2021) --> Nothing

    monthsBetween (full 2020 Dec) (full 2021 Jan) --> Just 1

    monthsBetween (onlyYear 2020) (onlyYear 2021) --> Just 12
    monthsBetween (onlyYear 2020) (onlyYear 2022) --> Just 24
```

-}
monthsBetween : Date -> Date -> Maybe Int
monthsBetween dA dB =
    let
        y1 = year dA
        y2 = year dB
        m1 = month dA
        m2 = month dB
    in
        --verificam sa vedem daca avem vreo luna egala cu Nothing
        if((m1 == Nothing && m2 /= Nothing) || (m2 == Nothing && m1 /= Nothing)) then
            Nothing
        else
            if(m1 == Nothing && m2 == Nothing) then -- daca sunt amandoua, diferenta e diferenta dintre ani
                Just (12 * abs (y1 - y2))
            else
                if(y1 == y2)then -- daca anii sunt egali si lunile exista tratam toate cazurile
                    Just (monthsBetweenMonths (Maybe.withDefault Jan <| m1) (Maybe.withDefault Jan <| m2))
                else
                    if(compare dA dB == GT) then
                        Just (abs((monthToInt (Maybe.withDefault Jan (month dA)) - monthToInt (Maybe.withDefault Jan (month dB)) + 12*(abs((year dA) - (year dB))))))
                    else
                        Just (abs((monthToInt (Maybe.withDefault Jan (month dA)) - monthToInt (Maybe.withDefault Jan (month dB)) - 12*(abs((year dA) - (year dB))))))
    --Nothing
    --Debug.todo "Implement Date.monthsBetween"


{-| Compares two dates.
First, dates are compared by the year field. If it's equal, the month fields are used as follows:

  - If both are present (`Just`), they are compared the result is returned
  - If both are missing (`Nothing`), the dates are equal
  - Otherwise the date without month is greater

```
    Model.Date.compare (full 2020 Jan) (full 2021 Jan) --> LT
    Model.Date.compare (full 2021 Dec) (full 2021 Jan) --> GT

    Model.Date.compare (full 2020 Jan) (full 2020 Dec) --> LT
    Model.Date.compare (onlyYear 2020) (onlyYear 2021) --> LT

    Model.Date.compare (onlyYear 2020) (full 2020 Dec) --> GT
    Model.Date.compare (onlyYear 2019) (full 2020 Dec) --> LT
```

-}
compare : Date -> Date -> Order 
compare (Date d1) (Date d2) =
    let
        month1WithDefaul = Maybe.withDefault Jan (month (Date d1))
        month2WithDefaul = Maybe.withDefault Jan (month (Date d2))
    in
    -- verificam daca anii sunt egali pt a trata cazurile de month == Nothing
    if Basics.compare (year (Date d1)) (year (Date d2)) == EQ then
        if ( (month (Date d1)) == Nothing && (month (Date d2)) /= Nothing ) then
            GT
        else
            if (month (Date d1)) /= Nothing && (month (Date d2)) == Nothing then
                LT
            else
                compareMonth (month1WithDefaul) (month2WithDefaul)   
    else          
        if Basics.compare (year (Date d1)) (year (Date d2)) == GT then
            GT
        else
            LT
       
            
    -- EQ
    -- Debug.todo "Implement Model.Date.compare"


{-| Given a current date and the number of months, it returns a new date with the given number of months passed.

-}
offsetMonths : Int -> Date -> Date
offsetMonths months (Date d) =
    let
        addMonths =
            modBy 12 months

        addedMonths =
            d.month
                |> Maybe.map monthToInt
                |> Maybe.map ((+) addMonths)

        newMonth =
            addedMonths
                |> Maybe.map (modBy 12)
                |> Maybe.andThen intToMonth

        addYears =
            months // 12

        extraYear =
            if Maybe.withDefault 0 addedMonths >= 12 then
                1

            else
                0
    in
    Date { year = d.year + addYears + extraYear, month = newMonth }


view : Date -> Html msg -- format de data: MM-yyyy 
view (Date d) =
    div [] [text (( (d.month |> Maybe.withDefault Jan ) |> monthToString) ++ " " ++ String.fromInt d.year)]
    --Debug.todo "Implement Model.Date.view"



-- MONTH


type Month
    = Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec


intToMonth : Int -> Maybe Month
intToMonth idx =
    case idx of
        0 ->
            Just Jan

        1 ->
            Just Feb

        2 ->
            Just Mar

        3 ->
            Just Apr

        4 ->
            Just May

        5 ->
            Just Jun

        6 ->
            Just Jul

        7 ->
            Just Aug

        8 ->
            Just Sep

        9 ->
            Just Oct

        10 ->
            Just Nov

        11 ->
            Just Dec

        _ ->
            Nothing


monthToInt : Month -> Int
monthToInt m =
    case m of
        Jan ->
            0

        Feb ->
            1

        Mar ->
            2

        Apr ->
            3

        May ->
            4

        Jun ->
            5

        Jul ->
            6

        Aug ->
            7

        Sep ->
            8

        Oct ->
            9

        Nov ->
            10

        Dec ->
            11


monthToString : Month -> String
monthToString m =
    case m of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


compareMonth : Month -> Month -> Order
compareMonth m1 m2 =
    Basics.compare (monthToInt m1) (monthToInt m2)


{-| Returns the number of months between two months as an **absolute** value.

    monthsBetweenMonths Jan Jan --> 0

    monthsBetweenMonths Jan Apr --> 3

    monthsBetweenMonths Apr Jan --> 3

-}
monthsBetweenMonths : Month -> Month -> Int 
monthsBetweenMonths m1 m2 =
    abs ((monthToInt m1) - (monthToInt m2))
    -- 0
    --Debug.todo "Implement Date.monthsBetweenMonths"
