module Model.Event exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Model.Event.Category exposing (EventCategory(..))
import Model.Interval as Interval exposing (Interval)
import Model.Date exposing (year)


type alias Event =
    { title : String
    , interval : Interval
    , description : Html Never
    , category : EventCategory
    , url : Maybe String
    , tags : List String
    , important : Bool
    }


categoryView : EventCategory -> Html Never
categoryView category =
    case category of
        Academic ->
            text "Academic"

        Work ->
            text "Work"

        Project ->
            text "Project"

        Award ->
            text "Award"

--functia de sortare a intervalelor folosind functia Interval.compare
sortWithInterval : Event -> Event -> Order
sortWithInterval i1 i2 = Interval.compare i1.interval i2.interval


sortByInterval : List Event -> List Event -- am utilizat List.sortWith pentru a folosi functia de comparare a 2 intervale
sortByInterval events =
    events |> List.sortWith sortWithInterval 
    --events
    --Debug.todo "Implement Event.sortByInterval"


view : Event -> Html Never
view event =
    
    div [classList [("event", True), ("event-important", event.important)]] [
        div [class "event-title"] [text event.title],
        div [class "event-description"] [Html.map never event.description],
        div [class "event-category"] [categoryView event.category],
        div [class "event-url"] [text (Maybe.withDefault "" event.url)],
        div [class "event-interval"][Interval.view event.interval]
        ]

    -- Debug.todo "Implement the Model.Event.view function"
