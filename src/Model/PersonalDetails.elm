module Model.PersonalDetails exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, id, href) 


type alias DetailWithName =
    { name : String
    , detail : String
    }


type alias PersonalDetails =
    { name : String
    , contacts : List DetailWithName
    , intro : String
    , socials : List DetailWithName
    }


view : PersonalDetails -> Html msg
view details =
    let
        --lambda expression pentru a afisa o lista de contacte
        lambdaExpForContacts = (\x -> p [] [text x.detail]) 
        --lambda expression pentru a afisa o lista de link-uri cu a href
        lambdaExpForLink = (\x -> p [] [a [href x.detail] [text x.detail]])
    in
    
    div [] 
    [ h1 [id "name"] [text details.name],
      em [id "intro"] [text details.intro],
      div [class "contact-detail"] [p [] (List.map lambdaExpForContacts details.contacts)],
      div [class "social-link"] [p [] (List.map lambdaExpForLink details.socials)]
    ]
    -- Debug.todo "Implement the Model.PersonalDetails.view function"
    
