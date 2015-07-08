module Client where

import Html as H
import StartApp
import Html.Attributes exposing (autofocus, class, id, name, placeholder, disabled)
import Html.Events exposing (onClick, on, targetValue)
import Task exposing (Task, andThen)
import Date exposing (Date)
import Maybe exposing (Maybe(Just, Nothing))
import Debug exposing (log)
import Regex exposing (Regex, caseInsensitive, contains, regex)

main =
  StartApp.start {
    model  = initModel,
    update = update,
    view   = view
  }

type alias Model =
  { currentTab  : Tab
--  , userData    : UserData
  }

type alias LoggedInUser =
  {
    username    : String,
    accessToken : String
  }

type alias NewUser =
  { username : String
  , password : String
--  , email    : String
  }

type alias User =
  { pk       : Int
  , username : String
--  , email : String
  }

type alias Url =
  { pk                 : Int
  , url                : String
  , creator            : User
  , createdDatetime    : Date -- Date includes time
  , creatorIp          : String
  , lastAccessDatetime : Date
  }

type NewUrl = InvalidUrl String | ValidUrl String

type alias UrlData =
  { urls     : List Url
  , newUrl   : NewUrl
  , errorMsg : Maybe String
  }

type alias UserData = String -- Dummy

type Tab = Login | MyUrls (LoggedInUser, UrlData)

type UrlAction = UpdateUrlInput String
               | SubmitUrlInput

type Action = MyUrlsAction UrlAction
            | ChangeTab Tab

initModel : Model
initModel = { currentTab = myUrls "test" "test" }

myUrls : String -> String -> Tab
myUrls username token = MyUrls ( { username = username, accessToken = token }, { urls = [], newUrl = InvalidUrl "", errorMsg = Nothing } )

topNav : Signal.Address Action -> Tab -> H.Html
topNav address currentTab =
  H.div [ id "topNav", class "topNav" ]
    [ H.div [ id "urlsButton", class "topNavButton" ] [ H.text "URLs" ]
    , H.div [ id "usersButton", class "topNavButton" ] [ H.text "Users" ]
    ]

-- use let bindings instead of inlining all the attrs, etc.?
view : Signal.Address Action -> Model -> H.Html
view address model =
  case model.currentTab of
    MyUrls (user, urlData) ->
      H.div [ id "pageContents" ]
        [ topNav address model.currentTab
        , H.div
            [ id "mainBody"
            , class "mainBody"
            ]
            [ myUrlsTab (Signal.forwardTo address MyUrlsAction) urlData ]
        ]
    _ -> H.text "Error!"

myUrlsTab : Signal.Address UrlAction -> UrlData -> H.Html
myUrlsTab address urlData = addNewUrl address urlData.newUrl

addNewUrl : Signal.Address UrlAction -> NewUrl -> H.Html
addNewUrl address newUrl =
  let extraSubmitAttrs = case newUrl of
    InvalidUrl _ -> [ disabled True ]
    ValidUrl   _ -> [ onClick address SubmitUrlInput ]
  in
    H.div []
      [ H.input
          [ id "newUrl"
          , class "textInput"
          , placeholder "https://example.com"
          , autofocus True
          , name "newUrl"
          , on "input" targetValue (Signal.message address << UpdateUrlInput)
        ] []
      , H.button
          ([ id "newUrlSubmitButton", class "submitButton", name "newUrlSubmitButton" ] ++ extraSubmitAttrs)
          [ H.text "Submit" ]
    ]

update : Action -> Model -> Model
update action model =
  case (action, model.currentTab) of
    (MyUrlsAction a, MyUrls (user, urlData)) ->
      { model | currentTab <- MyUrls (user, updateUrl a user urlData) }
    _ -> model

updateUrl : UrlAction -> LoggedInUser -> UrlData -> UrlData
updateUrl action user urlData =
  case action of
    UpdateUrlInput url    ->
      log ("Update URL: " ++ url)
      { urlData | newUrl <- validateUrl url }
    SubmitUrlInput  ->
      log "Submitted URL!"
      urlData

validUrlRegex : Regex
validUrlRegex = caseInsensitive <| regex "^https?://.+\\..+"

validateUrl : String -> NewUrl
validateUrl url = if contains validUrlRegex url then ValidUrl url else InvalidUrl url
