module Client where

import Date exposing (Date, fromString)
import Debug exposing (log)
import Html as H
import Html exposing (Attribute)
import Html.Attributes exposing (autofocus, class, id, name, placeholder, disabled)
import Html.Events exposing (onClick, on, targetValue, keyCode)
import Json.Decode as JDecode
import List
import Maybe exposing (Maybe(Just, Nothing), withDefault)
import Regex exposing (Regex, caseInsensitive, contains, regex)
import Result exposing (toMaybe)
import StartApp
import Task exposing (Task, andThen)

--
-- Main function!
--

main =
  StartApp.start {
    model  = initModel,
    update = update,
    view   = view
  }

--
-- Type aliases and records
--

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

--
-- Update model stuff
--

initModel : Model
initModel = { currentTab = myUrls "test" "test" }

-- Main update function -- delegates updates to separate function per Action
update : Action -> Model -> Model
update action model =
  case (action, model.currentTab) of
    (MyUrlsAction a, MyUrls (user, urlData)) ->
      { model | currentTab <- MyUrls (user, updateUrl a user urlData) }
    _ -> model

-- Update action for MyUrls tab
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

-- Page initialization functions

-- Placeholder for actual URL data
dummyUrls : List Url
dummyUrls =
  let dummyDate1 = toMaybe <| fromString "2015-05-13T21:45:00.000+00:00"
      dummyDate2 = toMaybe <| fromString "2015-07-13T21:45:00.000+00:00"
  in
    case (dummyDate1, dummyDate2) of
      (Just d1, Just d2) -> [ Url -1 "https://aldiyen.com" (User -1 "aldiyen") d1 "192.168.1.10" d2 ]
      _ -> []

-- Make a new MyUrls tab with the specified username and access token
myUrls : String -> String -> Tab
myUrls username token = MyUrls ( { username = username, accessToken = token }, { urls = dummyUrls, newUrl = InvalidUrl "", errorMsg = Nothing } )

-- Update the URL list (used for async loading of URL list)
updateUrlList : List Url -> Tab -> Tab
updateUrlList urls currentTab =
  case currentTab of
    MyUrls (user, urlData) -> MyUrls (user, { urlData | urls <- urls })
    _ ->
      log "Current tab is not MyUrls; no changes made"
      currentTab

--
-- View functions
--

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
    _ -> H.text "Error! Not implemented!"

topNav : Signal.Address Action -> Tab -> H.Html
topNav address currentTab =
  H.div [ id "topNav", class "topNav" ]
    [ H.div [ id "urlsButton", class "topNavButton" ] [ H.text "URLs" ]
    , H.div [ id "usersButton", class "topNavButton" ] [ H.text "Users" ]
    ]

myUrlsTab : Signal.Address UrlAction -> UrlData -> H.Html
myUrlsTab address urlData =
  H.div [ class "pageContents" ]
    [ addNewUrl address urlData.newUrl
    , urlList urlData
    ]

urlList : UrlData -> H.Html
urlList urlData =
  let urlToCell url =
        H.tr [ class "dataRow" ]
          [ cell url.url
          , cell (dateToString url.createdDatetime)
          , cell url.creatorIp
          , cell (dateToString url.lastAccessDatetime)
          ]
  in
    H.div [ id "urlList" ]
      [ H.table [ id "urlListTable" ]
        ([ H.tr [ class "headerRow" ]
          [ header "URL", header "Creation Date", header "Creator IP", header "Last Access" ]
        ] ++ List.map urlToCell urlData.urls)
      ]

header : String -> H.Html
header contents = H.th [ class "tableHeader" ] [ H.text contents ]

cell : String -> H.Html
cell contents = H.td [ class "tableCell" ] [ H.text contents ]

addNewUrl : Signal.Address UrlAction -> NewUrl -> H.Html
addNewUrl address newUrl =
  let urlIsValid = case newUrl of
        InvalidUrl _ -> False
        ValidUrl   _ -> True
  in
    H.div [ id "addNewUrl" ]
      [ H.input
          [ id "newUrl"
          , class "textInput"
          , placeholder "https://example.com"
          , autofocus True
          , name "newUrl"
          , on "input" targetValue (Signal.message address << UpdateUrlInput)
          , onEnter address SubmitUrlInput
          ] []
      , H.button
          ([ id "addUrlButton", class "submitButton", name "addUrlButton", onClick address SubmitUrlInput ] ++ if not urlIsValid then [ disabled True ] else [])
          [ H.text "Add URL" ]
      ]

--
-- Some general helper functions
--

-- Stolen from Elm's TodoMVC (and seems like it should be easier to accomplish?)
onEnter : Signal.Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (JDecode.customDecoder keyCode is13)
      (\_ -> Signal.message address value)

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not keycode for enter"

-- Function to turn a Date into a reasonable-to-represent String (toString on a Date just evaluates to "<internal structure>")
dateToString : Date -> String
dateToString d =
  let day   = toString (Date.day d)
      month = toString (Date.month d)
      year  = toString (Date.year d)
  in day ++ "-" ++ month ++ "-" ++ year
