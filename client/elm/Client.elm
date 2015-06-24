module Client where

import Html as H
import StartApp
import Html.Attributes exposing (class, id)
import Task exposing (Task, andThen)
import Date exposing (Date)
import Maybe exposing (Maybe(Just, Nothing))

main =
  StartApp.start {
    model  = initModel Nothing,
    update = update,
    view   = view
  }

type alias Model =
  { userContext : Maybe LoggedInUser
  , currentTab  : Tab
  , notice      : Maybe String
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

type alias NewUrl     = String
type alias PrimaryKey = Int

type alias User =
  { pk       : PrimaryKey
  , username : String
--  , email : String
  }


type alias Url =
  { pk                 : PrimaryKey
  , url                : String
  , creator            : User
  , createdDatetime    : Date -- Date includes time
  , creatorIp          : String
  , lastAccessDatetime : Date
  }

type Tab = Login | MyUrls (List Url) | Users (List User)

type Action = AddUrl    NewUrl
            | DeleteUrl PrimaryKey
            | AddUser   NewUser
            | ChangeTab Tab

initModel : Maybe LoggedInUser -> Model
initModel user = { userContext = user, currentTab = Login, notice = Nothing }

topNav : Tab -> H.Html
topNav currentTab =
  H.div [ id "topNav", class "topNav" ]
    [ H.div [ id "urlsButton", class "topNavButton" ] [ H.text "URLs" ]
    , H.div [ id "usersButton", class "topNavButton" ] [ H.text "Users" ]
    ]

view : Signal.Address Action -> Model -> H.Html
view address model =
  H.div [ class "pageWrapper" ]
    [ topNav model.currentTab
    , H.div [ id "mainBody", class "mainBody" ] [ H.text "lower portion" ]
    ]

update : Action -> Model -> Model
update action model =
  case action of
    AddUrl url -> model
    DeleteUrl id -> model
    AddUser user -> model
    ChangeTab tab -> model
