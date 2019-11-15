module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as D exposing (Decoder)
import Url
import Url.Parser as P exposing (Parser)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , categories : List Category
    }


type alias Category =
    { id : Int
    , name : String
    , imageUrl : String
    }


type alias Design =
    { id : Int
    , name : String
    , imageUrl : String
    , categoryId : Int
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key
        url
        [ { id = 0
          , name = ""
          , imageUrl = ""
          }
        ]
    , Cmd.none
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotCategories (Result Http.Error (List Category))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- こっちが先に発生する
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        -- pushUrlされたりするとUrlChangedが発行される
        -- modelを書き換えてるだけで実質何もしてない
        UrlChanged url ->
            case urlToRoute url of
                Home ->
                    ( { model | url = url }
                    , Cmd.none
                    )
                Card ->
                    ( { model | url = url }
                    , getCategories
                    )
                Payment ->
                    ( { model | url = url }
                    , Cmd.none
                    )
                Complete ->
                    ( { model | url = url }
                    , Cmd.none
                    )
                NotFound ->
                    ( { model | url = url }
                    , Cmd.none
                    )

        GotCategories result ->
            case result of
                Ok categories ->
                    ({ model | categories = Debug.log "categories:" categories }, Cmd.none)

                Err _ ->
                    ( model, Cmd.none )


type Route
    = Home
    | Card
    | Payment
    | Complete
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    P.oneOf
        [ P.map Home P.top
        , P.map Card (P.s "card")
        , P.map Payment (P.s "payment")
        , P.map Complete (P.s "complete")
        ]


urlToRoute : Url.Url -> Route
urlToRoute url =
    Maybe.withDefault
        NotFound
        (P.parse routeParser url)



--VIEW


view : Model -> Browser.Document Msg
view model =
    case urlToRoute model.url of
        Home ->
            { title = "HOME"
            , body =
                [ viewAnker "/card" "カードを作る" ]
            }

        Card ->
            { title = "CARD"
            , body =
                [ h1 [] [ text "カード" ]
                , viewAnker "/payment" "ドリンクを選ぶ"
                ]
            }

        Payment ->
            { title = "Payment"
            , body =
                [ h1 [] [ text "決済画面" ]
                , viewAnker "/complete" "決済"
                ]
            }

        Complete ->
            { title = "Complete"
            , body =
                [ h1 [] [ text "購入完了" ]
                , viewAnker "/" "もう一回！"
                ]
            }

        NotFound ->
            { title = "Not Found"
            , body =
                [ h1 [] [ text "ページが見つかりませんでした" ]
                ]
            }


viewAnker : String -> String -> Html msg
viewAnker path label =
    a [ href path ] [ h2 [] [ text label ] ]



-- HTTP


getCategories : Cmd Msg
getCategories =
    Http.get
        { url = "http://localhost:3000/categories"
        , expect = Http.expectJson GotCategories categoriesDecoder
        }


categoriesDecoder : Decoder (List Category)
categoriesDecoder =
     D.list categoryDecoder


categoryDecoder : Decoder Category
categoryDecoder =
    D.map3 Category
        (D.at [ "id" ] D.int)
        (D.at [ "name" ] D.string)
        (D.at [ "imageUrl" ] D.string)
