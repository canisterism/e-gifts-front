module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url



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
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url, Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


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
            ( { model | url = url }
            , Cmd.none
            )



--VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "URL Interceptor"
    , body =
        [ a [ href "https://twitter.com" ] [ h2 [] [ text "Twitter" ] ]
        , a [ href "card" ] [ h2 [] [ text "カードを作る" ] ]
        , a [ href "payment" ] [ h2 [] [ text "ドリンクを選ぶ" ] ]
        , a [ href "complete" ] [ h2 [] [ text "決済" ] ]
        ]
    }
