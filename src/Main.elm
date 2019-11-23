module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
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
                    ( { model | categories = Debug.log "categories:" categories }, Cmd.none )

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
                [ E.layout []
                    (cardLayouts model.categories)
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



-- showCategory : Category -> Element msg
-- showCategory category =
--     el []
--         ( h3 [] [ text category.name ]
--         , img [ src category.imageUrl ] []
--         )


cardLayouts : List Category -> Element msg
cardLayouts categories =
    E.column [ E.width <| E.px 560, E.height <| E.fill, E.centerX, E.centerY ]
        [ cardHeader
        , cardCarousel categories
        , cardPreview
        ]


cardHeader : Element msg
cardHeader =
    E.row
        [ Background.color <| E.rgb255 240 240 240
        , E.width E.fill
        , E.height <| E.fillPortion 1
        , E.spacing 30
        , E.paddingXY 16 8
        ]
        [ E.el
            []
            (E.link
                [ E.width <| E.px 16
                , E.height <| E.px 16
                , E.rotate <| degrees 45
                , Border.color <| E.rgb255 83 83 83
                , Border.widthEach <|
                    { bottom = 4
                    , left = 4
                    , right = 0
                    , top = 0
                    }
                , Border.solid
                ]
                { url = "/", label = E.text "" }
            )
        , E.el
            [ E.centerX
            , Font.color <| E.rgb255 123 117 112
            , Font.bold
            , Font.size 16
            ]
            (E.text "カードを作成する")
        , E.el
            [ Font.color <| E.rgb255 87 185 0
            , Font.bold
            , Font.size 16
            ]
            (E.link [] { url = "/payment", label = E.text "次へ" })
        ]


cardCarousel : List Category -> Element msg
cardCarousel categories =
    -- 元締め
    E.column
        [ E.width E.fill
        , E.height <| E.fillPortion 6
        , Border.color <| E.rgb255 120 20 20
        , Border.width 4
        ]
        -- カテゴリ行
        [ E.row [ E.width E.fill, E.height <| E.fillPortion 2 ]
            -- ←矢印
            [ E.column
                [ E.width <| E.px 20, E.height <| E.fill ]
                [ E.text "←" ]

            -- カテゴリパネル
            , E.row
                [ E.width E.fill
                , E.height <| E.fill
                ]
              <|
                List.repeat 5 <|
                    E.el
                        [ E.width <| E.fill
                        , E.height <| E.fill
                        , Border.color <| E.rgb255 120 20 20
                        , Border.width 1
                        ]
                    <|
                        E.text
                            "Category"

            -- →矢印
            , E.column
                [ E.width <| E.px 20, E.height <| E.fill ]
                [ E.text "→" ]
            ]

        -- デザインパネル
        , E.row
            [ E.width E.fill
            , E.height <| E.fillPortion 3
            ]
          <|
            List.repeat 5 <|
                E.el
                    [ E.width <| E.fill
                    , E.height <| E.fill
                    , Border.color <| E.rgb255 120 20 20
                    , Border.width 1
                    ]
                <|
                    E.text
                        "Design"
        ]


cardPreview : Element msg
cardPreview =
    E.row
        [ E.width E.fill
        , E.height <| E.fillPortion 30
        , E.centerY
        , E.centerX
        , E.spacing 30
        , Border.color <| E.rgb255 120 20 20
        , Border.width 1
        ]
        [ E.el [ E.padding 30 ]
            (E.text "カード")
        ]


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


class : String -> E.Attribute msg
class =
    Html.Attributes.class >> E.htmlAttribute



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



-- 頑張って積み上げたレイアウトをelm-uiで実装する
