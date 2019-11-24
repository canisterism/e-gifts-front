module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
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
    , message : String
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
    ( Model
        key
        url
        [ { id = 0
          , name = ""
          , imageUrl = ""
          }
        ]
        "String"
    , Cmd.none
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotCategories (Result Http.Error (List Category))
    | ChangeMessage String


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

        ChangeMessage message ->
            ( { model | message = message }, Cmd.none )

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
                    (cardLayouts model)
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


cardLayouts : Model -> Element Msg
cardLayouts model =
    E.column [ E.width <| E.px 560, E.height <| E.fill, E.centerX, E.centerY ]
        [ cardHeader
        , cardCarousel model.categories
        , cardPreview model.message
        , cardSubmit
        ]


cardHeader : Element msg
cardHeader =
    E.row
        [ Background.color <| E.rgb255 240 240 240
        , E.width E.fill
        , E.height <| E.fillPortion 1
        , E.spacing 30
        , E.padding 16
        ]
        [ E.el
            []
            (E.link
                [ E.width <| E.px 16
                , E.height <| E.px 16
                , E.rotate <| degrees 45
                ]
                { url = "/", label = E.text "」" }
             -- テキストは仮置 borderにする
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
        , E.paddingXY 16 8
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
                        []
                    <|
                        E.image [ E.width <| E.px 112, E.height <| E.px 112 ] <|
                            { description = "categoryImage", src = "https://e-gifts-dev.s3-ap-northeast-1.amazonaws.com/eg_gift_card_categories/images/1/sp_select/pickup.jpg" }

            -- →矢印
            , E.column
                [ E.width <| E.px 20, E.height <| E.fill ]
                [ E.text "→" ]
            ]

        -- デザインパネル
        , E.row
            [ E.height <| E.fillPortion 3
            ]
          <|
            List.repeat 5 <|
                E.el
                    []
                <|
                    E.image
                        [ E.width <| E.px 160
                        , E.height <| E.px 136
                        ]
                    <|
                        { description = "designImage", src = "https://e-gifts-dev.s3-ap-northeast-1.amazonaws.com/eg_gift_card_designs/images/1/sp_select/coffee_aroma.jpg" }
        ]


cardPreview : String -> Element Msg
cardPreview message =
    E.row
        [ E.width E.fill
        , E.height <| E.fillPortion 60
        ]
        -- 台紙
        [ E.column [ E.width E.fill, E.height <| E.fill, E.spacing 20, E.padding 20 ]
            -- 選択したデザイン
            [ E.image [ E.width E.fill, E.height <| E.px 280 ] { description = "selectedDesign", src = "https://e-gifts-dev.s3-ap-northeast-1.amazonaws.com/eg_gift_card_designs/images/1/sp_select/coffee_aroma.jpg" }

            -- メッセージ入力
            , Input.multiline [ E.padding 20, E.height <| E.px 184 ]
                { label = Input.labelHidden "message"
                , onChange = ChangeMessage
                , placeholder = Just <| Input.placeholder [] <| E.text "メッセージを入れることが出来ます"
                , spellcheck = True
                , text = message
                }
            ]
        ]


cardSubmit : Element Msg
cardSubmit =
    E.link
        [ E.width E.fill
        , Font.center
        , E.padding 16
        , Background.color <| E.rgb255 94 181 31
        , Font.color <| E.rgb255 255 255 255
        ]
        { url = "/payment", label = E.text "商品を選ぶ" }


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


class : String -> E.Attribute msg
class =
    Html.Attributes.class >> E.htmlAttribute


condClass : List ( String, Bool ) -> E.Attribute msg
condClass =
    Html.Attributes.classList >> E.htmlAttribute



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
