module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Events exposing (onResize)
import Browser.Navigation as Navigation exposing (Key)
import Element exposing (Color, Device, DeviceClass(..), Element, Length, Orientation(..), alignRight, centerX, centerY, column, el, fill, layout, link, padding, paddingEach, px, rgb, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input exposing (button, labelHidden, placeholder, search)
import FeatherIcons exposing (Icon)
import Graphql.Document as Document
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, div, h1, p, pre, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import PrintAny
import RemoteData exposing (RemoteData(..), WebData)
import ShopifyApi.Object
import ShopifyApi.Object.Collection as Collection
import ShopifyApi.Object.CollectionConnection as CollectionConnection
import ShopifyApi.Object.CollectionEdge as CollectionEdge
import ShopifyApi.Object.Image as Image
import ShopifyApi.Object.ImageConnection as ImageConnection
import ShopifyApi.Object.ImageEdge as ImageEdge
import ShopifyApi.Object.PageInfo as PageInfo
import ShopifyApi.Object.Product as Product
import ShopifyApi.Object.ProductConnection as ProductConnection
import ShopifyApi.Object.ProductEdge as ProductEdge
import ShopifyApi.Query as Query
import ShopifyApi.Scalar as Scalar
import ShopifyApi.ScalarCodecs as ScalarCodecs exposing (Id, Url)
import Svg exposing (path, style)
import Svg.Attributes exposing (cx, cy, d, r, transform, version, x1, x2, y1, y2)
import Tent as Tent exposing (tent)
import Url exposing (Url)
import Url.Builder as UrlBuilder exposing (absolute)
import Url.Parser as UrlParser exposing (..)


type alias Response =
    Maybe Collection


collectionByHandleQuery : SelectionSet (Maybe Collection) RootQuery
collectionByHandleQuery =
    Query.collectionByHandle
        { handle = "frontpage"
        }
        collectionSelection


collectionSelection : SelectionSet Collection ShopifyApi.Object.Collection
collectionSelection =
    SelectionSet.map3 Collection
        Collection.id
        Collection.handle
        (Collection.products (\r -> { r | first = Present 4 }) (ProductConnection.edges (ProductEdge.node productSelection)))


productSelection : SelectionSet Product ShopifyApi.Object.Product
productSelection =
    SelectionSet.map3 Product
        Product.handle
        (Product.images
            (\r -> { r | first = Present 4 })
            (ImageConnection.edges (ImageEdge.node imageSelection))
        )
        (Product.description (\r -> { r | truncateAt = Absent }))


imageSelection : SelectionSet Image ShopifyApi.Object.Image
imageSelection =
    SelectionSet.map Image
        Image.src


makeRequest : Maybe String -> Maybe String -> Cmd Msg
makeRequest url token =
    case url of
        Just innerUrl ->
            case token of
                Just innerToken ->
                    collectionByHandleQuery
                        |> Graphql.Http.queryRequest innerUrl
                        |> Graphql.Http.withHeader "X-Shopify-Storefront-Access-Token" innerToken
                        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)

                Nothing ->
                    Cmd.none

        Nothing ->
            Cmd.none


type alias Collection =
    { id : Id
    , handle : String
    , products : List Product
    }


type alias Product =
    { handle : String
    , images : List Image
    , description : String
    }


type alias Image =
    { src : ScalarCodecs.Url
    }


type Msg
    = GotResponse RemoteDataResponse
    | ClickedLink UrlRequest
    | ChangedUrl Url
    | DeviceClassified Device
    | Search String
    | Join


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { response : List RemoteDataResponse
    , key : Key
    , url : Maybe String
    , token : Maybe String
    , device : Device
    , currentRoute : Route
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Join ->
            ( model, Cmd.none )

        Search queryText ->
            ( model, Cmd.none )

        DeviceClassified device ->
            ( { model | device = device }
            , Cmd.none
            )

        GotResponse response ->
            case model.response of
                head :: rest ->
                    ( { model | response = response :: rest }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.key url.path )

                Browser.External href ->
                    if href == "" then
                        ( model, Cmd.none )

                    else
                        ( model, Navigation.load href )

        ChangedUrl url ->
            setRoute (fromLocation url) model


type alias RemoteDataResponse =
    RemoteData (Graphql.Http.Error Response) Response


babyview : Model -> Html.Html Msg
babyview model =
    div []
        [ div []
            [ h1 [] [ text "Generated Query" ]
            , pre [] [ text (Document.serializeQuery collectionByHandleQuery) ]
            ]
        , div []
            [ h1 [] [ text "Response" ]
            , PrintAny.view model
            ]
        ]


navbarLarge : Element Msg
navbarLarge =
    row
        [ Element.width fill, Element.spacing 25, Border.solid, Border.shadow { offset = ( 1, 1 ), size = 1, blur = 1, color = lightGrey }, Border.widthXY 0 1, Border.color lightGrey ]
        [ link [ Font.color grey, Element.moveRight 240, Element.paddingXY 0 25 ] { label = navItem "Home" FeatherIcons.home, url = "/" }
        , link [ Font.color grey, Element.paddingEach { top = 25, right = 25, bottom = 25, left = 240 } ] { label = navItem "Shop" FeatherIcons.gift, url = "/" }
        , search [ Border.rounded 15, Border.color white, Background.color lightGrey, width (px 450) ]
            { onChange = Search
            , text = ""
            , placeholder = Just (placeholder [ Font.color grey ] searchTextWithIcon)
            , label = labelHidden "Search"
            }
        , el
            [ Element.alignRight ]
            (row []
                [ link [ Font.color grey, Element.paddingXY 25 25 ] { label = el [ Element.moveDown 2 ] (Element.text "Login"), url = "/" }
                , el [ paddingEach { top = 0, right = 240, bottom = 0, left = 0 } ] (button [ Border.rounded 18, Background.color grey, Font.color white, Element.paddingXY 25 8 ] { label = el [ Element.moveDown 2 ] (Element.text "Join"), onPress = Just Join })
                ]
            )
        ]


navbarSmall : Element Msg
navbarSmall =
    row
        [ Element.width fill, Element.spacing 25, Border.solid, Border.shadow { offset = ( 1, 1 ), size = 1, blur = 1, color = lightGrey }, Border.widthXY 0 1, Border.color lightGrey ]
        [ column []
            [ link [ Font.color grey, Element.moveRight 20, Element.paddingXY 0 25 ] { label = navItem "Home" FeatherIcons.home, url = "/" }
            ]
        , column []
            [ link [ Font.color grey, Element.paddingXY 10 25 ] { label = navItem "Shop" FeatherIcons.gift, url = "/" }
            ]
        , column []
            [ search [ Border.rounded 15, Border.color white, Background.color lightGrey, width (px 250) ]
                { onChange = Search
                , text = ""
                , placeholder = Just (placeholder [ Font.color grey ] searchTextWithIcon)
                , label = labelHidden "Search"
                }
            ]
        , column [ Element.alignRight ]
            [ el
                []
                (row []
                    [ link [ Font.color grey, Element.paddingXY 25 25 ] { label = el [ Element.moveDown 2 ] (Element.text "Login"), url = "/" }
                    , el [ paddingEach { top = 0, right = 25, bottom = 0, left = 0 } ] (button [ Border.rounded 18, Background.color grey, Font.color white, Element.paddingXY 25 8 ] { label = el [ Element.moveDown 2 ] (Element.text "Join"), onPress = Just Join })
                    ]
                )
            ]
        ]


searchTextWithIcon : Element Msg
searchTextWithIcon =
    row []
        [ FeatherIcons.search |> FeatherIcons.toHtml [] |> Element.html
        , el [ Element.moveRight 2 ] (Element.text "Search shop")
        ]


navItem : String -> Icon -> Element Msg
navItem label icon =
    row [] [ icon |> FeatherIcons.toHtml [] |> Element.html, el [ Element.moveRight 2, Element.moveDown 2 ] (Element.text label) ]


portraitPhone : Model -> Html.Html Msg
portraitPhone model =
    layout []
        (row [ Element.width fill ]
            [ navbarSmall
            ]
        )


portraitTablet : Model -> Html.Html Msg
portraitTablet model =
    layout []
        (column [ Element.width fill ]
            [ navbarSmall
            , hero
            , frontPageCollection model
            ]
        )


frontPageCollection : Model -> Element Msg
frontPageCollection model =
    let
        head =
            List.head model.response
    in
    row [ Element.width fill, Element.height (px 500) ]
        (case head of
            Just innerHead ->
                webDataView innerHead

            Nothing ->
                noResponseView
        )


noResponseView : List (Element Msg)
noResponseView =
    [ Element.text "No Responses yet!" ]


webDataView : RemoteDataResponse -> List (Element Msg)
webDataView response =
    case response of
        NotAsked ->
            [ Element.text "Initialising." ]

        Loading ->
            [ Element.text "Loading." ]

        Failure err ->
            [ Element.text ("Error: " ++ Debug.toString err) ]

        Success innerResponse ->
            viewResponse innerResponse


viewResponse : Response -> List (Element Msg)
viewResponse response =
    case response of
        Just collection ->
            viewCollection collection

        Nothing ->
            [ Element.text "No collection here!" ]


viewCollection : Collection -> List (Element Msg)
viewCollection collection =
    viewProducts collection.products


viewProducts : List Product -> List (Element Msg)
viewProducts products =
    List.map (\p -> viewProduct p) products


viewProduct : Product -> Element Msg
viewProduct product =
    let
        head =
            List.head product.images
    in
    Element.column []
        (List.map (\i -> viewImage (Just i)) product.images)


viewImage : Maybe Image -> Element Msg
viewImage image =
    case image of
        Just innerImage ->
            let
                (Scalar.Url urlAsString) =
                    innerImage.src
            in
            Element.image [] { description = "", src = urlAsString }

        Nothing ->
            Element.text "No Image here."


hero : Element Msg
hero =
    row [ Element.width fill, Element.height (px 500) ]
        [ Element.el [ centerX, centerY ] tentHere ]


tentHere : Element Msg
tentHere =
    tent
        |> Element.html


portraitDesktop : Model -> Html.Html Msg
portraitDesktop model =
    layout []
        (row [ Element.width fill ]
            [ navbarLarge
            ]
        )


portraitBigDesktop : Model -> Html.Html Msg
portraitBigDesktop model =
    layout []
        (row [ Element.width fill ]
            [ navbarLarge
            ]
        )


landscapePhone : Model -> Html.Html Msg
landscapePhone model =
    layout [ Element.width fill ]
        (row [ Element.width fill ]
            [ navbarSmall
            ]
        )


landscapeTablet : Model -> Html.Html Msg
landscapeTablet model =
    layout [ Element.width fill ]
        (row [ Element.width fill ]
            [ navbarSmall
            ]
        )


landscapeDesktop : Model -> Html.Html Msg
landscapeDesktop model =
    layout []
        (row [ Element.width fill ]
            [ navbarLarge
            ]
        )


landscapeBigDesktop : Model -> Html.Html Msg
landscapeBigDesktop model =
    layout []
        (row [ Element.width fill ]
            [ navbarLarge
            ]
        )


view : Model -> Browser.Document Msg
view model =
    case model.currentRoute of
        NotFoundRoute ->
            { title = "Not Found"
            , body =
                [ babyview model ]
            }

        HomeRoute ->
            case model.device.orientation of
                Portrait ->
                    case model.device.class of
                        Phone ->
                            { title = "Elm Shopify Storefront Phone"
                            , body =
                                [ portraitPhone model
                                , babyview model
                                ]
                            }

                        Tablet ->
                            { title = "Elm Shopify Storefront"
                            , body =
                                [ portraitTablet model
                                , babyview model
                                ]
                            }

                        Desktop ->
                            { title = "Elm Shopify Storefront"
                            , body =
                                [ portraitDesktop model
                                , babyview model
                                ]
                            }

                        BigDesktop ->
                            { title = "Elm Shopify Storefront"
                            , body =
                                [ portraitBigDesktop model
                                , babyview model
                                ]
                            }

                Landscape ->
                    case model.device.class of
                        Phone ->
                            { title = "Elm Shopify Storefront"
                            , body =
                                [ landscapePhone model
                                , babyview model
                                ]
                            }

                        Tablet ->
                            { title = "Elm Shopify Storefront"
                            , body =
                                [ landscapeTablet model
                                , babyview model
                                ]
                            }

                        Desktop ->
                            { title = "Elm Shopify Storefront"
                            , body =
                                [ landscapeDesktop model
                                , babyview model
                                ]
                            }

                        BigDesktop ->
                            { title = "Elm Shopify Storefront"
                            , body =
                                [ landscapeBigDesktop model
                                , babyview model
                                ]
                            }


subscriptions : Model -> Sub Msg
subscriptions model =
    onResize <|
        \width height ->
            DeviceClassified (Element.classifyDevice { width = width, height = height })


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags location key =
    let
        x =
            flags.width

        y =
            flags.height

        apiUrl =
            flags.apiUrl

        token =
            flags.token

        currentRoute =
            location |> fromUrlToRoute
    in
    setRoute
        (fromLocation location)
        { response = [ RemoteData.Loading ]
        , key = key
        , url = Just apiUrl
        , token = Just token
        , device = Element.classifyDevice { width = x, height = y }
        , currentRoute = currentRoute
        }


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case maybeRoute of
        Just HomeRoute ->
            ( model, makeRequest model.url model.token )

        Just NotFoundRoute ->
            ( model, modifyUrl model.key HomeRoute )

        Nothing ->
            ( model, modifyUrl model.key HomeRoute )


type alias Flags =
    { width : Int
    , height : Int
    , apiUrl : String
    , token : String
    }


type Route
    = HomeRoute
    | NotFoundRoute


routeParser : UrlParser.Parser (Route -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map HomeRoute top
        ]


fromLocation : Url -> Maybe Route
fromLocation location =
    case UrlParser.parse routeParser location of
        Nothing ->
            Just HomeRoute

        Just route ->
            Just route


modifyUrl : Key -> Route -> Cmd msg
modifyUrl key route =
    Navigation.pushUrl key (route |> routeToString)


routeToString : Route -> String
routeToString page =
    case page of
        HomeRoute ->
            absolute [ "home" ] []

        NotFoundRoute ->
            absolute [ "home" ] []


fromUrlToRoute : Url.Url -> Route
fromUrlToRoute url =
    case parse routeParser url of
        Nothing ->
            NotFoundRoute

        Just r ->
            r


grey : Color
grey =
    rgb255 138 138 138


lightGrey : Color
lightGrey =
    rgb255 240 240 240


white : Color
white =
    rgb255 255 255 255
