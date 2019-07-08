module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation exposing (Key)
import Element exposing (Element, alignRight, centerY, el, fill, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Graphql.Document as Document
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, button, div, h1, p, pre, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import PrintAny
import RemoteData exposing (RemoteData, WebData)
import ShopifyApi.Object
import ShopifyApi.Object.Collection as Collection
import ShopifyApi.Object.CollectionConnection as CollectionConnection
import ShopifyApi.Object.CollectionEdge as CollectionEdge
import ShopifyApi.Object.PageInfo as PageInfo
import ShopifyApi.Query as Query
import ShopifyApi.ScalarCodecs as ScalarCodecs exposing (Id)
import Url exposing (Url)
import Url.Builder as UrlBuilder exposing (absolute)
import Url.Parser as UrlParser exposing (..)


type alias Response =
    Paginator (List Collection)


type alias Paginator dataType =
    { data : dataType
    , paginationData : PaginationData
    }


type alias PaginationData =
    { hasNextPage : Bool
    , hasPreviousPage : Bool
    }


collectionSearchSelection : SelectionSet Response ShopifyApi.Object.CollectionConnection
collectionSearchSelection =
    SelectionSet.succeed Paginator
        |> with searchResultFieldEdges
        |> with (CollectionConnection.pageInfo searchPageInfoSelection)


searchPageInfoSelection : SelectionSet PaginationData ShopifyApi.Object.PageInfo
searchPageInfoSelection =
    SelectionSet.succeed PaginationData
        |> with PageInfo.hasNextPage
        |> with PageInfo.hasPreviousPage


searchResultFieldEdges : SelectionSet (List Collection) ShopifyApi.Object.CollectionConnection
searchResultFieldEdges =
    CollectionConnection.edges
        (CollectionEdge.node collectionSelection)


collectionSelection : SelectionSet Collection ShopifyApi.Object.Collection
collectionSelection =
    SelectionSet.succeed Collection
        |> with Collection.id
        |> with Collection.handle


query : SelectionSet Response RootQuery
query =
    Query.collections
        (\optionals ->
            { optionals
                | first = Present 5
            }
        )
        collectionSearchSelection


makeRequest : Maybe String -> Maybe String -> Cmd Msg
makeRequest url token =
    case url of
        Just innerUrl ->
            case token of
                Just innerToken ->
                    query
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
    }


type Msg
    = GotResponse RemoteDataResponse
    | GetNextPage
    | ClickedLink UrlRequest
    | ChangedUrl Url


main : Program Decode.Value Model Msg
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
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetNextPage ->
            case model.response of
                (RemoteData.Success successResponse) :: rest ->
                    if successResponse.paginationData.hasNextPage then
                        ( { model | response = RemoteData.Loading :: model.response }, makeRequest model.url model.token )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

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
            , pre [] [ text (Document.serializeQuery query) ]
            ]
        , div [] [ button [ onClick GetNextPage ] [ text "Load next page..." ] ]
        , div []
            [ h1 [] [ text "Response" ]
            , PrintAny.view model
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Shopify Storefront"
    , body =
        [ babyview model ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : Decode.Value -> Url -> Key -> ( Model, Cmd Msg )
init val location key =
    setRoute
        (fromLocation location)
        { response = [ RemoteData.Loading ]
        , key = key
        , url = decodeUrlFromJson val
        , token = decodeUrlFromJson val
        }


decodeUrlFromJson : Decode.Value -> Maybe String
decodeUrlFromJson json =
    json
        |> Decode.decodeValue (Decode.field "url" Decode.string)
        |> Result.toMaybe


decodeKeyFromJson : Decode.Value -> Maybe String
decodeKeyFromJson json =
    json
        |> Decode.decodeValue (Decode.field "token" Decode.string)
        |> Result.toMaybe


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
    ()


type Route
    = HomeRoute
    | NotFoundRoute


routeParser : UrlParser.Parser (Route -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map HomeRoute (UrlParser.s "home") ]


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
