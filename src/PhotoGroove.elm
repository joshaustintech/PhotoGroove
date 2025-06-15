module PhotoGroove exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser

type alias Model =
    { photos : List Photo
    , selectedPhoto : Photo
    }

type alias Photo =
    { url : String }

type Msg =
    ClickedPhoto Photo

initialModel: Model
initialModel =
    {
        photos =
        [ { url = "https://picsum.photos/id/11/2500/1667" }
        , { url = "https://picsum.photos/id/23/3887/4899" }
        , { url = "https://picsum.photos/id/36/4179/2790" }
        ]
    , selectedPhoto = { url = "https://picsum.photos/id/11/2500/1667" }
    }

update: Msg -> Model -> Model
update msg model =
    case msg of
        ClickedPhoto photo -> { model | selectedPhoto = photo }

view: Model -> Html Msg
view model =
    div [ class "container-md" ]
    [ h1 [] [ text "Photo Groove" ]
    , div [ class "row" ] [
        img
        [ class "img-thumbnail"
        , style "max-height" "600px"
        , style "width" "auto"
        , src model.selectedPhoto.url
        ]
        []
    ]
    , div [ class "row" ]
        (List.map (viewThumbnail model.selectedPhoto) model.photos)
    ]
viewThumbnail : Photo -> Photo -> Html Msg
viewThumbnail selectedThumb thumb =
    if selectedThumb.url == thumb.url then
        div []
            [ img [ class "border border-primary img-thumbnail float-left"
                  , style "max-height" "200px"
                  , style "width" "auto"
                  , src thumb.url, alt "Selected Photo"
                  ] []
            ]
    else
        div []
            [ img
                [ class "img-thumbnail float-left"
                , style "max-height" "200px"
                , src thumb.url
                , alt "Photo Thumbnail"
                , onClick (ClickedPhoto thumb)
                ] []
            ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }