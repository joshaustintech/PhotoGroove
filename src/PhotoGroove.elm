module PhotoGroove exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser

type ThumbnailSize = Small
                   | Medium
                   | Large

type alias Model =
    { photos : List Photo
    , selectedPhoto : Maybe Photo
    , thumbnailSize : ThumbnailSize
    }

type alias Photo =
    { url : String }

type Msg = ClickedPhoto Photo
         | ClickedSize ThumbnailSize

initialModel: Model
initialModel =
    {
        photos =
        [ { url = "https://picsum.photos/id/11/2500/1667" }
        , { url = "https://picsum.photos/id/23/3887/4899" }
        , { url = "https://picsum.photos/id/36/4179/2790" }
        ]
    , selectedPhoto = Nothing
    , thumbnailSize = Medium
    }

update: Msg -> Model -> Model
update msg model =
    case msg of
        ClickedPhoto photo -> { model | selectedPhoto = Just photo }
        ClickedSize size -> { model | thumbnailSize = size }

view: Model -> Html Msg
view model =
    div [ class "container-md mt-4" ]
    [ h1 [ class "display-1" ] [ text "Photo Groove" ]
    , div [ class "row" ]
        (List.map (viewSizeChooser model) [Small, Medium, Large])
    , div [ class "row" ] [
        viewSelectedPhoto model
    ]
    , div [ class "row gx-3" ]
        (List.map (viewThumbnail model.selectedPhoto model.thumbnailSize) model.photos)
    ]

viewSizeChooser : Model -> ThumbnailSize -> Html Msg
viewSizeChooser model size =
    let
        sizeText = sizeToString size
    in
    div [ class "form-check" ]
    [ input [ type_ "radio"
            , id ("radio-size-" ++ sizeText)
            , class "form-check-input"
            , name "size"
            , checked (model.thumbnailSize == size)
            , onClick (ClickedSize size) ] []
    , label [ class "form-check-label"
            , for ("radio-size-" ++ sizeText) ] [ text sizeText ]
    ]

sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small -> "small"
        Medium -> "medium"
        Large -> "large"

viewSelectedPhoto : Model -> Html Msg
viewSelectedPhoto model =
    case model.selectedPhoto of
        Just p ->
            img
            [ class "img-thumbnail"
            , style "max-height" "600px"
            , style "width" "auto"
            , src p.url
            ]
            []
        Nothing ->
            Html.text ""

viewThumbnail : Maybe Photo -> ThumbnailSize -> Photo -> Html Msg
viewThumbnail selectedThumb size thumb =
    case selectedThumb of
        Just t ->
            if t.url == thumb.url then
                viewSelectedThumb thumb size
            else
                viewUnselectedThumb thumb size
        Nothing ->
                viewUnselectedThumb thumb size

viewSelectedThumb : Photo -> ThumbnailSize -> Html Msg
viewSelectedThumb thumb size =
    renderBaseThumb thumb size True

viewUnselectedThumb : Photo -> ThumbnailSize -> Html Msg
viewUnselectedThumb thumb size =
    renderBaseThumb thumb size False

renderBaseThumb : Photo -> ThumbnailSize -> Bool -> Html Msg
renderBaseThumb thumb size isSelected =
    let
        styles = "img-thumbnail m-1 mt-3 " ++ case size of
            Small -> "col-sm-1"
            Medium -> "col-sm-2"
            Large -> "col-sm-4"
        maxHeight = case size of
            Small -> "100px"
            Medium -> "150px"
            Large -> "200px"
    in
    img [ class (if isSelected then "border border-primary " ++ styles else styles)
            , style "max-height" maxHeight
            , style "width" "auto"
            , src thumb.url
            , alt "Photo Thumbnail"
            , onClick (ClickedPhoto thumb)
            ] []

main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }