module Main (..) where

import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import StartApp
import String


type alias Model =
  { open : Bool
  , highlight : Maybe Int
  , selection : String
  , options : List ( Int, String )
  , filter : String
  }


init : ( Model, Effects Action )
init =
  ( { open = False
    , highlight = Nothing
    , selection = "Choose"
    , options =
        [ ( 1, "Choose" )
        , ( 2, "Me" )
        , ( 3, "Pikachu" )
        ]
    , filter = ""
    }
  , Effects.none
  )


type Action
  = NoOp
  | ToggleDropdown
  | Highlight Int
  | RemoveHighlight Int
  | Select String
  | Filter String
  | ChooseElement
  | HighlightTopElement
  | HighlightPrevious
  | HighlightNext


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

    ToggleDropdown ->
      let
        isSelected ( _, value ) =
          value == model.selection

        fst ( x, _ ) =
          x

        selectedId =
          List.filter isSelected model.options
            |> List.head
            |> Maybe.map fst
      in
        ( { model
            | open = not model.open
            , filter = ""
            , highlight = selectedId
          }
        , Effects.none
        )

    Highlight id ->
      ( { model | highlight = Just id }
      , Effects.none
      )

    RemoveHighlight id ->
      let
        highlight' =
          case model.highlight of
            Nothing ->
              Nothing

            Just id' ->
              if id == id' then
                Nothing
              else
                Just id'
      in
        ( { model | highlight = highlight' }
        , Effects.none
        )

    Select value ->
      ( { model
          | selection = value
          , open = False
        }
      , Effects.none
      )

    Filter filter' ->
      update HighlightTopElement { model | filter = filter' }

    ChooseElement ->
      let
        option =
          List.filter (isHighlighted model) model.options
      in
        case option of
          [ ( _, value ) ] ->
            update (Select value) model

          _ ->
            ( model, Effects.none )

    HighlightTopElement ->
      let
        option =
          List.head <| filterOptions model.filter model.options
      in
        case option of
          Nothing ->
            ( model, Effects.none )

          Just ( id, _ ) ->
            update (Highlight id) model

    HighlightPrevious ->
      let
        previous =
          getPrevious (isHighlighted model) model.options
      in
        case previous of
          Just ( id, _ ) ->
            ( { model | highlight = Just id }, Effects.none )

          Nothing ->
            ( model, Effects.none )

    HighlightNext ->
      let
        next =
          getPrevious (isHighlighted model) <| List.reverse model.options
      in
        case next of
          Just ( id, _ ) ->
            ( { model | highlight = Just id }, Effects.none )

          Nothing ->
            ( model, Effects.none )


isHighlighted : Model -> ( Int, String ) -> Bool
isHighlighted model ( id, _ ) =
  case model.highlight of
    Nothing ->
      False

    Just id' ->
      id == id'


getPrevious : (a -> Bool) -> List a -> Maybe a
getPrevious p xs =
  case xs of
    [] ->
      Nothing

    [ x ] ->
      Nothing

    x :: x' :: xs ->
      if p x' then
        Just x
      else
        getPrevious p (x' :: xs)


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ class "main-content" ]
    [ div
        [ class "page-content"
        , style [ ( "overflow", "visible" ) ]
        ]
        [ div
            [ class "col-md-6 portlets" ]
            [ div
                [ class "panel" ]
                [ div
                    [ class "panel-content" ]
                    [ div
                        [ class "form-group" ]
                        [ single address model ]
                    ]
                ]
            ]
        ]
    ]


filterOptions : String -> List ( Int, String ) -> List ( Int, String )
filterOptions filter options =
  let
    contains filter ( _, value ) =
      String.contains (String.toLower filter) <| String.toLower value
  in
    List.filter (contains filter) options


single : Signal.Address Action -> Model -> Html
single address model =
  let
    open =
      model.open

    options =
      filterOptions model.filter model.options

    openClass =
      if open then
        " select2-container--open"
      else
        ""

    makeOption ( id, value ) =
      let
        activeClass =
          Maybe.map (\id' -> id' == id) model.highlight
            |> Maybe.withDefault False

        chosenClass =
          model.selection == value

        ariaSelected =
          String.toLower <| toString chosenClass
      in
        li
          [ classList
              [ ( "select2-results__option", True )
              , ( "select2-results__option--highlighted", activeClass )
              ]
          , style
              [ ( "cursor", "pointer" )
              , ( "padding", "4px" )
              ]
          , attribute "aria-selected" ariaSelected
          , on "mouseover" Json.value (\_ -> Signal.message address (Highlight id))
          , on "mouseout" Json.value (\_ -> Signal.message address (RemoveHighlight id))
          , onClick address (Select value)
          ]
          [ text value ]

    optionsList =
      List.map makeOption options
  in
    span
      [ class <| "select2 select2-container select2-container--default" ++ openClass
      , style [ ( "width", "100%" ) ]
      ]
      [ span
          [ class "selection" ]
          [ span
              [ class "select2-selection select2-selection--single"
              , onClick address ToggleDropdown
              ]
              [ span
                  [ class "select2-selection__rendered" ]
                  [ text model.selection ]
              , span
                  [ class "select2-selection__arrow"
                  ]
                  [ b [] [] ]
              ]
          ]
      , span
          [ class <| "select2-container select2-container--default" ++ openClass
          , style
              [ ( "position", "absolute" )
              , ( "top", "25px" )
              , ( "width", "100%" )
              ]
          ]
          [ span
              [ class "select2-dropdown select2-dropdown--below"
              ]
              [ input
                  [ class "select2-search__field"
                  , style
                      [ ( "margin", "4px" )
                      , ( "width", "calc(100% - 8px)" )
                      , ( "box-sizing", "border-box" )
                      ]
                  , on "input" targetValue (Signal.message address << Filter)
                  , onKey address NoOp
                  , value model.filter
                  ]
                  []
              , span
                  [ class "select2-results"
                  , style
                      [ ( "margin", "0px" )
                      , ( "padding", "0px" )
                      ]
                  ]
                  [ ul
                      [ class "select2-results__options" ]
                      optionsList
                  ]
              ]
          ]
      ]


onKey : Signal.Address Action -> Action -> Attribute
onKey address noop =
  let
    message code =
      case code of
        13 ->
          Signal.message address ChooseElement

        38 ->
          Signal.message address HighlightPrevious

        40 ->
          Signal.message address HighlightNext

        _ ->
          Signal.message address noop
  in
    on
      "keydown"
      keyCode
      message


app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }


model : Signal Model
model =
  app.model


main : Signal Html
main =
  app.html
