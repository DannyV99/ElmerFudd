module Main exposing (changer, main, update, view)
import Playground exposing (..)

main =
    game view update initModel
type alias Model =
    { x : Number, y : Number, angle : Number, garbageCanStatus : GarbageCanStatus }
type GarbageCanStatus
    = FullOfGarbage
    | Emptied
initModel : Model
initModel =
    { x = 0
    , y = 0
    , angle = 0
    , garbageCanStatus = FullOfGarbage
    }
view : Computer -> Model -> List Shape
view computer model =
    [ rectangle blue computer.screen.width computer.screen.height
    , image 300 300 "images/GarbageTruck.gif"
        |> move model.x model.y
        |> rotate model.angle
    , garbageCanView model
    ]
garbageCanView model =
    case model.garbageCanStatus of
        FullOfGarbage ->
            image 90 90 "images/fullGarbage.jpg"
                |> moveRight 300
                |> moveDown 20
        Emptied ->
            image 90 90 ("images/emptyGarbage" ++ ".jpg")
                |> moveRight 300
                |> moveDown 20
update : Computer -> Model -> Model
update computer model =
    let
        updatedX =
            model.x + toY computer.keyboard * cos (degrees model.angle)
        updatedY =
            model.y + toY computer.keyboard * sin (degrees model.angle)
        updatedAngle =
            model.angle - toX computer.keyboard
        updatedGarbageCanStatus =
            updateGarbageCanStatus model
    in
    { model
        | x = updatedX
        , y = updatedY
        , angle = updatedAngle
        , garbageCanStatus = updatedGarbageCanStatus
    }
updateGarbageCanStatus model =
    case model.garbageCanStatus of
        Emptied ->
            Emptied
        FullOfGarbage ->
            if changer model 300 20 then
                Emptied
            else
                FullOfGarbage
changer : Model -> Number -> Number -> Bool
changer truck x y =
    let
        buffer =
            150
    in
    [ (truck.x - buffer) < x
    , x < (truck.x + buffer)
    , (truck.y - buffer) < y
    , y < (truck.y + buffer)
    ]
        |> List.all identity
