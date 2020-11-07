module Main exposing (changer, main, update, view)
import Playground exposing (..)

main =
    game view update initModel
type alias Model =
    { x : Number, y : Number, angle : Number, garbageCan : GarbageCan }
type GarbageCanStatus
    = FullOfGarbage
    | Emptied

type alias GarbageCan
  = { x : Number, y : Number, status: GarbageCanStatus }

initModel : Model
initModel =
    { x = 0
    , y = 0
    , angle = 25
    , garbageCan = { x = 600, y = 20, status = FullOfGarbage }
    }


view : Computer -> Model -> List Shape
view computer model =
    [ rectangle blue computer.screen.width computer.screen.height
    , image 1400 1400 "images/Road.png"
        |> moveRight 1
        |> moveDown 1
    , garbageTruckView model
        |> moveRight 1
        |> moveDown 200
    , garbageCanView model.garbageCan.status model.garbageCan.x model.garbageCan.y
    ]

garbageCanView status x y =
    case status of
        FullOfGarbage ->
            image 110 110 "images/fullGarbage.jpg"
                |> moveRight x
                |> moveDown y
        Emptied ->
            image 110 100 ("images/emptyGarbage.jpg")
                |> moveRight x
                |> moveDown y


garbageTruckView model =
        if model.angle > 75 then
            image 350 350 "images/GarbageTruckLeft0.gif"
              |> move model.x model.y
              |> rotate (model.angle - 180)
        else if model.angle > 90 then
            image 350 350 "images/GarbageTruckRight0.gif"
                |> move model.x model.y
                |> rotate (model.angle - 180)

        else
            image 350 350 ("images/GarbageTruckRight0.gif")
                |> move model.x model.y
                |> rotate model.angle



update : Computer -> Model -> Model
update computer model =
    let
        updatedX =
            model.x + toY computer.keyboard * cos (degrees model.angle)
        updatedY =
            model.y + toY computer.keyboard * sin (degrees model.angle)
        updatedAngle =
          if model.angle > -1 && model.angle < 180  then
            model.angle - toX computer.keyboard
          else if model.angle == -1 && computer.keyboard.left then
            model.angle + 1
          else if model.angle == 180 && computer.keyboard.right then
            model.angle - 1
          else
            model.angle
        updatedGarbageCan =
            updateGarbageCan model
    in
    { model
        | x = updatedX
        , y = updatedY
        , angle = Debug.log "HI" updatedAngle
        , garbageCan = updatedGarbageCan
    }


updateGarbageCan : Model -> GarbageCan
updateGarbageCan model =
    let
        existingGC = model.garbageCan
        updatedStatus =

          case model.garbageCan.status of
            Emptied ->
              Emptied
            FullOfGarbage ->
              if changer model model.garbageCan.x model.garbageCan.y then
                Emptied
              else
                FullOfGarbage

    in
    { existingGC | status = updatedStatus}


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
