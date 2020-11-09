module Main exposing (changer, main, update, view)
import Playground exposing (..)



-- Main --

main =
    game view update initModel


-- Types --


type alias Model =
    { x : Number, y : Number, angle : Number, garbageCan : GarbageCan, garbageCan2 : GarbageCan2 }

type GarbageCanStatus
    = FullOfGarbage
    | Emptied


type alias GarbageCan
  = { x : Number, y : Number, status: GarbageCanStatus }

type alias GarbageCan2
  = { x : Number, y : Number, status: GarbageCanStatus }

  -- Model --

initModel : Model
initModel =
    { x = 0
    , y = 0
    , angle = 0
    , garbageCan = position1
    , garbageCan2 = position2
    }


position1 =

  { x = 600, y = 20, status = FullOfGarbage }

position2 =

  { x = 300, y = -300, status = FullOfGarbage }


-- View --


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
    , garbageCanView model.garbageCan2.status model.garbageCan2.x model.garbageCan2.y
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



-- Update --


update : Computer -> Model -> Model
update computer model =
    let
        updatedX =
            model.x + toY computer.keyboard * 4 * cos (degrees model.angle)
        updatedY =
            model.y + toY computer.keyboard * 4 * sin (degrees model.angle)
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
        updatedGarbageCan2 =
            updateGarbageCan2 model
    in
    { model
        | x = updatedX
        , y = updatedY
        , angle = Debug.log "HI" updatedAngle
        , garbageCan = updatedGarbageCan
        , garbageCan2 = updatedGarbageCan2
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


updateGarbageCan2 : Model -> GarbageCan2
updateGarbageCan2 model =
    let
        existingGC = model.garbageCan2
        updatedStatus =

          case model.garbageCan2.status of
            Emptied ->
              Emptied
            FullOfGarbage ->
              if changer model model.garbageCan2.x model.garbageCan2.y then
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
