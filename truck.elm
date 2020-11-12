module Main exposing (changer, main, update, view)

import Playground exposing (..)


-- Main --


main =
    game view update initModel



-- Types --


type alias Model =
    { x : Number, y : Number, angle : Number, garbageCans : List GarbageCan, levelUp : Shape }


type GarbageCanStatus
    = FullOfGarbage
    | Emptied


type alias GarbageCan =
    { x : Number, y : Number, status : GarbageCanStatus }


type alias GarbageCan2 =
    { x : Number, y : Number, status : GarbageCanStatus }




-- Model --


initModel : Model
initModel =
    { x = -400
    , y = -250
    , angle = 0
    , garbageCans = initGarbageCans
    , levelUp = levelUpImage 1 1
    }


initGarbageCans =
    [ position1, position2 ]


position1 =
    { x = -100, y = 20, status = FullOfGarbage }


position2 =
    { x = 300, y = -300, status = FullOfGarbage }

levelUpImage x y =
  image 110 110 "images/nextLevel.jpg"
      |> move x y

-- View --


view : Computer -> Model -> List Shape
view computer model =
    [ rectangle blue computer.screen.width computer.screen.height
    , garbageTruckView model
    ]
        ++ garbageCansView model.garbageCans


garbageCansView : List GarbageCan -> List Shape
garbageCansView gCs =
    List.map garbageCanView gCs


garbageCanView gC =
    let
        status =
            gC.status

        x =
            gC.x

        y =
            gC.y
    in
    case status of
        FullOfGarbage ->
            image 110 110 "images/fullGarbage.jpg"
                |> move x y

        Emptied ->
            image 110 100 "images/emptyGarbage.jpg"
                |> move x y


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
        image 350 350 "images/GarbageTruckRight0.gif"
            |> move model.x model.y
            |> rotate model.angle



-- Update --


isEmpty : GarbageCan -> Bool
isEmpty gC =
    case gC.status of
        FullOfGarbage ->
            False

        Emptied ->
            True




update : Computer -> Model -> Model
update computer model =
    let
        advanceToNextLevel =
            List.all isEmpty model.garbageCans

        ( updatedX, updatedY ) =
            updateXY computer ( model.x, model.y ) model.angle advanceToNextLevel

        updatedAngle =
            updateAngle computer model

        updatedGarbageCans =
            updateGarbageCans ( model.x, model.y ) advanceToNextLevel model.garbageCans model.angle
    in
    { model
        | x = updatedX
        , y = updatedY
        , angle = updatedAngle
        , garbageCans = updatedGarbageCans
    }


fill : GarbageCan -> GarbageCan
fill gC =
    { gC | status = FullOfGarbage }



updateGarbageCans : ( Number, Number ) -> Bool -> List GarbageCan -> Number -> List GarbageCan
updateGarbageCans truckPosition advance gCs angle =
    if advance then
        let
            newGCs =
                List.map fill gCs

            newCan =
                case List.head gCs of
                    Just headGC ->
                        { status = FullOfGarbage, x = headGC.x - (angle), y = headGC.y + ( 50 ) }

                    Nothing ->
                        position2
        in
        newCan :: newGCs

    else
        List.map (updateGarbageCan truckPosition) gCs




updateAngle computer model =
    if model.angle > -1 && model.angle < 180 then
        model.angle - toX computer.keyboard

    else if model.angle == -1 && computer.keyboard.left then
        model.angle + 1

    else if model.angle == 180 && computer.keyboard.right then
        model.angle - 1

    else
        model.angle


updateXY computer ( truckX, truckY ) angle advanceToNextLevel =
    if advanceToNextLevel then
        ( -300, -400 )

    else
        let
            updatedX =
                truckX + toY computer.keyboard * 6 * cos (degrees angle)

            updatedY =
                truckY + toY computer.keyboard * 6 * sin (degrees angle)
        in
        ( updatedX, updatedY )


updateGarbageCan : ( Number, Number ) -> GarbageCan -> GarbageCan
updateGarbageCan truckPosition existingGC =
    let
        updatedStatus =
            case existingGC.status of
                Emptied ->
                    Emptied

                FullOfGarbage ->
                    if changer truckPosition existingGC.x existingGC.y then
                        Emptied

                    else
                        FullOfGarbage
    in
    { existingGC | status = updatedStatus }


changer : ( Number, Number ) -> Number -> Number -> Bool
changer ( truckX, truckY ) x y =
    let
        buffer =
            150
    in
    [ (truckX - buffer) < x
    , x < (truckX + buffer)
    , (truckY - buffer) < y
    , y < (truckY + buffer)
    ]
        |> List.all identity
