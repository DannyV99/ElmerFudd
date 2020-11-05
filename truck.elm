import Playground exposing (..)


main =
  game view update
    { x = 0
    , y = 0
    , angle = 0
    }

view computer truck =

  [ rectangle blue computer.screen.width computer.screen.height
  , image 300 300 "images/GarbageTuck.gif"
      |> move truck.x truck.y
      |> rotate truck.angle

  , image 90 90 (changer truck 300)
        |> moveRight 300
        |> moveDown 20
  ]


changer truck x =
  if truck.x > x then
    "images/emptyGarbage" ++ ".jpg"
  else
    "images/fullGarbage" ++ ".jpg"


update computer truck =
  { x = truck.x + toY computer.keyboard * cos (degrees truck.angle)
  , y = truck.y + toY computer.keyboard * sin (degrees truck.angle)
  , angle = truck.angle - toX computer.keyboard
  }
