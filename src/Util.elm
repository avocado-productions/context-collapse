module Util exposing (choose)


choose : c -> c -> Bool -> c
choose x y b =
    if b then
        x

    else
        y
