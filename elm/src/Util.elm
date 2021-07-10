module Util exposing (choose)


choose : Bool -> c -> c -> c
choose b x y =
    if b then
        x

    else
        y
