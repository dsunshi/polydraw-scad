
import Graphics.Polydraw

pedestal = union [
    pyramid bSize1 bSize2 bHeight,
    up bHeight             $ pyramid pSize1 pSize2 pHeight,
    up (bHeight + pHeight) $ box s s (9)
    ]
    where
        bSize1  = 18
        bSize2  = 14
        bHeight = 6
        pSize1  = 11
        pSize2  = 8.5
        pHeight = 12
        s = 1 / (sqrt 2)

