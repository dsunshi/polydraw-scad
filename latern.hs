
import Graphics.Polydraw
import Linear.V3

pedestal :: Model3d
pedestal = stack [
        pyramid 18 14  6,
        pyramid 11 8.5 12,
        peg
    ]
    where
        ledD    = 5
        ledH    = 3.2
        sHeight = 2.5
        lbSize  = 11
        lbWinSize = 7.5
        ---
        peg = box s s boxH
        s = ledD / sqrt 2 * 0.9
        boxH = sHeight - (ledH - ((lbSize - lbWinSize) / 2))

lightbox :: Model3d
lightbox = union [
        base,
        up sHeight lb,
        translate (V3 pegOffset pegOffset (sHeight + lbSize)) capPeg,
        translate (V3 (-pegOffset) (-pegOffset) (sHeight + lbSize)) capPeg
    ]
    where
        lb = difference (pyramid lbSize1 lbSize2 lbSize) [
                up z hole,
                up z $ rotate (V3 0 0 90) hole,
                up z $ box lbWinSize lbWinSize (lbSize * 1.5),
                cylinder ledH (d ledD) (fn 33)
            ]
        sSize     = 16
        sHeight   = 2.5
        lbSize1   = 11
        lbSize2   = 13
        lbSize    = lbSize1
        lbWinSize = 7.5
        ---
        ledD    = 5
        ledH    = 3.2
        ---
        hole = translate (V3 (-lbSize * 0.75) 0 0)
            $ up (lbWinSize / 2)
            $ rotate (V3 0 90 0)
            $ cylinder (lbSize * 1.5) (d lbWinSize) (fn 33)
        base = difference (pyramid sSize sSize sHeight) [ peg ]
        peg = box s s sHeight
        capPegS = 1
        capPeg = cube capPegS
        pegOffset = lbWinSize / 2 + capPegS / 2
        s = ledD / sqrt 2
        z = (lbSize - lbWinSize) / 2

cap :: Model3d
cap = difference (stack [
        pyramid 22 9   2.75,
        pyramid 7  5.5 2,
        pyramid 4  2.5 2
    ]) [
        translate (V3   pegOffset    pegOffset  0) capPeg,
        translate (V3 (-pegOffset) (-pegOffset) 0) capPeg
      ]
      where
        capPegS = 1.5
        capPeg = cube capPegS
        pegOffset = lbWinSize / 2 + capPegS / 2
        lbWinSize = 7.5

scratchpad :: Model3d
scratchpad = polyhedron [V3 0 0 0, V3 1 1 1] [[0], [1]] 3

main :: IO ()
main = do
    write "pedestal.scad"   pedestal
    write "cap.scad"        cap
    write "lightbox.scad"   lightbox
    write "scratchpad.scad" scratchpad
