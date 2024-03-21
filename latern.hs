
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
        s = ledD / sqrt 2
        boxH = sHeight - (ledH - ((lbSize - lbWinSize) / 2))

lightbox :: Model3d
lightbox = union [
        base,
        up sHeight lb
    ]
    where
        lb = difference (pyramid lbSize1 lbSize2 lbSize) [
                up z $ box lbWinSize     (lbSize * 1.5) lbWinSize,
                up z $ box (lbSize * 1.5) lbWinSize     lbWinSize,
                up z $ box lbWinSize      lbWinSize    (lbSize * 1.5),
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
        base = difference (pyramid sSize sSize sHeight) [ peg ]
        peg = box s s sHeight
        s = ledD / sqrt 2
        z = (lbSize - lbWinSize) / 2

cap :: Model3d
cap = stack [
        pyramid 22 9   2.75,
        pyramid 7  5.5 2,
        pyramid 4  2.5 2
    ]

scratchpad :: Model3d
scratchpad = up 5 $ rotate (V3 0 90 0) $ cylinder 10 (d 5) (fn 33)

main :: IO ()
main = do
    write "pedestal.scad"   pedestal
    write "cap.scad"        cap
    write "lightbox.scad"   lightbox
    write "scratchpad.scad" scratchpad
