
include <BOSL2/std.scad>


PIN_RES = 8;
HOL_RES = 33;
GAP = 0.07;

// Base
bSize1 = 18;
bSize2 = 14;
bHeight = 6;

// LED
ledD = 5;
ledH = 3.2;


// Pedestal portion of base
pSize1 = 11; // This is on top of bSize2
pSize2 = 8.5;
pHeight = 12;

// Stand for the base/pedestal pair
sSize = 16;
sHeight = 2.5;

// Lightbox
lbSize1 = 11;
lbSize2 = 13;
lbSize = lbSize1;
lbWinSize = 7.5;

pinD = ledD;
pegS = 1;

// Cap
cbSize1 = 22;
cbSize2 = 9;
cbHeight = 2.75;

cp1Size1 = 7;
cp1Size2 = 5.5;
cp1Height = 2;

cp2Size1 = 4;
cp2Size2 = 2.5;
cp2Height = 2;

module pedestal() {
    prismoid(size1 = [bSize1, bSize1],
             size2 = [bSize2, bSize2],
             h = bHeight);

    translate([0, 0, bHeight])
        prismoid(size1 = [pSize1, pSize1],
                 size2 = [pSize2, pSize2],
                 h = pHeight);
    
    s = (pinD - GAP) / 1.41421356;
    translate([-s/2, -s/2, bHeight + pHeight])
        cube([s, s, sHeight - (ledH - ((lbSize - lbWinSize)/2))]);
}



module lightbox() {
    union() {
    difference() {
        x = (lbSize - lbWinSize) / 2;
        
        translate([0, 0, bHeight + pHeight])
            prismoid(size1 = [sSize, sSize],
                     size2 = [sSize, sSize],
                     h = sHeight);

        s = (pinD + GAP) / 1.41421356;
        translate([-s/2, -s/2, bHeight + pHeight])
            cube([s, s, sHeight]);
        
        #translate([-lbSize/2,
                    -lbSize/2,
                    bHeight + pHeight + sHeight])
            translate([lbSize/2,
                       lbSize/2,
                       x - ledH + 0.001])
                cylinder(h = ledH, d = ledD, $fn = HOL_RES);
    }



    translate([-lbSize/2,
               -lbSize/2,
                bHeight + pHeight + sHeight])
        difference() {
            translate([lbSize/2, lbSize/2, 0])
                prismoid(size1 = [lbSize1, lbSize1],
                         size2 = [lbSize2, lbSize2],
                         h = lbSize);

            x = (lbSize - lbWinSize) / 2;
            y = -lbSize / 4;
            z = x;
            
            translate([x, y, z]) cube([lbWinSize, lbSize * 1.5, lbWinSize]);
            translate([y, x, z]) cube([lbSize * 1.5, lbWinSize, lbWinSize]);
            translate([z, z, z]) cube([lbWinSize, lbWinSize, lbSize * 1.5]);
            
            translate([lbSize/2,
                       lbSize/2,
                       x - ledH + 0.001])
                cylinder(h = ledH, d = ledD, $fn = HOL_RES);
    }
    
    translate([lbSize/2 - pegS/2,
               lbSize/2 - pegS/2,
               bHeight + pHeight + sHeight + lbSize])
        cube([pegS - GAP, pegS - GAP, pegS - GAP]);
    translate([-(lbSize/2 + pegS/2),
               -(lbSize/2 + pegS/2),
               bHeight + pHeight + sHeight + lbSize])
        cube([pegS - GAP, pegS - GAP, pegS - GAP]);
}
}



module cap() {
    difference() {
        union() {
            translate([0,
                       0,
                       bHeight + pHeight + sHeight + lbSize])
                prismoid(size1 = [cbSize1, cbSize1],
                         size2 = [cbSize2, cbSize2],
                         h = cbHeight);
            
            translate([0,
                       0,
                       bHeight + pHeight + sHeight + lbSize + cbHeight])
                prismoid(size1 = [cp1Size1, cp1Size1],
                         size2 = [cp1Size2, cp1Size2],
                         h = cp1Height);
            
            translate([0,
                       0,
                       bHeight + pHeight + sHeight + lbSize + cbHeight + cp1Height])
                prismoid(size1 = [cp2Size1, cp2Size1],
                         size2 = [cp2Size2, cp2Size2],
                         h = cp2Height);
        }
        translate([lbSize/2 - pegS/2,
               lbSize/2 - pegS/2,
               bHeight + pHeight + sHeight + lbSize])
            cube([pegS + GAP, pegS + GAP, pegS]);
        translate([-(lbSize/2 + pegS/2),
               -(lbSize/2 + pegS/2),
               bHeight + pHeight + sHeight + lbSize])
            cube([pegS + GAP, pegS + GAP, pegS]);
    }
}

// Drawn together
//pedestal();
//lightbox();
//cap();

// Drawn seperate
pedestal();

// lightbox pt. 1
difference() {
    s = sSize * 1.5;
    h = sHeight + (lbSize - lbWinSize)/2 + lbWinSize;
translate([bSize1 * 2, 0, -(bHeight + pHeight)]) lightbox();
translate([bSize1 * 2 - s/2, -s/2, h]) cube([s, s, h]);
}
s = sSize * 1.5;
    translate([bSize1 * 2, 0, -(bHeight + pHeight + (lbSize - lbWinSize)/2)]) translate([lbSize/2 - pegS/2,
               lbSize/2 - pegS/2,
               bHeight + pHeight + sHeight + lbSize])
        cube([pegS - GAP, pegS - GAP, pegS - GAP]);
    translate([bSize1 * 2, 0, -(bHeight + pHeight + (lbSize - lbWinSize)/2)]) translate([-(lbSize/2 + pegS/2),
               -(lbSize/2 + pegS/2),
               bHeight + pHeight + sHeight + lbSize])
        cube([pegS - GAP, pegS - GAP, pegS - GAP]);

// lightbox pt. 2
translate([0,0,-(sHeight + (lbSize - lbWinSize)/2 + lbWinSize)]) difference() {
    translate([bSize1 * 2, bSize1 * 2, -(bHeight + pHeight)]) lightbox();
    s = sSize * 1.5;
    h = sHeight + (lbSize - lbWinSize)/2 + lbWinSize;
    translate([bSize1 * 2 - s/2, bSize1 * 2 - s/2, 0]) cube([s, s, h]);
    #translate([bSize1 * 2, bSize1 * 2, -(bHeight + pHeight + (lbSize - lbWinSize)/2)]) translate([lbSize/2 - pegS/2,
               lbSize/2 - pegS/2,
               bHeight + pHeight + sHeight + lbSize])
            cube([pegS + GAP, pegS + GAP, pegS]);
        # translate([bSize1 * 2, bSize1 * 2, -(bHeight + pHeight + (lbSize - lbWinSize)/2)])translate([-(lbSize/2 + pegS/2),
               -(lbSize/2 + pegS/2),
               bHeight + pHeight + sHeight + lbSize])
            cube([pegS + GAP, pegS + GAP, pegS]);
}

// cap
translate([0, bSize1 * 2, -(bHeight + pHeight + sHeight + lbSize)]) cap();

windowT = 0.3;
ledHeight = 5;

translate([-(bSize1 * 2), 0, 0]) union() {
    difference() {
        cube(lbWinSize);
        translate([windowT, windowT, windowT]) cube([lbWinSize - windowT * 2, lbWinSize - windowT * 2, lbWinSize]);
    }
    translate([lbWinSize/2, lbWinSize/2, 0]) cylinder(h = lbWinSize - ledHeight + ledH, d = ledD, $fn = PIN_RES);
}