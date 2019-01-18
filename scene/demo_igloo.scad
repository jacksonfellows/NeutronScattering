//units: inches. 


module waxblock1() {
   difference(){
        cube([20,9.85,9.8]);
        translate([-2,8,6]) {
               cube([5,5,5]);}
        }
        }
module waxblock2() {
    translate([0,9.85,0]) {
        rotate(a = 90, v = [10,0, 0])
        {waxblock1();}
            }
            }

module waxblockside1() {
    translate([0,0,20]) {
           rotate(a= 90, v=[0,10,0]) 
        {waxblock1();}}
        }

module waxblockside2() {
    translate([0,0,20]) {
           rotate(a= 90, v=[0,10,0]) 
        {waxblock2();}}
        }


for(x=[20], y=[9.85], z=[9.8]) {
    
//the paraffin side walls are pushed against the inside wall of plywood enclosure:
    
translate([0,0,2]) {waxblock1();}
translate([x,0,2]) {waxblock2();}
translate([0,y,2]) {waxblock2();}
translate([x,y,2]) {waxblock1();}
translate([0,2*y,2]) {waxblock2();}
translate([x,2*y,2]) {waxblock1();}
translate([0,3*y,2]) {waxblock1();}
translate([x,3*y,2]) {waxblock1();}

// in right side, 32.5 inches was replaced w x+z+2 to close the gap (Mein's instructions).

translate([0,0,x+z+2]) {waxblock2();}
translate([x,0,x+z+2]) {waxblock1();}
translate([0,y,x+z+2]) {waxblock2();}
translate([x,y,x+z+2]) {waxblock1();}
translate([0,2*y,x+z+2]) {waxblock2();}
translate([x,2*y,x+z+2]) {waxblock2();}
translate([0,3*y,x+z+2]) {waxblock1();}
translate([x,3*y,x+z+2]) {waxblock1();}

//the back paraffin blocks are pushed against inside wall of plywood enclosure:

translate([27,0,z+2]) {waxblockside1();}
translate([27,y,z+2]) {waxblockside1();}
translate([27,2*y,z+2]) {waxblockside2();}
translate([27,3*y,z+2]) {waxblockside1();}

//stand-in front wall:

translate([0,0,z+2]) {waxblockside1();}
translate([0,y,z+2]) { waxblockside2();}
translate([0,2*y,z+2]) { waxblockside2();}
translate([0,3*y,z+2]) { waxblockside2();}

//stand-in for paraffin on top of enclosure:

translate([6.5,4*y,2]) {waxblockside2();}
translate([6.5,4*y,2+x]) {waxblockside1();}
translate([6.5+z,4*y,2]) {waxblockside2();}
translate([6.5+z,4*y,2+x]) {waxblockside1();}
translate([6.5+2*z,4*y,2]) {waxblockside1();}
translate([6.5+2*z,4*y,2+x]) {waxblockside1();}

//stand-in for molded wax block at the bottom of the enclosure:

translate([z,0,z+2]) {waxblockside1();}
translate([2*z,0,z+2]) {waxblockside2();}


}

