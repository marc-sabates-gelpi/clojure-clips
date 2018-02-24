// 109300 + 17x = 126300
// x = 1000
for (var b = 109300, c = 126300; b != c; b += 17) {
    var f = 1;
    for(var d = 2; d <= sqrt(b) && f != 0; d++){
	for (var e = d; e <= sqrt(b) && f != 0; e++) {
	    if (d * e == b) { //There are 2 nums that multiplied are b => b is not prime
		f = 0;
	    }
	}
    }
    if (f == 0) {
	h++;
    }
}
