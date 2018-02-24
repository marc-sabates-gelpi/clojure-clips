for (var b = 109300, c = 126300; b != c; b += 17) {
    var f = 1;
    for(var d = 2; d != b; d++){
	for (var e = 2; e != b; e++) {
	    if (d * e == b) {
		f = 0;
	    }
	}
    }
    if (f == 0) {
	h++;
    }
}
