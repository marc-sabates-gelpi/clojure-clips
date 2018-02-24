var b = 109300, c = 126300;

do {
    var f = 1, d = 2;
    do {
	var e = 2;
	do {
	    if (d * e == b) {
		f = 0;
	    }
	    e++;
	} while (e != b)
	d++;
    } while (d != b)
    if (f == 0) {
	h++;
    }
    if (b == c) {
	return;
    }
    b += 17;
} while (true)
