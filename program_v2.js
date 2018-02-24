// # non-primes on 109300, 109317, 109334, ..., 109300 + n*17 < 126300


while (var b = 109300, c = 126300; b!=c; b+=17) {

    var f = 1, d = 2;
    do {

	var e = 2;
        do {
            if ( d * e == b ) {
                f = 0;
            }
            e++;
        } while ( e != b );

	d++;
    } while ( d != b );

    if ( f == 0 ) { // non-primes between b <= x < c with steps of 17
        h++;
    }
}
