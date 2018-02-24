var b = 93,
    c = 93;

if (a != 0) {
    b = 109300;
    c = 126300;
}

while ( true ) {
    var f = 1,
        d = 2;

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

    if ( f == 0 ) {
        h++;
    }

    if ( b == c ) {
        return;
    }

    b += 17;
}
