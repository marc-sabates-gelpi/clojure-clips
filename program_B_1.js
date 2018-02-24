var b = 93, c = 93, a = 1;
if(a != 0) {
    b = 109300;
    c = 126300;
}
C: var f = 1,
    d = 2;

B: var e = 2;

A: if (d * e - b == 0) {
    f = 0;
}
e++;
if (e - b != 0) {
    goto A;
}
d++;
if (d - b != 0) {
    goto B;
}
if (f == 0) {
    h++;
}
if ( b - c == 0) {
    return;
}
b+=17;
goto C;
