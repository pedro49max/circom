
template Switcher() {
    signal a <== 5;
    signal b <== 2;
    signal aux;
    var mulFix[253];
    var i;
    for (i=0; i<253; i++) {
        mulFix[i] =a*b;
    }
    signal safe;
    safe <-- a != mulFix[0];
}

component main = Switcher();