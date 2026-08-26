pragma circom 2.0.0;

include "../../circuits/escalarmul.circom";


template Main() {
    signal input in[256];
    signal output out[2];

    var i;

    var base[2] = [5299619240641551281634865583518297030282874472190772894086521144482721001553,
                   16950150798460657717958625567821834550301663161624707787222815936182638968203];

    component escalarMul = EscalarMul(256, base);

    escalarMul.inp[0] <== 0;
    escalarMul.inp[1] <== 1;

    for  (i=0; i<256; i++) {
        in[i] ==> escalarMul.in[i];
    }

    escalarMul.out[0] ==> out[0];
    escalarMul.out[1] ==> out[1];
}

component main = Main();

/*thread 'main' has overflowed its stack
error: process didn't exit successfully: `target\debug\circom.exe circomlib/test/circuits/escalarmul_test_min.circom` (exit code: 0xc00000fd, STATUS_STACK_OVERFLOW)
*/