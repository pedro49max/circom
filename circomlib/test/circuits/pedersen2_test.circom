pragma circom 2.0.0;

include "../../circuits/pedersen.circom";
include "../../circuits/bitify.circom";


template Main() {
    signal input in;
    signal output out[2];

    component pedersen = Pedersen(256);

    component n2b;
    n2b = Num2Bits(253);

    var i;

    in ==> n2b.in;

    for  (i=0; i<253; i++) {
        pedersen.in[i] <== n2b.out[i];
    }

    for (i=253; i<256; i++) {
        pedersen.in[i] <== 0;
    }

    pedersen.out[0] ==> out[0];
    pedersen.out[1] ==> out[1];
}

component main = Main();


/*
thread 'main' has overflowed its stack
error: process didn't exit successfully: `target\debug\circom.exe circomlib/test/circuits/pedersen2_test.circom` (exit code: 0xc00000fd, STATUS_STACK_OVERFLOW)
*/