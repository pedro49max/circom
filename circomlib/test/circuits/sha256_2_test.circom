pragma circom 2.0.0;

include "../../circuits/sha256/sha256_2.circom";

template Main() {
    signal input a; //private
    signal input b; //private
    signal output out;

    component sha256_2 = Sha256_2();

    sha256_2.a <== a;
    sha256_2.b <== b;
    out <== sha256_2.out;
}

component main = Main();

/*
thread 'main' has overflowed its stack
error: process didn't exit successfully: `target\debug\circom.exe circomlib/test/circuits/sha256_2_test.circom` (exit code: 0xc00000fd, STATUS_STACK_OVERFLOW)
*/