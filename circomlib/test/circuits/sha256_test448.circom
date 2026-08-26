pragma circom 2.0.0;

include "../../circuits/sha256/sha256.circom";

component main = Sha256(448);

/*
thread 'main' has overflowed its stack
error: process didn't exit successfully: `target\debug\circom.exe circomlib/test/circuits/sha256_test448.circom` (exit code: 0xc00000fd, STATUS_STACK_OVERFLOW)
*/