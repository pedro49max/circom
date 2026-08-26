pragma circom 2.0.0;

include "../../circuits/eddsa.circom";

component main = EdDSAVerifier(80);
/*
thread 'main' has overflowed its stack
error: process didn't exit successfully: `target\debug\circom.exe circomlib/test/circuits/eddsa_test.circom` (exit code: 0xc00000fd, STATUS_STACK_OVERFLOW)
*/