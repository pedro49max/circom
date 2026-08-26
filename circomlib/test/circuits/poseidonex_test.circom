pragma circom 2.0.0;

include "../../circuits/poseidon.circom";

component main = PoseidonEx(16, 17);

/*
thread 'main' has overflowed its stack
error: process didn't exit successfully: `target\debug\circom.exe circomlib/test/circuits/poseidonex_test.circom` (exit code: 0xc00000fd, STATUS_STACK_OVERFLOW)
*/