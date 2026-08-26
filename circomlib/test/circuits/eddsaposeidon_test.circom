pragma circom 2.0.0;

include "../../circuits/eddsaposeidon.circom";

component main = EdDSAPoseidonVerifier();
/*
thread 'main' has overflowed its stack
error: process didn't exit successfully: `target\debug\circom.exe circomlib/test/circuits/eddsaposeidon_test.circom` (exit code: 0xc00000fd, STATUS_STACK_OVERFLOW)
*/