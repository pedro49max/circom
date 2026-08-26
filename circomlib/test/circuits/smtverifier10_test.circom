pragma circom 2.0.0;

include "../../circuits/smt/smtverifier.circom";

component main = SMTVerifier(10);

/*
thread 'main' has overflowed its stack
error: process didn't exit successfully: `target\debug\circom.exe circomlib/test/circuits/smtverifier10_test.circom` (exit code: 0xc00000fd, STATUS_STACK_OVERFLOW)
*/