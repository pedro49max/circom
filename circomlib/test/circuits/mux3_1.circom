pragma circom 2.0.0;

include "../../circuits/mux3.circom";
include "../../circuits/bitify.circom";


template Constants() {
    var i;
    signal output out[8];

    out[0] <== 37;
    out[1] <== 47;
    out[2] <== 53;
    out[3] <== 71;
    out[4] <== 89;
    out[5] <== 107;
    out[6] <== 163;
    out[7] <== 191;
}

template Main() {
    var i;
    signal input selector;//private
    signal output out;

    component mux = Mux3();
    component n2b = Num2Bits(3);
    component cst = Constants();

    selector ==> n2b.in;
    for (i=0; i<3; i++) {
        n2b.out[i] ==> mux.s[i];
    }
    for (i=0; i<8; i++) {
        cst.out[i] ==> mux.c[i];
    }

    mux.out ==> out;
}

component main = Main();

/*
template instances: 5
Signal: a21, Bounds: (Bounds: [0, 2], false)
Signal: a20, Bounds: (Bounds: [0, 2], false)
Signal: s10, Bounds: (Bounds: [0, 1], false)
Signal: a10, Bounds: (Bounds: [0, 2], false)
Signal: out, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495613], false)
Signal: a2, Bounds: (Bounds: [-21888242871839275222246405745257275088548364400416034343698204186575808495616, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: i, Bounds: (Bounds: [1, 1], true)
Signal: a1, Bounds: (Bounds: [0, 1], false)
Signal: a0, Bounds: (Bounds: [0, 1], false)
Signal: a210, Bounds: (Bounds: [0, 4], false)
Signal: a, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)

Signal: mux.s, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: mux.c, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: out, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: i, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: mux, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)

Signal: e2, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: i, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: out, Bounds: (Bounds: [0, 1], false)
Signal: lc1, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)

Signal: i, Bounds: (Bounds: [0, 0], true)
Signal: out, Bounds: (Bounds: [37, 191], true)

Signal: mux.s, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: n2b, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: n2b.in, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: cst, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: mux.c, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: i, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: mux, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: out, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)

The template MultiMux3 has the following fathers: 
Template Mux3 subcomponent mux
The template Num2Bits has the following fathers: 
Template Main subcomponent n2b
The template Constants has the following fathers: 
Template Main subcomponent cst
The template Mux3 has the following fathers: 
Template Main subcomponent mux
The signal s of the template MultiMux3 has the following bounds in the father template Mux3
Bounds: 0, 21888242871839275222246405745257275088548364400416034343698204186575808495616
*/