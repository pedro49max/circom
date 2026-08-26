pragma circom 2.0.0;

include "../../circuits/mux1.circom";
include "../../circuits/bitify.circom";


template Constants() {
    var i;
    signal output out[2];

    out[0] <== 37;
    out[1] <== 47;
}

template Main() {
    var i;
    signal input selector;//private
    signal output out;

    component mux = Mux1();
    component n2b = Num2Bits(1);
    component cst = Constants();

    selector ==> n2b.in;
    n2b.out[0] ==> mux.s;
    for (i=0; i<2; i++) {
        cst.out[i] ==> mux.c[i];
    }

    mux.out ==> out;
}

component main = Main();


/*
template instances: 5
Signal: i, Bounds: (Bounds: [1, 1], true)
Signal: out, Bounds: (Bounds: [0, 0], false)

Signal: i, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: mux.c, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: mux.s, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: mux, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: out, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)

Signal: e2, Bounds: (Bounds: [2, 2], true)
Signal: lc1, Bounds: (Bounds: [0, 1], true)
Signal: i, Bounds: (Bounds: [1, 1], true)
Signal: out, Bounds: (Bounds: [0, 1], false)

Signal: i, Bounds: (Bounds: [0, 0], true)
Signal: out, Bounds: (Bounds: [37, 47], true)

Signal: mux.s, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: i, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: out, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: cst, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: mux.c, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: n2b.in, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: mux, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: n2b, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)

The template Num2Bits has the following fathers: 
Template Main subcomponent n2b
The template MultiMux1 has the following fathers: 
Template Mux1 subcomponent mux
The template Mux1 has the following fathers: 
Template Main subcomponent mux
The template Constants has the following fathers: 
Template Main subcomponent cst
The signal in of the template Num2Bits has the following bounds in the father template Main
Bounds: 0, 21888242871839275222246405745257275088548364400416034343698204186575808495616
*/