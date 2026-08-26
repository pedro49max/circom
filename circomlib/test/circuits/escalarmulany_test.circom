pragma circom 2.0.0;

include "../../circuits/escalarmulany.circom";
include "../../circuits/bitify.circom";

template Main() {
    signal input e;
    signal input p[2];
    signal output out[2];

    component n2b = Num2Bits(253);
    component escalarMulAny = EscalarMulAny(253);

    escalarMulAny.p[0] <== p[0];
    escalarMulAny.p[1] <== p[1];

    var i;

    e ==> n2b.in;

    for  (i=0; i<253; i++) {
        n2b.out[i] ==> escalarMulAny.e[i];
    }

    escalarMulAny.out[0] ==> out[0];
    escalarMulAny.out[1] ==> out[1];
}

component main = Main();

/*
template instances: 13
Signal: out, Bounds: (Bounds: [0, 1], false)
Signal: i, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: lc1, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: e2, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)

Signal: inv, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: out, Bounds: (Bounds: [1, 1], false)

Signal: out, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)

Signal: a, Bounds: (Bounds: [168700, 168700], true)
Signal: B, Bounds: (Bounds: [1, 1], true)
Signal: lamda, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: out, Bounds: (Bounds: [-21888242871839275222246405745257275088548364400416034343698204186575808495616, 21888242871839275222246405745257275088548364400416034343698204186575808326922], false)
Signal: x1_2, Bounds: (Bounds: [0, 1], false)
Signal: A, Bounds: (Bounds: [168698, 168698], true)
Signal: d, Bounds: (Bounds: [168696, 168696], true)

Signal: lamda, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: out, Bounds: (Bounds: [-21888242871839275222246405745257275088548364400416034343698204186575808495616, 21888242871839275222246405745257275088548364400416034343698204186575808326922], false)
Signal: d, Bounds: (Bounds: [168696, 168696], true)
Signal: B, Bounds: (Bounds: [1, 1], true)
Signal: a, Bounds: (Bounds: [168700, 168700], true)
Signal: A, Bounds: (Bounds: [168698, 168698], true)

Signal: out, Bounds: (Bounds: [0, 0], false)

Signal: doubler, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: selector.sel, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: doubler.in, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: selector.in, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: selector, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: adder.in2, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: dblOut, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: addOut, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: adder, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: adder.in1, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)

Signal: out, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)

Signal: tau, Bounds: (Bounds: [0, 1], true)
Signal: xout, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: yout, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: beta, Bounds: (Bounds: [0, 1], false)
Signal: a, Bounds: (Bounds: [168700, 168700], true)
Signal: gamma, Bounds: (Bounds: [0, 1], false)
Signal: d, Bounds: (Bounds: [168696, 168696], true)
Signal: delta, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808158219], false)

Signal: lastSel.sel, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: eadder.y1, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: i, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: eadder.x1, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: eadder.y2, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: m2e, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: bits, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: bits.dblIn, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: lastSel.in, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: eadder, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: out, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: bits.sel, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: dbl, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: lastSel, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: e2m.in, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: e2m, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: eadder.x2, Bounds: (Bounds: [-21888242871839275222246405745257275088548364400416034343698204186575808495616, 0], false)
Signal: bits.addIn, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: m2e.in, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)

Signal: lastSel, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: lastSel.sel, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: m2e, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: e2m.in, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: eadder.x1, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: e2m, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: dbl, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: eadder.y2, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: eadder.x2, Bounds: (Bounds: [-21888242871839275222246405745257275088548364400416034343698204186575808495616, 0], false)
Signal: bits.sel, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: eadder.y1, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: bits.dblIn, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: bits.addIn, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: i, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: eadder, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: out, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: m2e.in, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: bits, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: lastSel.in, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)

Signal: nsegments, Bounds: (Bounds: [2, 2], true)
Signal: nseg, Bounds: (Bounds: [105, 148], true)
Signal: s, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: i, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: adders.x2, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: adders.y1, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: m2e, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: adders.y2, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: adders.x1, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: m2e.in, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: zeropoint.in, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: segments.e, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: segments.p, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: segments, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: nlastsegment, Bounds: (Bounds: [105, 105], true)
Signal: adders, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: zeropoint, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: doublers, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: doublers.in, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: out, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)

Signal: escalarMulAny.p, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: out, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: escalarMulAny.e, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: escalarMulAny, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: n2b, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: i, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: n2b.in, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)

The template SegmentMulAny has the following fathers: 
Template EscalarMulAny subcomponent segments
The template EscalarMulAny has the following fathers: 
Template Main subcomponent escalarMulAny
The template IsZero has the following fathers: 
Template EscalarMulAny subcomponent zeropoint
The template BitElementMulAny has the following fathers: 
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
Template SegmentMulAny subcomponent bits
The template Edwards2Montgomery has the following fathers: 
Template SegmentMulAny subcomponent e2m
Template SegmentMulAny subcomponent e2m
The template MontgomeryAdd has the following fathers: 
Template BitElementMulAny subcomponent adder
The template Montgomery2Edwards has the following fathers: 
Template SegmentMulAny subcomponent m2e
Template SegmentMulAny subcomponent m2e
Template EscalarMulAny subcomponent m2e
The template MontgomeryDouble has the following fathers: 
Template BitElementMulAny subcomponent doubler
Template EscalarMulAny subcomponent doublers
The template SegmentMulAny has the following fathers: 
Template EscalarMulAny subcomponent segments
The template Num2Bits has the following fathers: 
Template Main subcomponent n2b
The template BabyAdd has the following fathers: 
Template SegmentMulAny subcomponent eadder
Template SegmentMulAny subcomponent eadder
Template EscalarMulAny subcomponent adders
The template Multiplexor2 has the following fathers: 
Template BitElementMulAny subcomponent selector
Template SegmentMulAny subcomponent lastSel
Template SegmentMulAny subcomponent lastSel
The signal e of the template SegmentMulAny has the following bounds in the father template EscalarMulAny
Bounds: 0, 21888242871839275222246405745257275088548364400416034343698204186575808495616
*/