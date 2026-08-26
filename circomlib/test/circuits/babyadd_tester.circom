pragma circom 2.0.0;
include "../../circuits/babyjub.circom";

component main = BabyAdd();
/*
template instances: 1
Signal: yout, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: xout, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: a, Bounds: (Bounds: [168700, 168700], true)
Signal: beta, Bounds: (Bounds: [0, 1], false)
Signal: d, Bounds: (Bounds: [168696, 168696], true)
Signal: tau, Bounds: (Bounds: [0, 1], true)
Signal: gamma, Bounds: (Bounds: [0, 1], false)
Signal: delta, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808158219], false)

Signal: yout, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: xout, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: a, Bounds: (Bounds: [168700, 168700], true)
Signal: beta, Bounds: (Bounds: [0, 1], false)
Signal: d, Bounds: (Bounds: [168696, 168696], true)
Signal: tau, Bounds: (Bounds: [0, 1], true)
Signal: gamma, Bounds: (Bounds: [0, 1], false)
Signal: delta, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808158219], false)

Signal: yout, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: xout, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808495616], false)
Signal: a, Bounds: (Bounds: [168700, 168700], true)
Signal: beta, Bounds: (Bounds: [0, 1], false)
Signal: d, Bounds: (Bounds: [168696, 168696], true)
Signal: tau, Bounds: (Bounds: [0, 1], true)
Signal: gamma, Bounds: (Bounds: [0, 1], false)
Signal: delta, Bounds: (Bounds: [0, 21888242871839275222246405745257275088548364400416034343698204186575808158219], false)
*/