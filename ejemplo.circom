include ".\node_modules\circomlib\circuits\compconstant.circom";
include ".\node_modules\circomlib\circuits\poseidon.circom";
include ".\node_modules\circomlib\circuits\pointbits.circom";
include ".\node_modules\circomlib\circuits\mimc.circom";
include ".\node_modules\circomlib\circuits\mimcsponge.circom";
include ".\node_modules\circomlib\circuits\bitify.circom";
include ".\node_modules\circomlib\circuits\escalarmulany.circom";
include ".\node_modules\circomlib\circuits\escalarmulfix.circom";


template Switcher() {
    signal input sel;
    signal input L;
    signal input R;
    signal output outL;
    signal output outR;

    signal aux;

    aux <== (R-L)*sel;    // We create aux in order to have only one multiplication
    outL <==  aux + L;
    outR <== -aux + R;
}

component main = Switcher();