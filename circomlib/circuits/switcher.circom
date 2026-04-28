/*
    Copyright 2018 0KIMS association.

    This file is part of circom (Zero Knowledge Circuit Compiler).

    circom is a free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    circom is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
    or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
    License for more details.

    You should have received a copy of the GNU General Public License
    along with circom. If not, see <https://www.gnu.org/licenses/>.
*/

/*
    Assume sel is binary.

    If sel == 0 then outL = L and outR=R
    If sel == 1 then outL = R and outR=L

 */
 
pragma circom 2.0.0;

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

/*
Signal: outR, Bounds: Bounds: [21888242871839275222246405745257275088548364400416034343698204186575808495615, 21888242871839275222246405745257275088548364400416034343698204186575808495615]
Signal: aux, Bounds: Bounds: [0, 1]
Signal: outL, Bounds: Bounds: [0, 0]
*/