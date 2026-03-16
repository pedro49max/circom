template A(){
    signal input a;
    signal x[3];
    var aux = 0;
    var aux1 = 5;
    x[0]<--2;
     x[1]<--7;
    while(x[1]==7){
        aux=aux1 + 1;
        aux1 = aux1 + 1;
    }


}


component main = A();