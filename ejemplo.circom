template A(){
    signal input a;
    signal x[3];
    x[0]<--2;
    x[1]<--7;
    x[2] <-- 14;
}


component main = A();