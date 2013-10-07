/* Fibonacci Series c language */
#include<stdio.h>

int main(){
    segfault();
    return 0;
}

int segfault();
int segfault() {
    int* j; int i;
    j = (int*) 0;
    i = *j;
}
