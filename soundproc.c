#include "soundproc.h"

#include <stdio.h>

float a = 0;

void writeNote(float (*note)(float), float start, float length){
    a += 1;
    printf("%f\n", a);
}
