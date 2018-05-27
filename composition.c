#include <stdio.h>
#include <math.h>

#include "soundproc.h"
#include "instruments.h"

float triangle(float theta);
float sine(float theta);
float square(float theta);

void playNote(
        float (*note)(float),
        float (*envelope)(int, int),
        float freq,
        float volume,
        float start,
        float length
        );

int main(int argc, char* argv[]){
    // setup format, sampling rate, and mono / stereo

    soundProcInit();
    setASDR(960, 960, 0, 2000);

    playNote(&triangle, &ASDR, 440, 1, 0, 2);
    playNote(&square, &ASDR, 554.4, 1, 0.2, 1.8);
    playNote(&sine, &ASDR, 659.3, 1, 0.4, 1.6);
    playNote(&whiteNoise, &ASDR, 100, 1, 2.00, 0.8);
    playNote(&triangle, &ASDR, 880, 1, 2.6, 1);
    playSamples(argv[0], argv[0]);
}

