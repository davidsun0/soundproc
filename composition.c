#include <stdio.h>
#include <math.h>

#include "soundproc.h"
#include "instruments.h"

int main(int argc, char* argv[]){
    // setup format, sampling rate, and mono / stereo

    soundProcInit();
    setEnvelope(960, 960, 0, 2000);

    writeNote(&triangle, &envelope, 440, 8192, 0, 2);
    writeNote(&triangle, &envelope, 554.4, 8192, 0.2, 1.8);
    writeNote(&triangle, &envelope, 659.3, 8192, 0.4, 1.6);
    writeNote(&triangle, &envelope, 880, 8192, 3, 1);

    playSamples(argv[0], argv[0]);
}

