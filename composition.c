#include <stdio.h>
#include <math.h>

#include "soundproc.h"

/**
 * Generates the sample of a triangle wave for input theta.
 * Theta is the point in the wave; 0 is the beginning and 2*pi the end.
 */
float triangle(float theta){
    if(theta < M_PI){
        return theta/M_PI;
    }
    else{
        return 1.0 - (theta - M_PI)/M_PI;
    }
}

/**
 * Generates the sample of a square wave for input theta.
 * Theta is the input of the wave ranging from 0 to 2*pi.
 */
float square(float theta){
    if(theta < M_PI){
        return 0;
    }
    else{
        return 1;
    }
}

/**
 * Returns amplitude for note given percentage of note completion.
 * Attack, Decay, Sustain and Release are temporarily hard coded.
 */

float envelope(float percent){
    // Attack
    if(percent < 0.05){
        // percent / length * peak amp
        return percent / 0.05 * 1.5;
    }
    // Decay
    else if(percent < 0.1){
        // peak amp - (peak amp - normal amp)
        // * (percent - decay start) / decay length
        return 1.5 - 0.5 * (percent - 0.05) / 0.05;
    }
    // Sustain
    else if(percent < 0.95){
        // normal amp
        return 1;
    }
    // Release
    else{
        // normal amp - (percent - release start) / release length
        return 1 - (percent - 0.95) / 0.05;
    }
}

float tempEnvelope(int x, int len){
    return envelope((float)x / len);
}

int main(int argc, char* argv[]){
    // setup format, sampling rate, and mono / stereo

    init();
    writeNote(&triangle, &tempEnvelope, 440, 8192, 0, 2);
    writeNote(&triangle, &tempEnvelope, 554.4, 8192, 0.2, 1.8);
    writeNote(&triangle, &tempEnvelope, 659.3, 8192, 0.4, 1.6);
    writeNote(&triangle, &tempEnvelope, 880, 8192, 3, 1); 

    playSamples(argv[0], argv[0]);
}

