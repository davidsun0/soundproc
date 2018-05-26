#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include <pulse/simple.h>
#include <pulse/error.h>

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

int main(int argc, char* argv[]){
    // setup format, sampling rate, and mono / stereo

    //*
    init();

    int16_t buffer[48000];
    float theta = 0;

    // A
    for(int i = 0; i < 48000; i ++){
        theta += 2 * M_PI * 440.0 / SAMPLE_RATE;
        if(theta > 2 * M_PI){
            theta -= 2 * M_PI;
        }
        buffer[i] = 8192 * envelope(i / 48000.0) 
            * square(theta);
    }
    writeSamples(buffer, 48000, 0);

    // C#
    theta = 0;
    for(int i = 0; i < 40000; i ++){
        theta += 2 * M_PI * 554.4 / SAMPLE_RATE;
        if(theta > 2 * M_PI){
            theta -= 2 * M_PI;
        }
        buffer[i] = 8192 * envelope(i / 40000.0) 
            * triangle(theta);
    }
    writeSamples(buffer, 40000, 8000);
    
    // E
    theta = 0;
    for(int i = 0; i < 32000; i ++){
        theta += 2 * M_PI * 659.3 / SAMPLE_RATE;
        if(theta > 2 * M_PI){
            theta -= 2 * M_PI;
        }
        buffer[i] = 8192 * envelope(i / 32000.0) 
            * triangle(theta);
    }
    writeSamples(buffer, 32000, 16000);
    
    playSamples(argv[0], argv[0]);
}

