#include "instruments.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#include "soundproc.h"

// UTILITY FUNCTIONS

float random(float max){
    return (float)rand() / (float)(RAND_MAX / max);
}

// NOTES

void playNote(
        float (*note)(float),
        float (envelope)(int, int),
        float freq,
        float volume,
        float start,
        float length
        ){
    float amplitude = 4096 * volume;
    int sampStart = SAMPLE_RATE * start;
    int sampLen = SAMPLE_RATE * length;
    writeNote(note, envelope, freq, amplitude, sampStart, sampLen);
}

// ENVELOPE

int envlpVal[4];
void setASDR(float atk, float dcy, float stn, float rls){
    envlpVal[0] = atk;
    envlpVal[1] = dcy;
    envlpVal[2] = stn;
    envlpVal[3] = rls;
}

float ASDR(int x, int length){
    // attack
    if(x < envlpVal[0]){
        // x / atk length * peak amp
        return (float)x / envlpVal[0] * 1.2;
    }
    // decay
    else if(x < envlpVal[0] + envlpVal[1]){
        // peak amp - (peak amp - normal amp) * (x - decay start)
        // / decay length
        return 1.2 - 0.2 * (float)(x - envlpVal[0]) / envlpVal[1];
    }
    // sustain
    else if(x < length - envlpVal[3]){
        return 1;
    }
    // release
    else{
        // normal amp - (x - release start) / release length
        return 1 - (float)(x - (length - envlpVal[3])) / envlpVal[3];
    }
}

// WAVEFORMS

float triangle(float theta){
    if(theta < M_PI){
        return 2 * theta / M_PI - 1;
    }
    else{
        return 1 - 2 * (theta - M_PI) / M_PI;
    }
}

float square(float theta){
    if(theta < M_PI){
        return -1.0f;
    }
    else{
        return 1.0f;
    }
}

float sine(float theta){
    // you thought it was a sin wave, but 'twas a cos all along!
    return (float)cos(theta);
}

/**
 * Approximates white noise. White noise is Gaussian distributed
 * sample points. This function generates Bates distributed points
 * from uniform values.
 * @param   theta   does nothing
 */
float whiteNoise(float theta){
    /*
     * white noise is the same as gaussian samples
     * Bate's distribution to approx gauss
     * mu = 0.5, var = 1 / (12 * n)
     */
    float output = 0;
    for(int i = 0; i < 12; i ++){
        output += random(1) / 12.0;
    }
    /*
     * transform to standard volume (1f)
     * mu = 0, var = 4 / (3 * n)
     */
    return output * 2 - 1;
}
