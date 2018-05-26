#include "instruments.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

float random(float max){
    return (float)rand() / (float)(RAND_MAX / max);
}

float triangle(float theta){
    if(theta < M_PI){
        return theta / M_PI;
    }
    else{
        return 1.0 - (theta - M_PI) / M_PI;
    }
}

float square(float theta){
    if(theta < M_PI){
        return 0.0f;
    }
    else{
        return 1.0f;
    }
}

float sine(float theta){
    // you thought it was a sin wave, but 'twas a cos all along!
    return (float)cos(theta);
}

float whiteNoise(float theta){
    // white noise is the same as gaussian samples
    float output = 0;
    for(int i = 0; i < 12; i ++){
        output += random(1);
    }
    return output;
}

int envlpVal[4];
void setEnvelope(float atk, float dcy, float stn, float rls){
    envlpVal[0] = atk;
    envlpVal[1] = dcy;
    envlpVal[2] = stn;
    envlpVal[3] = rls;
}

float envelope(int x, int length){
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
