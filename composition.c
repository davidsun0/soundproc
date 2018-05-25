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
    //*/
    /*
    static const pa_sample_spec ss = {
        .format = PA_SAMPLE_S16NE,
        .rate = 48000,
        .channels = 1
    };

    float samp_rate = (float)ss.rate;
    int buff_size = 1024;
    int samples = samp_rate * 1.5;
    
    pa_simple *s = NULL;
    int error;

    // create connection object
    s = pa_simple_new(
            NULL,               //default pa server
            argv[0],            //application name
            PA_STREAM_PLAYBACK,
            NULL,               //default device
            argv[0],            //stream description
            &ss,                //sample format
            NULL,               //default channel map
            NULL,               //default buffering
            &error              //error code
    );

    if(s == NULL){
        printf("Failed to connect to Pulse Audio server");
        return 1;
    }

    // create buffer
    int16_t buffer[1024];

    int j = 0;
    float theta = 0;
    for(int i = samples; i >= 0; i --){
        // theta += TAU * Frequency (Hz) / Sample Rate (Hz)
        theta += 2.0 * M_PI * 440.0 / samp_rate;
        if(theta > 2.0 * M_PI)
            theta -= 2.0 * M_PI;
        // printf("%f\n", 32 * envelope(((float)(samples - i)) / samples));
        buffer[j++] = 8192 * envelope(((float)(samples - i)) / samples)
            * square(theta);
        if(j == buff_size){
            if(pa_simple_write(s, buffer, sizeof(buffer), &error) < 0){
                fprintf(stderr, "failed to write to pulse audio");
                return 1;
            }
            else{
                j = 0;
            }
        }
    }
    // play remaining samples from buffer[0] to buffer[j]
    if(j != 0){
        pa_simple_write(s, buffer, sizeof(buffer[0]) * (j - 1), &error);
    }

    // wait until all data is played
    pa_simple_drain(s, &error);
    // free connection
    pa_simple_free(s);
    return 0;
    //*/
}

