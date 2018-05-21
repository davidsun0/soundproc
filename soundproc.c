#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include <pulse/simple.h>
#include <pulse/error.h>

/*
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

/*
 * Returns amplitude for note given percentage of note completion.
 * Attack, Decay, Sustain and Release are temporarily hard coded.
 */

float envelope(float percent){
    // Attack
    if(percent < 0.05){
        // percent / length * peak amp
        return percent / 0.05 * 2;
    }
    // Decay
    else if(percent < 0.1){
        // peak amp - (peak amp - normal amp)
        // * (percent - decay start) / decay length
        return 2 - 1 * (percent - 0.05) / 0.05;
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

    int buff_size = 1024;
    float theta = 0;

    static const pa_sample_spec ss = {
        .format = PA_SAMPLE_S16NE,
        .rate = 48000,
        .channels = 1
    };

    float samp_rate = (float)ss.rate;
    printf("%f\n", samp_rate);
    int samples = samp_rate * 2;
    
    pa_simple *s = NULL;
    int error;

    // create connection object
    s = pa_simple_new(NULL,             //default pa server
                    argv[0],            //application name
                    PA_STREAM_PLAYBACK,
                    NULL,               //default device
                    argv[0],            //stream description
                    &ss,                //sample format
                    NULL,               //default channel map
                    NULL,               //default buffering
                    &error);            //error code

    if(s == NULL){
        printf("Failed to connect to Pulse Audio server");
        return 1;
    }

    // create buffer
    uint8_t buffer[1024] = {0};

    int j = 0;
    for(int i = samples; i >= 0; i --){
        // theta += TAU * Frequency (Hz) / Sample Rate (Hz)
        theta += 2.0 * M_PI * 220.0 / samp_rate;
        if(theta > 2.0 * M_PI)
            theta -= 2.0 * M_PI;
        // printf("%f\n", 32 * envelope(((float)(samples - i)) / samples));
        buffer[j++] = 64 * envelope(((float)(samples - i)) / samples)
            * triangle(theta);
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
}

