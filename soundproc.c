#include <stdio.h>
#include <stdlib.h>

#include <pulse/simple.h>
#include <pulse/error.h>

int main(int argc, char* argv[]){
    static const pa_sample_spec ss = {
        .format = PA_SAMPLE_U8,
        .rate = 8000,
        .channels = 1
    };

    pa_simple *s = NULL;
    int ret = 1;
    int error;

    s = pa_simple_new(NULL,             //default pa server
                    argv[0],            //application name
                    PA_STREAM_PLAYBACK,
                    NULL,               //default device
                    "Sine Test",        //stream description
                    &ss,                //sample format
                    NULL,               //default channel map
                    NULL,               //default buffering
                    &error);            //error code

    if(s == NULL){
        printf("Failed to connect to Pulse Audio server");
        return 1;
    }

    /*
    FILE *fp = fopen("sine.wav", "rb");
    if(fp == NULL){
        printf("Failed to open file");
        return 1;
    }
    //fseek(fp, 44, SEEK_SET);
    */

    uint8_t buffer[1024];
    //ssize_t r;

    int k = 0;
    int rising = 1;
    for(int i = 0; i < 1024; i ++){
        buffer[i] = k * 2;
        if(rising){
            k ++;
            if(k > 15){
                k = 15;
                rising = 0;
            }
        }
        else{
            k --;
            if(k < 0){
                k = 0;
                rising = 1;
            }
        }
    }

    while(1){
        /*
        if((r = fread(buffer, 1, sizeof(buffer), fp)) <= 0){
            break;
        }
        */
        if(pa_simple_write(s, buffer, sizeof(buffer), &error) < 0){
            printf("failed to write to pa");
            break;
        }
    }

    //fclose(fp);

    //pa_simple_drain(s, &error);
    pa_simple_free(s);
    return 0;
}
