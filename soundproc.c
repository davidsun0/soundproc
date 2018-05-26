#include "soundproc.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <pulse/simple.h>
#include <pulse/error.h>

int min(int a, int b){
    return a < b ? a : b;
}

int max(int a, int b){
    return a > b ? a : b;
}

typedef struct Node {
    struct Node *prev;  // prev node
    struct Node *next;  // next node
    int length;         // length of used data
    SAMPLE_DEPTH *data; // pointer to data
} Node;

Node *head;

/**
 * Initializes the sound data structure.
 */
void init(){
    head = (Node *)malloc(sizeof(Node));
    if(head == NULL){
        fprintf(stderr, "Failed to allocate linked list");
        exit(1);
    }

    head->prev = NULL;
    head->next = NULL;
    head->length = 0;
    head->data = (SAMPLE_DEPTH *) malloc(sizeof(SAMPLE_DEPTH) *
            SAMPS_PER_NODE);
}

/**
 * Writes samples to the sound data structure.
 * @param   data    pointer to PCM data
 * @param   length  number of samples in data
 * @param   start   sample offset to write data at
 */
void writeSamples(SAMPLE_DEPTH *data, int length, int start){
    printf("[writing %d samples at %d]\n", length, start);
    Node *curr = head;
    // calculate start node and start position
    // TODO: improve start node search
    // I think keeping track of the last node and moving left or right
    // from there can be much faster than starting at head every time
    int frameStart = start;
    while(frameStart > SAMPS_PER_NODE){
        if(curr->next == NULL){
            printf("allocating new node\n");
            // allocate the next node
            curr->next = (Node *)malloc(sizeof(Node));
            curr->next->prev = curr;
            curr->next->next = NULL;
            curr->next->length = 0;
            // don't have to allocate data on the way
            curr->next->data = NULL;
        }
        printf("moving to new node\n");
        curr->length = SAMPS_PER_NODE;
        curr = curr->next;
        frameStart -= SAMPS_PER_NODE;
        if(frameStart < SAMPS_PER_NODE && curr->data == NULL){
            printf("allocating data for end node\n");
            // allocate data of current node
            curr->data = (SAMPLE_DEPTH *) malloc(sizeof(SAMPLE_DEPTH) *
                    SAMPS_PER_NODE);
        }
        else{
            curr->length = SAMPS_PER_NODE;
        }
    }

    /*
    while samples > 0
        write as much as possible into current node
        calculate how many more samples will need to be written
    */
    int dataStart = 0;
    while(dataStart < length){
        int toWrite = min(SAMPS_PER_NODE - frameStart, length);
        printf("writing %d samples from %d to %d\n", toWrite,
                frameStart, frameStart + length);
        for(int i = 0; i < toWrite; i ++){
           curr->data[frameStart + i] += data[dataStart + i];
        }
        printf("node length: %d -> ", curr->length);
        curr->length = max(curr->length,
                min(frameStart + toWrite, SAMPS_PER_NODE));
        printf("%d\n", curr->length);

        // This should never happen
        if(curr->length > SAMPS_PER_NODE){
            printf("something went terribly wrong.\n");
            printf("%d samples in node at %p,", curr->length, curr);
            printf(" %d were expected\n", SAMPS_PER_NODE);
        }
        // printf("node length now %d\n", curr->length);
        dataStart += toWrite;
        
        // if the data spills over into the next node, start at the
        // beginning sample
        frameStart = 0;
        curr = curr->next;
    }
    printf("\n\n");
}

void writeNote(
        float (*note)(float),
        float (*envelope)(int, int),
        float freq,
        float amplitude,
        float start,
        float length
        ){

    int numSamples = length * SAMPLE_RATE;
    SAMPLE_DEPTH *buffer = (SAMPLE_DEPTH *) malloc(sizeof(SAMPLE_DEPTH)
            * numSamples);

    float theta = 0;
    for(int i = 0; i < numSamples; i ++){
        theta += 2 * M_PI * freq / SAMPLE_RATE;
        if(theta > 2 * M_PI){
            theta -= 2 * M_PI;
        }
        buffer[i] = amplitude * envelope(i, numSamples) * note(theta);
    }
    writeSamples(buffer, numSamples, start * SAMPLE_RATE);
    return;
}

/**
 * Plays the entire composition through Pulse Audio.
 * @param   appName     application name given to Pulse Audio
 * @param   streamName  title of stream given to Pulse Audio
 */
void playSamples(char *appName, char *streamName){
    printf("playing samples through Pulse Audio...\n");
    static const pa_sample_spec ss = {
        .format = PA_FORMAT,
        .rate = SAMPLE_RATE,
        .channels = 1
    };

    pa_simple *s = NULL;
    int error;

    s = pa_simple_new(
            NULL,               // default PA server
            appName,            // application name
            PA_STREAM_PLAYBACK,
            NULL,               // default device
            streamName,         // stream description
            &ss,                // sample format
            NULL,               // default channel map
            NULL,               // default buffering
            &error              // error code
    );

    if(s == NULL){
        fprintf(stderr, "Failed to connect to Pulse Audio server\n");
        return;
    }

    // iterate through the list, playing all of the data
    Node *curr = head;
    while(curr != NULL){
        printf("playing %d samples\n", curr->length);
        if(pa_simple_write(
                s,
                curr->data,
                sizeof(SAMPLE_DEPTH) * curr->length,
                &error) < 0){
            fprintf(stderr, "failed to write to Pulse Audio\n");
            if(curr->length == 0){
                printf("most likely because wrote a length 0 buffer\n");
            }
        }
        curr = curr->next;
    }

    // wait until all data is played and clean up
    pa_simple_drain(s, &error);
    pa_simple_free(s);
}
