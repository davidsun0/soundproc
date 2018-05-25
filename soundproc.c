#include "soundproc.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <pulse/simple.h>
#include <pulse/error.h>

typedef struct Node {
    struct Node *prev;  // prev node
    struct Node *next;  // next node
    int length;         // length of used data
    SAMPLE_DEPTH *data; // pointer to data
} Node;

Node *head;
Node *curr;

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

    curr = head;
}

/*
 * Writes samples to the linked list.
 * @param   data    pointer to PCM data
 * @param   length  number of samples in data
 * @param   start   sample offset to write data at
 */
void writeSamples(SAMPLE_DEPTH *data, int length, int start){
    int remaining = SAMPS_PER_NODE - (start + length);
    printf("%d samples left in curr\n", remaining);

    // TODO: calculate start node and start position

    // if start + length < SAMPLE_DEPTH
    if(remaining > length){
        for(int i = 0; i < length; i ++){
            // layer sound with existing samples
            curr->data[start + i] += data[i];
        }
        // curr->length += length;
        curr->length = SAMPS_PER_NODE;
        // node->length = max(node->length, start + length)
        return;
    }
    else{
        printf("need %d more samples, allocate more\n",
                length - remaining);
    }
}

void writeNote(float (*note)(float), float start, float length){
    return;
}

void playSamples(char *appName, char *streamName){
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
        fprintf(stderr, "Failed to connect to Pulse Audio server");
        return;
    }

    printf("implement! only plays first node's data\n");
    if(pa_simple_write(
                s,
                head->data,
                sizeof(*head->data) * head->length,
                &error) < 0){
        fprintf(stderr, "failed to write to Pulse Audio");
    }
    
    pa_simple_drain(s, &error);
    pa_simple_free(s);
}
