#ifndef SOUNDPROC_H
#define SOUNDPROC_H

#include <stdint.h>

#define SAMPS_PER_NODE (128 * 1024)

#define SAMPLE_RATE 48000
#define CHANNELS 1

#define SAMPLE_DEPTH int16_t
// corresponding Pulse Audio format
// S16NE for signed 16 bit native endian
#define PA_FORMAT PA_SAMPLE_S16NE


void writeBuffer(void * buffer, int startsamp);
void writeNote(float (*note)(float), float start, float length);

void init();
void writeSamples(SAMPLE_DEPTH * data, int length, int start);
void playSamples(char * appName, char * streamName);

#endif
