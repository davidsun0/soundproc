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

/**
 * Initializes the Sound Proc library.
 */
void soundProcInit();

/**
 * Copies sound buffers to the Sound Proc data structure.
 * Provides the core functionality of Sound Proc through an abstraction
 * of a PCM container.
 * @param   data    pointer to PCM data of type SAMPLE_DEPTH
 * @param   length  number of samples of data
 * @param   start   sample offset to write the data at
 */
void writeSamples(SAMPLE_DEPTH * data, int length, int start);

/**
 * Writes a note into the Sound Proc data structure.
 * Gives more abstraction than writeSamples. Works with sound level
 * ideas like notes, envelopes and frequencies.
 * Generates PCM with sample = note * envelope * amplitude
 * @param   note        function pointer to a wave form with input
 *                      between 0 and 2*PI and output between -1 and 1
 * @param   envelope    function pointer to a sound envelope with the
 *                      first parameter as the xth sample and the
 *                      second parameter the total number of samples.
 *                      envelope should return a value between 0 and 1
 * @param   amplitude   the amplitude factor of the note
 * @param   start       starting point of note (in samples)
 * @param   length      length of note (in samples)
 */
void writeNote(
        float (*note)(float),
        float (*envelope)(int, int),
        float freq,
        float amplitude,
        int start,
        int length
        );

/**
 * Plays entire peice through Pulse Audio.
 * @param   appName     title of app given to Pulse Audio
 * @param   streamName  title of stream given to Pulse Audio
 */
void playSamples(char * appName, char * streamName);

#endif  // SOUNDPROC_H
