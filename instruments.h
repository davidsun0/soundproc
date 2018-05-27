#ifndef SP_INSTRUMENTS_H
#define SP_INSTRUMENTS_H

void playNote(
        float (*note)(float),
        float (*envelope)(int, int),
        float freq,
        float volume,
        float start,
        float length
        );

void setASDR(float atk, float dcy, float stn, float rls);
float ASDR(int x, int length);

float triangle(float theta);
float square(float theta);
float sine(float theta);
float whiteNoise(float theta);

#endif  // INSTRUMENTS_H
