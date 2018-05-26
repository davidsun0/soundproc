#ifndef SP_INSTRUMENTS_H
#define SP_INSTRUMENTS_H

float triangle(float theta);
float square(float theta);
float sine(float theta);
float whiteNoise(float theta);

void setEnvelope(float atk, float dcy, float stn, float rls);
float envelope(int x, int length);

#endif  // INSTRUMENTS_H
