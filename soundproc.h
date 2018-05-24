#ifndef SOUNDPROC_H
#define SOUNDPROC_H

void writeBuffer(void * buffer, int startsamp);
void writeNote(float (*note)(float), float start, float length);

#endif
