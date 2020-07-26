#pragma once

#include <cstdio>

FILE *mypopen(const char *command, const char *mode);
int mypclose(FILE *stream);
