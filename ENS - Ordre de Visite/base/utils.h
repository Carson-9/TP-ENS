#ifndef UTILS_H
#define UTILS_H

#include <stdio.h>

#include "graphe.h" 

bool lireligne(FILE* fichier, char *chaine, size_t max);
p_graphe* lecturefichier(char * nomfichier, size_t *nombregraphes);

#endif
