#include <stdlib.h>

#include "utils.h"
#include "graphe.h"

bool lireligne(FILE* fichier, char *chaine, size_t max) {
   size_t i;
   for (i = 0; i < max - 1; ++i) {
     char c;
     if (fscanf(fichier, "%c", &c) != 1)
       return false;
     else if (c == '\n')
       break;
     chaine[i] = c;
   }
   chaine[i] = '\0';
   return true;
}

p_graphe* lecturefichier(char * nomfichier, size_t *nombregraphes) {
  FILE* fichier = NULL;
  fichier = fopen(nomfichier,"r");
  p_graphe* graphes = NULL;
  if(fichier==NULL) {
    printf("Erreur sur l'ouverture du fichier.\n");
  } else {
    fscanf(fichier, "%zu\n", nombregraphes);
    graphes = (p_graphe*) malloc((*nombregraphes)*sizeof(p_graphe));
    size_t i;
    for(i = 0; i < *nombregraphes; i++) {
      creergraphe(fichier, graphes, i);
    }
    fclose(fichier);
  }
  return graphes;
}
