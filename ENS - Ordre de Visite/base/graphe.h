#ifndef GRAPHE_H
#define GRAPHE_H

#include <stdbool.h>

struct arete {
  size_t idvoisin;
  struct arete* narete;
};
typedef struct arete* p_arete;

struct sommet {
  /* informations spécifiques aux sommets */
  size_t identifiant;
  p_arete premierearete;
};
typedef struct sommet* p_sommet;

struct graphe {
  size_t taille;
  p_sommet* sommets;
};
typedef struct graphe* p_graphe;

void ajouterarete(p_graphe graphe, size_t src, size_t tgt);
void creergraphe(FILE* fichier, p_graphe* graphes, size_t rang);
void affichergraphe(p_graphe graphe);

#endif
