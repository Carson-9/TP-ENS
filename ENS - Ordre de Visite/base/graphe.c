#include <stdio.h>
#include <stdlib.h>

#include "graphe.h"
#include "utils.h"

#define MAX_SIZE 128 /* éviter les hyper-arêtes dont l'encodage ne tient pas sur MAX_SIZE caractères */

/* ajoute une arête de façon unidirectionnelle: src -> tgt
 * la liste d'adjacence de src reste triée par identifiants croissants */
void ajouterarete(p_graphe graphe, size_t src, size_t tgt) {
  p_arete nouvellearete = (p_arete) malloc(sizeof(struct arete));
  nouvellearete->idvoisin = tgt;
  nouvellearete->narete = NULL;

  if (graphe->sommets[src]->premierearete == NULL) {
    /* ajout en première position */
    graphe->sommets[src]->premierearete = nouvellearete;
  } else if (graphe->sommets[src]->premierearete->idvoisin == tgt) {
    printf("Erreur doublon.\n"); /* ne devrait jamais être affiché */
  } else if (graphe->sommets[src]->premierearete->idvoisin > tgt) {
    /* ajout en première position */
    nouvellearete->narete = graphe->sommets[src]->premierearete;
    graphe->sommets[src]->premierearete = nouvellearete;
  } else {
    p_arete actuel = graphe->sommets[src]->premierearete;
    /* sauter les sommets avec valeur d'identifiant inférieur */
    while(actuel->narete != NULL && actuel->narete->idvoisin < tgt) {
      actuel = actuel->narete;
    }
    if(actuel->narete == NULL) {
      /* ajout après le sommet actuel */
      actuel->narete = nouvellearete;
    }
    else if (actuel->narete->idvoisin == tgt) {
      printf("Erreur doublon.\n"); /* ne devrait jamais être affiché */
    } else {
      /* ajout après le sommet actuel */
      nouvellearete->narete = actuel->narete;
      actuel->narete = nouvellearete;
    }
  }
}

void creergraphe(FILE* fichier, p_graphe* graphes, size_t rang) {
  /* lecture des informations du graphe à partir du fichier */
  char verif[MAX_SIZE];
  lireligne(fichier,verif,MAX_SIZE); /* la clé est là pour séparer les graphes entre eux */
  size_t nbsommets, nbaretes, nbhyperaretes;
  fscanf(fichier,"%zu %zu %zu\n", &nbsommets, &nbaretes, &nbhyperaretes);
  /* allocation de la structure du graphe */
  p_graphe nouveaugraphe = (p_graphe) malloc(sizeof(struct graphe));
  graphes[rang] = nouveaugraphe;
  graphes[rang]->taille = nbsommets;
  /* allocation des structures des sommets */
  p_sommet* nouveauxsommets = (p_sommet*) malloc(nbsommets*sizeof(p_sommet));
  graphes[rang]->sommets = nouveauxsommets;
  size_t i;
  for(i = 0; i < nbsommets; i++) {
    /* allocation d'une structure de sommet */
    p_sommet nouveausommet = (p_sommet) malloc(sizeof(struct sommet));
    (graphes[rang]->sommets[i]) = nouveausommet;    
    (graphes[rang]->sommets[i])->identifiant = i;
  }
  /* lecture des arêtes */
  for(i = 0; i < nbaretes; i++) {
    size_t src, tgt;
    fscanf(fichier, "%zu %zu\n", &src, &tgt); 
    /* ajouter les deux arêtes car on est non dirigé */   
    ajouterarete(graphes[rang], src, tgt);
    ajouterarete(graphes[rang], tgt, src);
  }
  /* ignorer les hyper-arêtes */
  for(i = 0; i < nbhyperaretes; i++) {
    lireligne(fichier,verif,MAX_SIZE);
  }
}

/* affichage d'un graphe
 * peut être utilisée à titre d'illustration sur un des tout premiers 
 * graphes du jeu de données, car ils sont petits */
void affichergraphe(p_graphe graphe) {
  printf("%zu sommets :\n", graphe->taille);
  size_t j;
  for(j = 0; j < graphe->taille; j++) {
    printf("  %zu -> ", j);
    p_arete vers = graphe->sommets[j]->premierearete;
    while(vers != NULL) {
      printf("%zu ", vers->idvoisin);
      vers = vers->narete;
    }
    printf("\n");
  }
}
