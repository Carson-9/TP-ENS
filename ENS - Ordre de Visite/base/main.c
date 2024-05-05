#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "utils.h"
#include "graphe.h"

typedef unsigned char u8;
typedef unsigned int u16;
typedef unsigned long u32;
typedef unsigned long long u64;

typedef char i8;
typedef int i16;
typedef long i32;
typedef long long i64;

typedef float f32;
typedef double f64;



/// @brief Returns whether a cycle was found in the graph
/// @param g : (p_graphe) The graph to check
/// @param sommet : (u16) The current node to check
/// @param hierarchy : (i16*) A table holding information regarding dft (Signed!)
/// @param visite : (bool*) A table holding information on previously visited nodes
/// @param step : (u16*) A number representing tne current step 'time'

bool visite_rec(p_graphe g, u16 sommet, u16 caller, i16* hierarchy, bool* visite, u16* step){
  visite[sommet] = true;
  p_arete voisins = g->sommets[sommet]->premierearete;
  step++;

  bool contains_cycle = false;

  while(voisins){   // Implicit bool conversion
    if(!visite[voisins->idvoisin]){
      // Has to evaluate visite_rec -> no lazy eval possible!
      contains_cycle = visite_rec(g, voisins->idvoisin, sommet, hierarchy, visite, step) || contains_cycle;
    }
    else if((voisins->idvoisin != caller) && (hierarchy[voisins->idvoisin] < 0)){
      // Found a node currently being treated but not finished : An ancestor of current node -> cycle
      // Visited acts as the discovery date, true means discovery < current
      contains_cycle = true; 
    }
    
    voisins = voisins->narete;
  }

  hierarchy[sommet] = *step;
  return contains_cycle;
}


u8 graphe_est_acyclique_connexe(p_graphe g){
  bool* visited = (bool*)malloc(g->taille * sizeof(bool));
  i16* hierarchy = (i16*)malloc(g->taille * sizeof(i16));
  u16* step = (u16*)malloc(sizeof(u16));

  bool contains_cycle = false;
  bool connexe = true;
  *step = 0;

  for(u16 i = 1; i < g->taille; i++){
    visited[i] = false;  // Ignore 0 as dfs will start at 0
    hierarchy[i] = -1;
  }
  hierarchy[0] = -1;

  contains_cycle = visite_rec(g, 0, 0, hierarchy, visited, step);
  
  for(u16 i = 1; i < g->taille; i++){
    if(!visited[i]){
      connexe = false;
      contains_cycle = visite_rec(g, i, i, hierarchy, visited, step) || contains_cycle;
    }
  }

  free(visited);
  free(hierarchy);
  free(step);
 
  //printf("Current graph is %d Acyclical and %d Connected\n", !contains_cycle, connexe);

  if(connexe){
    if(!contains_cycle) return 'b'; // Both
    else return 'c'; // Connex only
  }
  else if(!contains_cycle) return 'a'; // Acyclical only
  return 'n'; // None

}

void compte_acyclique_connexe(p_graphe* g_list, u16 nb_graphe){
  u16 foret = 0;
  u16 arbre = 0;
  u8 res;

  for(u16 i = 0; i < nb_graphe; i++){
    res = graphe_est_acyclique_connexe(g_list[i]);
    if(res == 'b'){
      foret++; arbre++;
    }

    if(res == 'a') foret++;

  }

  printf("   * Question 6 : La liste contient %d Arbres et %d Forêts!\n", arbre, foret);
}


int main() {
  printf("Initialisation du programme...");
  size_t nombregraphes;
  p_graphe* graphes = NULL;
  graphes = lecturefichier("graphes.txt", &nombregraphes);
  printf(" %zu graphes chargés depuis le fichier.\n",nombregraphes);
  
  printf("Affichage du premier graphe à titre d'exemple :\n");
  affichergraphe(graphes[0]);

  /* Implémentez vos algorithmes dans ce fichier, nous vous recommendons 
   * de ne *pas* utiliser de fichiers auxilliaires afin d'éviter de devoir 
   * modifier le makefile.
   *
   * Vous pouvez être amené à devoir modifier la structure du graphe 
   * cf. graphe.h et graphe.c pour les initialisations si besoin.
   *
   * Pensez à bien copier les fichiers qui vous ont été fournis, en effet, 
   * il serait dommage de les supprimer par erreur.
   * */

  compte_acyclique_connexe(graphes, nombregraphes);


  return 0;
}
