// ========================= START PRELUDE ===================== //
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// Utilitaires

#define MAX_MOTIF_LEN 1000

// Le type des chaines null-terminated contenant ACGT.
typedef char* string;

// Un vecteur de string
struct vector_s {
  int len;
  string * strs;
};
typedef struct vector_s vector;

// `allocate_vector()` aloue un vecteur vide
vector* allocate_vector() {
  string *all_lines = malloc(MAX_MOTIF_LEN * sizeof(string));
  for(int i=0;i<MAX_MOTIF_LEN;i++) { all_lines[i] = NULL; };
  vector* v = malloc(sizeof(vector));
  v->len = 0;
  v->strs = all_lines;
  return v;
}

// `add_string(v,s)` ajoute la string `s` au vecteur `v`.
void add_string(vector* v, string s) {
  v->strs[v->len] = s;
  v->len = v->len +1;
}

// `read_text(filename)` lit la chaine dans le fichier `filename` et renvoi
// une string
string read_text(char* filename) {
  FILE *fptr = fopen(filename, "r");
  
  char * line = NULL;
  size_t len = 0;
  int read = getline(&line, &len, fptr);
  if (read != -1) { return line; } { return NULL; }
}

// `read_motif(filename)` lit le motif dans le fichier `filename` et renvoi
// un vector
vector* read_motif(char* filename) {
  FILE *fptr = fopen(filename, "r");

  vector* m = allocate_vector();

  string line = NULL;  
  size_t len = 0;
  size_t read;

  while ((read = getline(&line, &len, fptr)) != -1) {
    if (line[read-1] == '\n') { line[read-1] = '\0'; };
    add_string(m, line);
    line = NULL;
  }
  return m ;
}

// `string_of_node(depth, pos, repr, a, c, g, t)` converti le noeud d'un patricia trie fourni en 
// une string à l'indentation `depth`, étant donné les informations `pos` et `repr` et les 
// strings représentant les descendants a, c, g, t
string string_of_node(int depth, unsigned pos, string repr,
                      string a, string c, string g, string t) {
  string res = malloc(8*1000);
  sprintf(res, "pos=%d repr=%s\n", pos, repr);
  char blank[depth+1];
  for (int i = 0;i<depth;i++) { blank[i]=' ';};
  blank[depth]='\0';
  if (strcmp(a,"")!=0) { strcat(res, blank); strcat(res, "└A→ "); strcat(res, a);};
  if (strcmp(c,"")!=0) { strcat(res, blank); strcat(res, "└C→ "); strcat(res, c);};
  if (strcmp(g,"")!=0) { strcat(res, blank); strcat(res, "└G→ "); strcat(res, g);};
  if (strcmp(t,"")!=0) { strcat(res, blank); strcat(res, "└T→ "); strcat(res, t);};
  return res;
}

// ========================= STOP PRELUDE ===================== //


typedef struct pat_trie_t{
  int pos;
  string* word_ref;
  struct pat_trie_t* a;
  struct pat_trie_t* c;
  struct pat_trie_t* g;
  struct pat_trie_t* t;
} pat_node;

typedef pat_node* pat_trie;


string pat_string(pat_trie t, int depth){

  if(t == NULL) return "\0";

  if(t->pos == -1)
    return string_of_node(0, t->pos, *t->word_ref, NULL, NULL, NULL, NULL);
  
  else return string_of_node(depth, t->pos, *t->word_ref, pat_string(t->a, depth + 4), pat_string(t->c, depth + 4), pat_string(t->g, depth + 4), pat_string(t->t, depth + 4));
}

void print_pat(pat_trie t){
  printf("%s\n", pat_string(t, 0));
}


pat_trie pat_of_word(string w){
  pat_trie new_pat_t = (pat_trie)malloc(sizeof(pat_node));
  new_pat_t->pos = -1;
  new_pat_t->word_ref = w;
  new_pat_t->a = NULL; new_pat_t->c = NULL; new_pat_t->g = NULL; new_pat_t->t = NULL;

  return new_pat_t;
}

pat_trie make_pat(vector* motif){
  if(motif->len == 0) return NULL;
  if(motif->len == 1) return pat_of_word(motif->strs[0]);

  vector* prefixes_communs = allocate_vector();
  int longest_pref_len = 0;
  int current_pref_len = 0;
  string longest_pref = "";

  pat_trie* word_nodes = (pat_trie*)malloc(motif->len * sizeof(pat_trie));

  for(int word = 0; word < motif->len; word++) word_nodes[word] = pat_of_word(motif->strs[word]);

  for(int word = 0; word < motif->len; word++){
    for(int prev_word = 0; prev_word < motif->len; prev_word++){
      while()
    }
  }

}

int main(int argc, char *argv[]){
  print_pat(NULL);
  return 0;
}
