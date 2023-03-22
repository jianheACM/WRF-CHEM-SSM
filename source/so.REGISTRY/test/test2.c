
#include <stdio.h>
#include <stdlib.h>

#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>

int main(){

  char* ppp[];

  fprintf(stdout,"%p\n",ppp);

  ppp++;

  fprintf(stdout,"%p\n",ppp);

  ppp++;

  fprintf(stdout,"%p\n",ppp);

  ppp++;

  fprintf(stdout,"%p\n",ppp);

  ppp++;

  fprintf(stdout,"%p\n",ppp);

  ppp++;

  fprintf(stdout,"%p\n",ppp);

  ppp++;

  fprintf(stdout,"%p\n",ppp);

  ppp++;

  fprintf(stdout,"%p\n",ppp);

  ppp++;

  fprintf(stdout,"%p\n",ppp);

  return(0); 
}




