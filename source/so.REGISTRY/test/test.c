
#include <stdio.h>
#include <stdlib.h>

#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>



int fun(int num){

  num = num + 1;

  return(num);
}



int main(){

  int xx = 1;
  int yy;

  fprintf(stdout,"HELLO\n");

  fprintf(stdout,"%d\n",xx);

  yy = fun(xx);

  fprintf(stdout,"%d\n",xx);
  fprintf(stdout,"%d\n",yy);

  return(0); 
}




