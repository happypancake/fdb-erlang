#include "erl_interface.h"
#include "ei.h"
#include <stdlib.h>
#include <string.h>

typedef struct {
  const char* name;
  int arity;
  ETERM*(*get_result)(ETERM*);
} APIFunc;

ETERM* api_add(ETERM* cmd);
ETERM* api_double(ETERM* cmd);


static APIFunc API[]= { 
  {"add", 2, api_add},
  {"double", 1, api_double}
};    

ETERM* api_add(ETERM* cmd) 
{
   int val1 = ERL_INT_VALUE(erl_element(2,cmd));
   int val2 = ERL_INT_VALUE(erl_element(3,cmd));
   int result = val1+val2;
   return erl_mk_int(result);
}

ETERM* api_double(ETERM* cmd) 
{
   int val = ERL_INT_VALUE(erl_element(2,cmd));
   int result = val*2;
   return erl_mk_int(result);
}

typedef unsigned char byte;
static ETERM* run(ETERM* buf);
ETERM* api_add(ETERM* cmd);
ETERM* api_double(ETERM* cmd);
byte* read_cmd();
void write_cmd(byte *buf,int len);

int main() {
  byte* buf;
  erl_init(NULL, 0);

  while ((buf=read_cmd()) != NULL) {
    ETERM* cmd = erl_decode(buf);
    free(buf);

    ETERM* result = run(cmd);

    int size = erl_term_len(result)+1;

    byte* output = malloc(size);
    erl_encode(result,output);

    write_cmd(output,size);

    free(output);
    erl_free_compound(cmd);
    erl_free_compound(result);
  }
  return 0;
}


static ETERM* run(ETERM* cmd)
{
  ETERM* fn = erl_element(1,cmd);
  char * funcname = ERL_ATOM_PTR(fn);
  ETERM* result = NULL;
  int  found = 0;
  int api_count = sizeof(API)/sizeof(API[0]);
  int i;
  for (i = 0; i < api_count; i++)
  {
    if (strlen(funcname) == strlen(API[i].name) &&
        strncmp(funcname,API[i].name,strlen(funcname))== 0 &&
        ERL_TUPLE_SIZE(cmd)-1 == API[i].arity)
    {
      found = !found;
      result = API[i].get_result(cmd);
      break;
    }
  }
  if (!found) {
   ETERM* members[] = {erl_mk_atom("error"),erl_mk_atom("badmatch")};
   result = erl_mk_tuple(members,2);
  }
  
  erl_free_term(fn);
  return result;
}

