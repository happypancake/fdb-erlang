#include <stdlib.h>
#include <unistd.h>

typedef unsigned char byte;

int read_exact(byte* b,int cnt);
int write_exact(byte *buf, int len);

byte *read_cmd()
{
    int len;
    byte buffer[3], *resultBuffer;

    if (read_exact(buffer, 2) != 2)
        return NULL;

    len = (buffer[0] << 8) | buffer[1];

    resultBuffer = (byte *)malloc(sizeof(byte)*len);
    read_exact(resultBuffer, len);
    return resultBuffer;
}

int write_cmd(byte *buf, int len)
{
  byte li;

  li = (len >> 8) & 0xff;
  write_exact(&li, 1);
  
  li = len & 0xff;
  write_exact(&li, 1);

  return write_exact(buf, len);
}

int read_exact(byte *buf, int len)
{
    int i, got=0;

    do
    {
        if ((i = read(0, buf+got, len-got)) <= 0)
            return(i);
        got += i;
    } while (got<len);

    return(len);
}

int write_exact(byte *buf, int len)
{
  int i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote<len);

  return (len);
}
