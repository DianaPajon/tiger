#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

typedef struct {
    long length;
    unsigned char chars[0];
} string;

string *consts[256];
string empty = {0};

long *_initArray(long size, long init)
{
    long i;
    long *a = malloc((size+1) * sizeof(long));
	a[0] = size;
    for (i = 1; i <= size; i++)
		a[i] = init;
    return a+1;
}
void _checkIndexArray(long *a, long i)
{
	if(i<0 || i>=a[-1]) {
		fprintf(stderr, "indice %ld excedido!\n", i);
		exit(-1);
	}
}
long *_allocRecord(long ctos, ...)
{
    long i;
    long *p, *a;
	va_list va;
    p = a = malloc(ctos*sizeof(long));
	va_start(va, ctos);
    for (i = 0; i < ctos; i ++)
		*p++ = va_arg(va, long);
    return a;
}
void _checkNil(long* r)
{
	if(r==0) {
		fprintf(stderr, "Nil!\n");
		exit(-1);
	}
}
long _stringCompare(string *s, string *t)
{
    long i;
    if(s == t)
		return 0;
    for(i = 0; i<s->length && i<t->length; i++)
		if(s->chars[i]!=t->chars[i])
		    return s->chars[i]-t->chars[i];
	return s->length-t->length;
}
void print(string *s)
{
    long i;
    unsigned char *p = s->chars;
    for (i = 0; i < s->length; i++, p++)
		putchar(*p);
}
void flush()
{
    fflush(stdout);
}
long ord(string *s)
{
    if (s->length != 1) {
		fprintf(stderr, "ord: string inadecuada\n");
		exit(-1);
	}
    else
		return s->chars[0];
}
string *chr(long i)
{
    if (i < 0 || i >= 256) {
		printf("chr(%ld) out of range\n", i);
		exit(1);
    }
    return consts[i];
}
long size(string *s)
{
    return s->length;
}
string *substring(string *s, long first, long n)
{
    if (first < 0 || first + n > s->length) {
		fprintf(stderr, "substring([%ld],%ld,%ld) out of range\n",
				s->length, first, n);
		exit(1);
    }
    if (n == 1)
		return consts[s->chars[first]];
    {
		string *t = malloc(sizeof(string) + n);
		long i;
		t->length = n;
		for (i = 0; i < n; i++)
		    t->chars[i] = s->chars[first + i];
		return t;
    }
}
string *concat(string *a, string *b)
{
    if (a->length == 0)
		return b;
    else if (b->length == 0)
		return a;
    else {
		long i, n = a->length + b->length;
		string *t = malloc(sizeof(string) + n);
		t->length = n;
		for (i = 0; i < a->length; i++)
		    t->chars[i] = a->chars[i];
		for (i = 0; i < b->length; i++)
		    t->chars[i + a->length] = b->chars[i];
		return t;
    }
}
long not(long i)
{
    return !i;
}
string *getstr()
{
    int i = getc(stdin);
    if (i == EOF)
		return &empty;
    else
		return consts[i];
}
#if defined(__DEBUG__)
long _tigermain(long l)
{
	return 0;
}
#endif
int main()
{
	extern long _tigermain(long);
    int i, lelem=sizeof(string)+1;;
	string *p=malloc(256*lelem);
    for (i = 0; i < 256; i++) {
		consts[i]=p+i*lelem;
		consts[i]->length = 1;
		consts[i]->chars[0] = i;
    }
    return _tigermain(0 /* static link!? */ );
}
