#include <stddef.h>
#include <stdint.h>
int a;int bb;/*comment for cc*/int ccc;
/*
123456789012345678901234567890
*/
typedef uintptr_t ngx_uint_t;
void f(void){}void g(void){}/*comment for h*/void h(void){}int d;int e;;;;;;;;int another;
//comment for ngx_hash_init
void
ngx_hash_init(void)
{
    size_t len;
    ngx_uint_t align = len & ~sizeof(void *);
}
int
anotherfunc(void)
{
    int var = 0;
    return 4 + var;
}
int
funcholdingdefine(void)
{
#define FOO 4000+50
    return FOO;
}
// comment before multiple decl
int a1, a2, a3;
void forwarddeclbeforeotherdef(int x);
void funcafterdecl(int n) {}
/*real defn*/
void forwarddeclbeforeotherdef(int x) { }

