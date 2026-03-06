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
another_func(void)
{
    int var = 0;
    return 4 + var;
}
int
func_holding_define(void)
{
#define FOO 4000+50
    return FOO;
}
// comment before multiple decl
int a1, a2, a3;
void forward_decl_before_other_def(int x);
void func_after_decl(int n) {}
/*real defn*/
void forward_decl_before_other_def(int x) { }

