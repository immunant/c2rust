// Unlike struct or union, there are no forward-declared enums in C. However, 
// the Enlightenment Foundation Libraries, and possibly other code, contains
// code containing this pattern:  

typedef enum _Evas_Filter_Support Evas_Filter_Support;

struct _Evas_Func
{
    Evas_Filter_Support (*gfx_filter_supports) (void *engine, void *cmd);
};

// dummy item imported by `test_enums.rs`
int foo(int i) { return i;}