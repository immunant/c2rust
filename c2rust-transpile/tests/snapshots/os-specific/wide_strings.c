#include <wchar.h>

wchar_t static_array[] = L"x";
wchar_t static_array_longer[3] = L"x";
wchar_t static_array_shorter[1] = L"xy";
// Currently broken, see #1571
// const wchar_t *static_ptr = L"x";

void func() {
    wchar_t array[] = L"x";
    wchar_t array_longer[3] = L"x";
    wchar_t array_shorter[1] = L"xy";

    // Currently broken, see #1571
    // const wchar_t *ptr = L"x";

    size_t len = wcslen(array);
}
