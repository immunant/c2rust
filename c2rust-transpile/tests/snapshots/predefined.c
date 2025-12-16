void predefined() {
    int line = __LINE__; // Update the snapshot if this gets moved
    const char* file_name = __FILE_NAME__;
    const char* func = __func__;
}

const char *extension_operator() {
    return __extension__ __PRETTY_FUNCTION__;
}
