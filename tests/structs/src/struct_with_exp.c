
struct s { 
    int i; 
};

void struct_with_exp(const unsigned int buffer_size, int buffer[const]){
    if (buffer_size < 1) return;
    
    struct s *p;
    int j = 42;
    p = &((struct s){j++}); // Compound literal with address-of operator and post-increment operator

    buffer[0] = p->i;
}
