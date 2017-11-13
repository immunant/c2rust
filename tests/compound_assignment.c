void entry(unsigned buffer_size, int buffer[])
{
    buffer[0] += 3;
    buffer[1] -= 5;
    buffer[2] *= 5;
    buffer[3] %= 5;
    buffer[4] ^= 5;
    buffer[5] <<= 5;
    buffer[6] >>= 5;
    buffer[7] |= 5;
    buffer[8] &= 5;
    
    buffer[9] = (buffer[10] = 7);
    buffer[11] = (buffer[9] *= 6);
}
