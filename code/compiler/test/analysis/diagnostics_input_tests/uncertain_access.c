int main(void) {
    int *p = (int*) 0x123;
    *p = 12;

    12 + *p;
}
