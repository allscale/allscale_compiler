int* foo(void);

int main(void) {
    int* p = foo();
    *p = 21;
}
