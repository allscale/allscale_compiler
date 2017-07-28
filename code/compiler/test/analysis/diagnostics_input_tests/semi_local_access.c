int y;

int foo(void);

int main(void) {
    int x = 5;
    int* b;
    if (foo()) {
        b = &x;
    } else {
        b = &y;
    }

    *b = 42;
    *b = *b + 1;
    (*b)++;
}
