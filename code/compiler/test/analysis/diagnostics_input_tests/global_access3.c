void foo(void) {
    static int x = 5;
}

void bar(void) {
    static int x;
    x = 5;
}

int baz(void) {
    static int x;
    return x + 1;
}

int main(void) {
    foo();
    bar();
    return baz();
}
