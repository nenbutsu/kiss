#include <stdio.h>
#include <windows.h>

typedef int (*add_t)(int, int);

int main() {
  HMODULE hModule = LoadLibrary("add.dll");
  add_t add = (add_t)GetProcAddress(hModule, "add");
  printf("1 + 2 = %d\n", add(1, 2));
  FreeLibrary(hModule);
  return 0;
}
