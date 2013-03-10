#include <cstdio>
#include <algorithm>

class B final {};
struct A;
struct A{
	template <typename T>
	A(T x){}
	struct C{C();~C();};
};
A::C::~C(){}

int x=10;
enum E{E1=1,E2=2,E3=4};
int main()
{
	extern int x;
	if(1&E1&E2) return 0;
}
