#include <cstdio>
#include <algorithm>
#include <type_traits>

class B{};
class A;
class A : B{
	template <typename T>
	A(T x){}
};

int x=10;
enum E{E1=1,E2=2,E3=4};
int main()
{
	int a,&b=a,**c,**&d=c, &&f=10;
	extern int x;
	printf("%d\n",[&](){return 10;}(),10);
	[&](){};
	if(1&E1&E2) return 0;
}
