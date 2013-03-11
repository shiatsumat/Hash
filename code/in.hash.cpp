#line 0
#include "hash.hpp"
//////forward declarations begin//////
template<typename T>
struct A;
int main();
template<typename T, int x>
int f(T y);
//////forward declarations end//////
#line 1
template<typename T>
struct A{
    typedef A thistype;
    static_assert(hash::is_pod<T>::value,"DOESN'T MATCH TYPE CONDITION");
    static_assert((10)<(20),"DOESN'T MATCH STATIC CONDITION");
    public: A(thistype* x){
    }
    public: ~A(){
    }
    public: T fun(T x, T y){
        #line 5
        return (x)*(y);
    }
};
#line 7
enum class X :int {
    X1 = 1, X2 = 2, X3 = 4, X4 = 8
};
#line 9
int main(){
    #line 11
    int a = (0);
    #line 12
    int returnNotReservedWord;
    #line 13
    (returnNotReservedWord)=(([&](){
        return hash::unit;
    }()),(10));
    #line 14
    printf("Hello %d", returnNotReservedWord);
    #line 15
    if(bool x = (true)){
        #line 16
        printf("foo\n");
    }
    #line 16
    return returnNotReservedWord;
}
#line 18
template<typename T, int x>
int f(T y){
    return ((10)+(x))+(--(-(++(+(((((y)++)++)--)--)))));
}
