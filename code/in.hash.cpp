#include "hash.hpp"
//////forward declarations begin//////
template<typename T>
class A;
int main();
template<typename T>
int f();
//////forward declarations end//////
template<typename T>
class A{
    typedef A thistype;
    static_assert(Hash::is_pod<T>::value,"DOESN'T MATCH TYPE CONDITION");
    static_assert((10)<(20),"DOESN'T MATCH STATIC CONDITION");
    private: A(thistype* x){
    }
    private: ~A(){
    }
};
enum class X :int {
    X1 = 1, X2 = 2, X3 = 4, X4 = 8
};
int main(){
    signed int a = 0;
    ::printf("Hello");
    return 0;
}
template<typename T>
int f(){
    return 10;
}
