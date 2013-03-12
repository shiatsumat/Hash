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
    private: typedef A thistype;
    static_assert(hash::is_pod<T>::value,"DOESN'T MATCH TYPE CONDITION");
    static_assert((10)<(20),"DOESN'T MATCH STATIC CONDITION");
    #line 2
    public: A(thistype* x){
    }
    #line 3
    private: virtual ~A(){
    }
    #line 4
    public: T fun(T x, T y){
        #line 5
        return (x)*(y);
    }
};
#line 6
template<typename T>
class XOO;
#line 8
enum class X;
#line 9
enum class X :int {
    X1 = 1, X2 = 2, X3 = 4, X4 = 8
};
#line 10
int main(){
    #line 12
    static const int a = (0);
    #line 13
    int returnNotReservedWord;
    #line 14
    (returnNotReservedWord)=(([&](){
        return hash::unit;
    }()),(10));
    #line 15
    printf("Hello %d", returnNotReservedWord);
    #line 16
    if(bool x = (true)){
        #line 17
        printf("foo\n");
    }
    #line 17
    typedef int foo;
    #line 18
    ;
    #line 18
    hash::vector<int> x;
    #line 19
    (x).push_back(0);
    #line 19
    if((x).size()==3&&true&&((x).at(1))==2&&true){
        auto& a=((x).at(0));
        auto& q=((x).at(2));
        printf("%d", (a)+(q));
    }else if(true&&((x).at(0))==0){
        printf("foo");
    }else{
        throw "No Match For Pattern";
    }
    #line 22
    ;
    #line 22
    return returnNotReservedWord;
}
#line 24
template<typename T, int x>
int f(T y){
    return ((10)+(x))+(--(-(++(+(((((y)++)++)--)--)))));
}
#line 25
typedef int foo;
#line 26
;
