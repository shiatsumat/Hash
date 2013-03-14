#pragma once
#include <cstdio>
#include <algorithm>
#include <tuple>
#include <functional>
#include <list>
#include <forward_list>
#include <type_traits>

namespace hash{
	using namespace std;
	typedef tuple<> Unit;
	const auto unit = make_tuple();
	auto null = nullptr;

	template <typename T>
	struct functional_list{
		typedef tuple<T,functional_list> type0;
		typedef tuple<> type1;
		typedef functional_list thistype;
		union {
			type0* data0;
			type1* data1;
		}uniondata;
		int now;
		const type0& Cons()const{if(now==0)return *(type0*)(uniondata);}
		const type1& Nil()const{if(now==1)return *(type1*)(uniondata);}
		type0& Cons(){if(now==0)return *(type0*)(uniondata);}
		type1& Nil(){if(now==1)return *(type1*)(uniondata);}
		functional_list(){now=-1;}
		~functional_list(){
			switch(now){
				case(0):{delete uniondata.data0;}
				case(1):{delete uniondata.data1;}
			}
		}
	};
	template <typename T>
	functional_list<T> make_Cons(typename functional_list<T>::type0 data){
		functional_list<T> x;
		x.uniondata.data0 = new typename functional_list<T>::type0(data);
		x.now = 0;
		return x;
	}
	template <typename T>
	functional_list<T> make_Nil(typename functional_list<T>::type1 data){
		functional_list<T> x;
		x.uniondata.data1 = new typename functional_list<T>::type1(data);
		x.now = 1;
		return x;
	}

	template <typename T>
	constexpr int static_tuple_size(T& tuple){return tuple_size<T>::value;}
	tuple<int,int,int> x;
	const int n = static_tuple_size(x);
}

