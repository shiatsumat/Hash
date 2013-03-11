#pragma once
#include <cstdio>
#include <algorithm>
#include <tuple>
#include <functional>
#include <type_traits>

namespace hash{
	using namespace std;
	typedef tuple<> Unit;
	const auto unit = make_tuple();

	template <typename T>
	class functional_list{
		typedef functional_list thistype;
		typedef tuple<T,functional_list> type0;
		typedef tuple<> type1;
		union{
			type0* data0;
			type1* data1;
		}data;
		int now;
		const type0& Cons()const{if(now==0)return *(type0*)(data);}
		const type1& Nil()const{if(now==1)return *(type1*)(data);}
		type0& Cons(){if(now==0)return *(type0*)(data);}
		type1& Nil(){if(now==1)return *(type1*)(data);}
	};
}

