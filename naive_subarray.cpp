/**
 * Seem to be sqrt decoposition technique.
 */

#include <bits/stdc++.h>
#define pc putchar
#define gc() getchar()
#define pb emplace_back

/**
 * The the next int form stdin
 */
inline int read()
{
	int now=0,f=1; char c=gc();
	for(;!isdigit(c);c=='-'&&(f=-1),c=gc());
	for(;isdigit(c);now=now*10+c-48,c=gc());
	return now*f;
}

typedef long long LL;
typedef unsigned long long ull;

/**
 * N is the maximum number of elements in the array
 * M is 
 * B is the number of buckets used for sqrt decomposition
 * B_size is the size of each bucket
 */

const int N = 2e5+5, M = 1e5+5, B = 450, B_size = 450;
const int mod = 2099;

/**
 * las is the last index where the element appear
 * bel is the index of the bucket in which the element belong to
 * L is the left bound of the bucket
 * R is the right bound of the bucket
 */
int las[M], bel[N], L[B], R[B];

/**
 * val is the hash value of each element
 * seq is TODO
 * tag is TODO
 */
ull val[M], seq[N], tag[B];

std::mt19937_64 Rand(19511016);

struct Hash
{
	int Enum, Time, time[mod+2], H[mod+2], nxt[B_size+2], cnt[B_size+2];
	ull to[B_size+2];

	inline void AE(int u, ull v, int t)
	{
		to[++Enum] = v;
		nxt[Enum] = H[u];
		cnt[Enum] = t;
		H[u] = Enum;
	}
	void Clear() {
		++Time, Enum=0;
	}

	/**
	 * v: number of element
	 */
	void Init(int v)
	{
		Clear();
		// time[0] = Time
		// H[0] = 0
		Get_head(0); 
		AE(0, 0, v);
	}
	
	/**
	 * Lazy update here?
	 */
	int Get_head(int x)
	{
		return time[x] == Time ? H[x] : (time[x] = Time, H[x] = 0);
	}
	
	int Query(ull v)
	{
		for (int i = Get_head(v % mod); i; i = nxt[i])
			if (to[i] == v) return cnt[i];
		return 0;
	}
	void Insert(ull v)
	{
		for (int i = Get_head(v % mod); i; i = nxt[i]) if (to[i]==v) {
			++cnt[i];
			return;
		}
		AE(v % mod, v, 1);
	}
} cnt[B];

/**
 * p is the last index of the element
 * v is the hash value of the element
 */
void Update(int p, ull v)
{
	if (!p) return;
	int b = bel[p];
	for(int i=1; i<b; ++i) tag[i]^=v;
	
	cnt[b].Clear();
	for(int i=L[b]; i<=p; ++i) cnt[b].Insert(seq[i]^=v);
	for(int i=R[b]; i>p; --i) cnt[b].Insert(seq[i]);
}

/**
 * p is the index of the element
 */
int Query(int p)
{
	int b = bel[p], ans = 0; ull v = tag[b];
	for(int i=1; i<b; ++i) ans+=cnt[i].Query(tag[i]);
	for(int i=L[b]; i<=p; ++i) ans+=(seq[i]==v);
	return ans;
}

template<std::size_t S>
LL solve(std::array<int,S> A)
{
	int n=A.size();
	for(int i=1; i<=n; ++i) seq[i]=0, las[A[i-1]]=0;
	
	/**
	 * For each element in the array, generate a hash for that number
	 */
	for(int i=1; i<=n; ++i) if(!val[A[i-1]]) val[A[i-1]]=Rand();

	/**
	 * Tag each element with a bucket
	 */
	for(int i=1; i<=n; ++i) bel[i]=(i-1)/B_size+1;
	
	/**
	 * How many buckets?
	 */
	int tot=bel[n];

	/**
	 * For each bucket: find the index of the left element, and the index of the right element
	 */
	for(int i=1; i<=tot; ++i) L[i]=(i-1)*B_size+1, R[i]=i*B_size;
	
	/**
	 * The last bucket may have less than 450 elements
	 */
	R[tot]=std::min(R[tot], n);

	/**
	 * For each bucket: initialize the count. But what about the tag?
	 */
	for(int i=1; i<=tot; ++i) tag[i]=0, cnt[i].Init(R[i]-L[i]+1);

	LL ans = 0;
	/**
	 * For each element in the array: update the structure with 
         * the last index where the element appeared
	 * And the element hash value
         * Afterward, we do query and update las array
	 */
	for (int i=1; i<=n; ++i) {
		Update(las[A[i-1]], val[A[i-1]]); 
		ans += Query(i); 
		las[A[i-1]] = i;
	}
	return ans;
}

int main()
{
	std::array<int, 27> A2 = {6,1,7,4,6,7,1,4,7,1,4,6,6,7,4,1,6,4,7,1,4,5,3,2,1,6,9};
	printf("%lld\n",solve(A2)); //114
	std::array<int, 15> A = {2, 5, 2, 3, 6, 7, 8, 23, 23, 13, 65, 31, 3, 4, 3};
	printf("%lld\n",solve(A)); //53	
	return 0;
}