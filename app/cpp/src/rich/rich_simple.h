#include <cstdio>

const int maxN = 100500500;//124500500;
int to[2][maxN], // graph with 2 types of edges (0 and 1)
	smart[2][maxN]; // smart sufflinks
char len[maxN]; // lengthes of palindrome in all vertices
char s[maxN]; // main string
bool was[maxN]; // visited vertices
int now, //current vertex in graph
    size, //size of graph
    N; //limit to size of string


void init()
{
	len[1] = -1;
	smart[0][2] = smart[1][2] = 1;
	s[0] = 2;
	// letter 0
	len[3] = 1;
	smart[1][3] = 1;
	smart[0][3] = 2;
	to[0][1] = 3;

	//letter 1
	len[4] = 1;
	smart[0][4] = 1;
	smart[1][4] = 2;
	to[1][1] = 4;

	size = 5;
}

long long cnt[100]; //answer to the problem

void dfs(int i, int state)
{
	if(i == N + 1) return;
	for(int bit = 0; bit < 2; bit++)
	{
		int * go = to[bit];
		s[i] = bit;
		int first = i - len[state] - 1;
		int now = (bit == s[first]) ? state : smart[bit][state];
		int nxt = go[now];
		if(nxt == 0)
		{
			nxt = size++;
			len[nxt] = len[now] + 2;
			int sb = go[smart[bit][now]];
			int B = s[i - len[sb]];
			smart[B][nxt] = sb; 
			smart[B ^ 1][nxt] = smart[B ^ 1][sb];
			go[now] = nxt;
		}
		if(!was[nxt])
		{
			was[nxt] = 1;
			cnt[i]++;
			dfs(i + 1, nxt);
			was[nxt] = 0;
		}
	}
}

void print(long long num)
{
	bool first = true;
	long long n = num;
	for(long long mod = (long long) 1e9; mod > 0; mod /= 1000)
	{
		if(n / mod == 0)
		{
		}
		else if(first)
		{
			printf("% 4lld", n / mod);
			first = false;
		}
		else
		{
			printf("'%03lld", n / mod);
		}
		n %= mod;
	}
	printf("\n");
}

void compute_rich_strings_of(int n)
{
    N = n;
	if(n == 2)
	{
		cnt[1] = 2;
	}
	else
	{
		init();
		dfs(2, 3);
	}
	#ifndef BENCH_RUN
	print(cnt[n-1] * 2);
	#endif
}
