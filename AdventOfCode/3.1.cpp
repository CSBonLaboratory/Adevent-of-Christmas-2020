#include <iostream>
#include <fstream>
#include <vector>
using namespace std;
ifstream fin("input.txt");
int main()
{
    char v[323][31];

    for(int i=0;i<323;i++){
        fin.getline(v[i],32,'\n');

    }

    int i,j;
    long long ans = 1;

    int S;

    vector<pair<int,int>> slopes;
    slopes.push_back(make_pair(1,1));
    slopes.push_back(make_pair(3,1));
    slopes.push_back(make_pair(5,1));
    slopes.push_back(make_pair(7,1));
    slopes.push_back(make_pair(1,2));

    for(int k=0;k<slopes.size();k++)
    {
        i=0;
        j=0;
        S=0;
        while(i<323-slopes[k].second)
        {
            if(v[i+slopes[k].second][(j+slopes[k].first)%31]=='#')
                S++;
        
            i=i+slopes[k].second;
            j=(j+slopes[k].first)%31;
        }
        ans *= S;
    }

    cout<<ans;

    
    

}