#include <iostream>
#include <fstream>
#include <vector>
using namespace std;
ifstream fin("input.txt");


int main()
{
    vector<int> v(200);

    for(int i=0;i<200;i++)
        fin>>v[i];
    
    for(int i=0;i<198;i++)
        for(int j=i+1;j<199;j++)
            for(int k=j+1;k<200;k++)
                if(v[i]+v[j]+v[k]==2020)
                {
                    cout<<v[i]*v[j]*v[k];
                    break;
                }

    
}