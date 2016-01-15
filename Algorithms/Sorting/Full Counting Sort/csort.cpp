#include <string>
#include <vector>
#include <iostream>
using namespace std;


int main() {
    vector<string> assocs[100] = {vector<string>()};
    
    size_t n;
    cin >> n;
    size_t half = n/2;
    while (n--) {
        size_t t;
        cin >> t;
        
        string a;
        cin >> a;
        
        if (n >= half)
            assocs[t].push_back("-");
        else
            assocs[t].push_back(a);
    }
    
    for (auto i = 0; i < 100; ++i) {
        for (vector<string>::iterator it = assocs[i].begin(); it != assocs[i].end(); ++it)
            cout << *it << ' ';
    }
    cout << '\n';
    
    return 0;
}
