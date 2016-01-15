#include <iostream>
using namespace std;

int main() {
    int count_a[10001] = {0};
    size_t n;
    cin >> n;
    
    int tmp;
    for (size_t i = 0; i < n; ++i) {
        cin >> tmp;
        ++count_a[tmp];
    }
    
    int count_b[10001] = {0};
    size_t k;
    cin >> k;
    for (size_t i = 0; i < k; ++i) {
        cin >> tmp;
        ++count_b[tmp];
    }
    
    for (int i = 0; i < 10001; ++i) {
        if (count_a[i] != count_b[i])
            cout << i << ' ';
    }
    
    return 0;
}
