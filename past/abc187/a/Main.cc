#include <iostream>

int s(int x) {
	int sum = 0;
	while (x) {
		sum += x % 10;
		x /= 10;
	}
	return sum;
}

int main() {
	int a, b;
	std::cin >> a >> b;
	const int sa = s(a);
	const int sb = s(b);
	std::cout << std::max(sa, sb) << std::endl;
	return 0;
}
