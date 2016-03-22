
#include <vector>

class NotCopyable {
public:
  NotCopyable() {}
private:
  NotCopyable(const NotCopyable&) = delete;
  NotCopyable& operator=(const NotCopyable&) = delete;
};

int main()
{
  std::vector<int> v = {1, 2, 4};

  {
  int& three = v[2];
  three = 3;
  }
  {
  int three = v[2];
  three = 3;
  }

  std::vector<int> v2;
  NotCopyable nc;

  return 0;
}
