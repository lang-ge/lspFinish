//清华课程例子Hello!
#include <iostream>
using namespace std;
int main()
{
    cout << "Hello!" << endl;
    cout << "Weocome to c++!" << endl;
    return 0;
}
//经典hanoi函数
#include "stdio.h"
void move(char get, char put)
{
    printf("%c--->%c\n", get, put);
}
void hanoi(int n, char A, char B, char C)
{
    if (n == 1) //比较运算符,意为如果n=1刚为ture
        move(A, C);
    else
    {
        hanoi(n - 1, A, C, B);
        move(A, C);
        hanoi(n - 1, B, A, C);
    }
}
int main()
{
    int n;
    printf("input the number of diskes:");
    scanf("%d", &n);
    printf("the step to moveing %d diskes:\n", n);
    hanoi(n, 'A', 'B', 'C');
}
//C语言教学昆明理工大学微机程式
#include "stdio.h"
#define MASK 0x80 //1000 0000 16进制数8
int main()
{
    char s = 0x08;                           //0000 0000
    printf("s=%X,~s=%hX\n", s, ~s);          //1111 0111 0xf7
    printf("s=%X,s&0x80=%X\n", s, s & MASK); //0000 0000 0x00
    printf("s=%X,s|0x80=%X\n", s, s | MASK); //1000 1000 0x88
}

//两个数大小比较
// #include "stdio.h"
// int findMax(int a, int b) //定义findmax函数
// {
// int max;
// /* if (a > b)
// max = a;
// else
// max = b; */
// max = a > b ? a : b; //条件运算符效果同上
// return max; //通过return语句返回max
// }

// int main()
// {
// int x, y, t; //输入操作提示
// printf("please input two integers x,y:");
// scanf("%d,%d", &x, &y); //从键盘输入数据
// t = findMax(x, y); //调用findMax函数
// printf("The maximum is:%d\n", t);
// }

#include <iostream>
using namespace std;
int main()
{
    int x, y;
    cout << "please input two integers x,y:\n";
    cin >> x >> y;
    if (x > y)
        cout << "The maximum is:" << x;
    else
        cout << "The maximum is:" << y;
}
//枚举实例
#include <iostream>
using namespace std;
int main()
{
    int day;
    cin >> day;
    switch (day)
    {
    case 0:
        cout << "sunday" << endl;
        break;
    case 1:
        cout << "monday" << endl;
        break;
    case 2:
        cout << "turesday" << endl;
        break;
    case 3:
        cout << "wednesday" << endl;
        break;
    case 4:
        cout << "thursday" << endl;
        break;
    case 5:
        cout << "friday" << endl;
        break;
    case 6:
        cout << "saturday" << endl;
        break;
    default:
        cout << "day out of range sunday ..saturday" << endl;
        break;
    }
    return 0;
}
//嵌套条件
#include <iostream>
using namespace std;
int main()
{
    int x, y;
    cout << "enter x and y:";
    cin >> x >> y;
    if (x != y)
        if (x > y)
            cout << "x>y" << endl;
        else
            cout << "x<y" << endl;
    else
        cout << "x=y" << endl;
    return 0;
}
//条件运算符
#include <iostream>
using namespace std;
int main()
{
    int a, b, x;
    cout << "input value of a:\n"; //等同于printf
    cin >> a;                      //等同于scanf
    cout << "input value of b：\n";
    cin >> b;
    x = (a - b) > 0 ? (a - b) : (b - a);
    cout << "the difference of a and b is:\t" << x;
}
//同时显示面积与周长
#include "stdio.h"
#define PI 3.1415
int rmax() //定义findmax函数
{
    int r;
    printf("input the r:");
    scanf("%d", &r); //printf("r=%d\n", r);
    return r;        //通过return语句返回max
}
int main()
{
    double r, circum, area; //定义三个双精度变量值
    r = rmax();
    circum = 2 * PI * r; /* 计算圆的周长 */
    area = PI * r * r;   /* 计算圆的面积 */
    printf("circum=%f, area=%f\n", circum, area);
}
//无符号

#include <iostream>
using namespace std;
int main()
{
    int x;
    unsigned int y = 100;
    unsigned int z = 50;
    x = z - y;
    cout << " \n Now difference is: " << x << endl;
    return 0;
}
//输入一个数并将各个位数翻转！
#include <iostream>
using namespace std;
int main()
{
    int n, right_digit, newnum = 0;
    cout << "enter the number:";
    cin >> n;
    cout << "thee number in reverse order is: ";
    do
    {
        right_digit = n % 10; //取n除10的余,例:568/10=56.8中的8, 赋给变量a
        cout << right_digit;
        n /= 10;
    } while (n != 0); //当n不等于零的时候进入循环！
    cout << endl;
    return 0;
}
//运算符
#include <iostream>
using namespace std;
int main()
{
    int x, y;
    cout << "please input two integers x,y:\n";
    cin >> x >> y;
    //a += 3;                       //等价于a=a+3
    x *= y + 8;                   //等价于x=x*(y+8)
    cout << "x is:" << x << "\n"; //输出变量b的值，及说明信息
    return 0;
}
/* {
    int a;
    a = 3 * 5, a * 4;             //逗号运算与逗号表达式
    cout << "a is:" << a*4 << "\n"; //输出变量b的值，及说明信息
} */

//字节大小输出
#include "stdfix.h"
#include <iostream>
using namespace std;
int main()
{
    cout << "the size of an int is:\t\t" << sizeof(int) << "bytes. \n";
    cout << "the size of an long is:\t\t" << sizeof(long) << "bytes. \n";
    cout << "the size of an char is:\t\t" << sizeof(char) << "bytes. \n";
    cout << "the size of an float is:\t\t" << sizeof(float) << "bytes. \n";
    cout << "the size of an double is:\t\t" << sizeof(double) << "bytes. \n";
    cout << "the size of an double is:\t\t" << sizeof(short) << "bytes. \n";
    return 0;
}
//ASCII码值
#include "stdio.h"
int main()
{
    char ch;
    printf("请输入一个字符");
    scanf("%c", &ch);
    printf("\n字符%c的ASCII码值为%d\n", ch, ch);
}
/* 计算长方形的面积 */
#include "stdio.h"
int main()
{
    float a, b, area; //声明三个实型变量
    a = 2.5;
    b = 3.0;      //两条语句可以写在同一行
    area = a * b; //乘号用*号表示
    printf("该长方形的面积=%f\n", area);
}
//cout带字符串
#include <iostream>
using namespace std;
int main()
{
    int radius; //定义变量，int表示变量是整数类型
    cout << "please enter the radius!\n";
    cin >> radius;                              //从标准输入设备读入一个整数存入radius中
    cout << "the radius is:" << radius << "\n"; //输出变量radius的值，及说明信息
    cout << "please enter a different radius!\n";
    cin >> radius;                                             //输入一个不同的整数值，也存入radius中
    cout << "now the radius is changed to:" << radius << '\n'; //输出变量radius新的值
    return 0;
}
/* for用法之求一个数的比例因子
(n % k == 0)意思是计算n整除k的余数，与0做比较，
或者说判断n%i的余数为0时取值是真(非0值)，否则取值是假(0) */
#include <iostream>
using namespace std;
int main()
{
    int n;
    cout << "enter a positive integer:";
    cin >> n;
    cout << "Number " << n << "  factors ";
    for (int k = 1; k <= n; k++)
        if (n % k == 0)
            cout << k << " ";
    cout << endl;
    return 0;
}
//pause用法
#include "stdio.h"
#include "windows.h"
int main()
{
    printf("helo world!\n");
    system("pause");
}
//pi的用法
#include <iostream>
using namespace std;
int main()
{
    const double pi(3.14159); //定义符号常量
    int radius;               //定义变量，int表示变量是整数类型
    cout << "please enter the radius!\n";
    cin >> radius;                              //从标准输入设备读入一个整数存入radius中
    cout << "the radius is:" << radius << "\n"; //输出变量radius的值，及说明信息
    cout << "pi is:" << pi << '\n';
    cout << "please enter a different radius!\n";
    cin >> radius;                                             //输入一个不同的整数值，也存入radius中
    cout << "now the radius is changed to:" << radius << '\n'; //输出变量radius新的值
    // pi (3.1414)
    return 0;
}
//printf用法
#include "stdio.h"
#define PI 3.14159
int main()
{
    float r, area;
    printf("input the r:");
    scanf("%f", &r);           //s1
    area = PI * r * r;         //s3
    printf("面积=%f\n", area); //s3
    ;                          //s1空语句，设置断点
}
//result实例
#include "stdio.h"
int main()
{
    int x = 300, y = 200, result;
    result = x * y;
    printf("result=%d\n", result);
}
//while用法
#include <iostream>
using namespace std;
int main()
{
    int i = 0, sum = 0;
    for (i = 1; i <= 10; i++) //超强用法
    {
        sum += i;
    }
    cout << "sum= " << sum << endl;
    return 0;
}
/* {
    int i = 1, sum = 0;
    while (i <= 10)
    {
        sum += i; //相当于sum=sum+i;
        i++;
    }
    cout << "sum=" << sum << endl;
    return 0;
} */
/* int main()
{
    int i = 1, sum = 0;
    do
    {
        sum += i;
        i++;
    } while (i <= 10);
    cout << "sum=" << sum << endl;
    return 0;
} */

/* n-- 是先拿n本身的值用于表达式使用，再让n递减； 
--n 是先让n递减，再拿n递减后的值来给表达式使用； 
例子如下： int a = 10; int b = 10; 
cout << "a=" << a-- << " , b=" << --b << endl; 
这段代码的输出结果是： a=10 , b=9 */
//计算x的n次方
#include <iostream>
using namespace std;
double power(double x, int n)
{
    double val = 1.0;
    while (n--)
        val *= x; //等价于val = val * x
    return val;
}
int main()
{
    double pow;
    pow = power(5, 3);
    cout << "5 to the power 3 is " << pow << endl; 
    //函数调用作为一个表达式出现在输出语句中
    return 0;
}
//统计正整数个数和负整数个数
#include <iostream>
using namespace std;
int main()
{
    int i = 0, j = 0, n;
    cout << "Enter some integers please(enter 0 to quit):" << endl;
    cin >> n;
    while (n != 0)
    {
        if (n > 0)
            i += 1;
        if (n < 0)
            j += 1;
        cin >> n;
    }
    cout << "count of positive integers:" << i << endl;
    cout << "count of negative integers:" << j << endl;
    return 0;
}

//枚举类型例子
#include <iostream>
using namespace std;
enum gameresult { WIN,LOSE, TIE,CANCEL};
int main()
{
   gameresult result;                              //声明变量可以不写关键字
   gameresult omit = CANCEL;                  //可以在类型名前写enum
   for (int count = WIN; count <= CANCEL; count++) //隐含类型转换
   {
      result = gameresult(count); //强制转换
      if (result == omit)
         cout << "the game was cancelled" << endl;
      else
      {
         cout << "the game was played";
         if (result == WIN)
            cout << "and we win!";
         if (result == LOSE)
            cout << "and we lose";
         cout << endl;
      }
   }
   return 0;
}
//清华switch-case例子
#include <iostream>
using namespace std;
#define PI 3.1415
int main()
{
   int iType;
   float radius, a, b, area;
   cout << "图形的类型为？(1-圆形 2-长方形 3-正方形)：";
   cin >> iType;
   switch (iType)
   {
   case 1:
      cout << "圆的半径为：";
      cin >> a;
      area = PI * a * a;
      cout << "面积为：" << area << endl;
      break;
   case 2:
      cout << "长方形的长为：";
      cin >> a;
      cout << "长方形的宽为：";
      cin >> b;
      area = a * b;
      cout << "面积为：" << area << endl;
      break;
   case 3:
      cout << "正方形的边长为：";
      cin >> a;
      area = a * 4;
      cout << "面积为：" << area << endl;
      break;
      return 0;
   }
}

//8位二进制位转化为十进制输出
#include <iostream>
using namespace std;
double power(double x, int n);
int main()
{
    int value = 0;
    cout << "enter an 8 bit binary number ";
    for (int i = 7; i >= 0; i--)
    {
        char ch;
        cin >> ch;
        if (ch == '1')
            value += static_cast<int>(power(2, i));
    }
    cout << "decimal value is " << value << endl;
    return 0;
}
double power(double x, int n)
{
    double val = 1.0;
    while (n--)
        val *= x; //等价于val = val * x
    return val;
}

//计算π的值
#include <iostream>
using namespace std;
double arctan(double x)
{
    double sqr = x * x;
    double e = x;
    double r = 0;
    int i = 1;
    while (e / i > 1e-15)
    {
        double f = e / i;
        r = (i % 4 == 1) ? r + f : r - f;
        e = e * sqr;
        i += 2;
    }
    return r;
}

int main()
{
    double a = 16.0 * arctan(1 / 5.0);
    double b = 4.0 * arctan(1 / 239.0); //注意：因为整数相除结果取整，如果参数写成1/5，1/239结果就都是0
    cout << "PI = " << a - b << endl;
    return 0;
}

//计算11~999中的数满足m,m²,m³均为回文数
#include <iostream>
using namespace std;//判断n是否为回文数
bool symm(unsigned n)
{
    unsigned i = n;
    unsigned m = 0;
    while (i > 0)
    {
        m = m * 10 + i % 10;
        i /= 10;
    }
    return m == n;
}

int main()
{
    for (unsigned m = 11; m < 1000; m++)
        if (symm(m) && symm(m * m) && symm(m * m * m)) //逻辑与
        {
            cout << "m=" << m;
            cout << " m^2=" << m * m;
            cout << " m^3=" << m * m * m << endl;
        }
    return 0;
}

//阶乘实例
#include <iostream>
#include <cmath> //头文件cmath中具有C++对标准库数学中函数说明
using namespace std;
const double TINY_VALUE = 1E-10;
double tsin(double x)
{
    double g = 0;
    double t = x;
    int n = 1;
    do
    {
        g += t;
        n++;
        t = -t * x * x / (2 * n - 1) / (2 * n - 2);

    } while (fabs(t) >= TINY_VALUE);
    return g;
}
int main()
{
    double k, r, s;
    cout << "r= ";
    cin >> r;
    cout << "s= ";
    cin >> s;
    if (r * r <= s * s)
        k = sqrt(tsin(r) * tsin(r) + tsin(s) * tsin(s));
    else
        k = tsin(r * s) / 2;
    cout << k << endl;
    return 0;
}

//清华郑莉教授投骰子游戏
#include <iostream>
#include <cstdlib>
using namespace std;
//投骰子、计算和数、输出和数自定义函数
int rollDice()
{
    int die1 = 1 + rand() % 6;
    int die2 = 1 + rand() % 6;
    int sum = die1 + die2;
    cout << "player rolled " << die1 << "+" << die2 << "=" << sum << endl;
    return sum;
}
enum GameStatus{WIN, LOSE,PLAYING};
int main()
{
    int sum, mypoint;
    GameStatus status;
    unsigned seed;
    int rollDice();
    cout << "please enter an unsigned integer:";
    cin >> seed;      //输入随机种子
    srand(seed);      //将种子传递给rand()
    sum = rollDice(); //第一轮投骰子、计算和数
    switch (sum)
    {
    case 7: //如果和数为7或11则为胜，状态为WIN
    case 11:
        status = WIN;
        break;
    case 2: //如果和数为2、3或12为负，状态为LOSE
    case 3:
    case 12:
        status = LOSE;
        break;
    default: //其它情况，尚无结果，状态为PLAYING,记下点数,为下一轮做准备
        status = PLAYING;
        mypoint = sum;
        cout << "point is " << mypoint << endl;
        break;
    }
    while (status == PLAYING) { //只要状态为PLAYING,继续进行下一轮
        sum = rollDice();
        if (sum == mypoint) //某轮的和数等于点数则为胜
            status = WIN;
        else if (sum == 7) //出现和数为7则为负
            status = LOSE;
    }
    //当状态不为PLAYING时循环结束，以下程序输出游戏结果
    if (status == WIN)
        cout << "playing wins" << endl;
    else
        cout << "playing loses" << endl;
    return 0;
}
