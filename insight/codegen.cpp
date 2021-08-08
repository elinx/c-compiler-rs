#include <bits/stdc++.h>

using namespace std;

char register_operand_map[100];                      // 模拟寄存器的个数
char instructions[100][100];                         // 输入表达式字符串
int instrs_num, registers_num, register_counter = 0; //n输入的表达式，m是寄存器个数

const static int first_operand_index = 3;
const static int op_index = 4;
const static int second_operand_index = 5;

// 寄存器中是否存在该变量
int get(char ch)
{
    for (int i = 0; i < registers_num; i++)
    {
        if (ch == register_operand_map[i])
            return i;
    }
    return -1;
}
///寄存器的使用顺序为:
///寄存器中存有 > 空寄存器 > 内存中存有 > 以后不再使用 > 最远距离使用
//判断后面是否还会使用
//x是输入表达式的编号，语句执行顺序
//ch需要判断的字符
int use(int curr_instr_idx, char operand)
{
    for (int i = curr_instr_idx; i < instrs_num; i++)
    {
        // A:=B+C这里就是B、C
        // 如果有就返回第几句用到
        if (operand == instructions[i][first_operand_index] ||
            operand == instructions[i][second_operand_index])
            return i;
    }
    return instrs_num;
}

///寄存器的使用顺序为:
///寄存器中存有 > 空寄存器 > 内存中存有 > 以后不再使用 > 最远距离使用
int find_free_register(int curr_instr_idx)
{
    // 如果top小于m，就说明就空的寄存器可以使用
    if (register_counter < registers_num)
        return register_counter++;
    //否则就是
    int t = -1;
    int ans = -1;
    for (int reg_idx = 0; reg_idx < registers_num; reg_idx++)
    {
        // 逐个判断在第x句之后，存在寄存器中的字符的使用顺序
        // 返回值k是执行顺序的标号
        int nearest_instr_idx = use(curr_instr_idx, register_operand_map[reg_idx]);
        // 我们的目的是找到最远不适用的，如果返回值越大，那就是最远不适用的
        // 记录标号存在t里面
        if (nearest_instr_idx > ans)
        {
            ans = nearest_instr_idx;
            t = reg_idx;
        }
    }
    return t;
}
// 减加乘除
void print1(char ch)
{
    if (ch == '+')
        printf("ADD ");
    else if (ch == '-')
        printf("SUB ");
    else if (ch == '*')
        printf("MUL ");
    else if (ch == '\\')
        printf("DIV ");
}
// 第二个操作数的使用
void print2(char ch)
{
    // 查看是否在寄存器中
    int x = get(ch);
    // 如果在寄存器中，那就直接使用
    if (x != -1)
    {
        printf("R%d\n", x);
    }
    else
    {
        // 否则就直接使用内存中的即可，因为已经有一个寄存器了
        printf("%c\n", ch);
    }
}
int main()
{
    cin >> instrs_num >> registers_num;
    for (int i = 0; i < instrs_num; i++)
    {
        cin >> instructions[i];
    }
    for (int i = 0; i < instrs_num; i++)
    {
        // 将第1个操作数取出来，get()来获取寄存器中是否存在该变量
        // T:=A-B中的A
        int reg_idx = get(instructions[i][first_operand_index]); //如果不为-1，得到的x为所在寄存器的标号
        // 如果x为-1说明该数不在寄存器中，需要做处理
        if (reg_idx == -1)
        {
            //此时的reg_idx是我们可以使用的寄存器标号
            reg_idx = find_free_register(i);
            // 如果register_operand_map[reg_idx]中有东西，并且use(i,register_operand_map[reg_idx]) < instrs_num即后面会使用到
            //那我们就把原来在寄存器中的字符ST到内存中
            if (register_operand_map[reg_idx] != '\0' && use(i, register_operand_map[reg_idx]) < instrs_num)
            {
                printf("ST R%d, %c\n", reg_idx, register_operand_map[reg_idx]);
                register_operand_map[reg_idx] = 0;
            }
            //将该操作数加载到我们使用的寄存器标号中
            printf("LD R%d, %c\n", reg_idx, instructions[i][first_operand_index]);
        }
        // 这里下面的操作就是执行运算
        // 1、输出操作符，这里是加减乘除
        print1(instructions[i][op_index]);
        //2、输出寄存器标号，需要的操作数已经加载到里面了
        printf("R%d, ", reg_idx);
        //3、第二个操作数的使用
        print2(instructions[i][second_operand_index]);
        // 寄存器中当前存的数值就是该表示的左边的字符,即T:=A-B中的T
        register_operand_map[reg_idx] = instructions[i][0];
    }
    return 0;
}
