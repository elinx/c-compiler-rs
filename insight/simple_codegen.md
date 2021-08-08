# 简单的代码生成程序
## Description

通过三地址代码序列生成计算机的目标代码,在生成算法中,对寄存器的使用顺序为:寄存器中存有 > 空寄存器 > 内存中存有 > 以后不再使用 > 最远距离使用

## Input
单组输入,给定输出的三地址代码的个数和寄存器的个数.所有的变量为大写字母,寄存器的数量不超过9

## Output
参照示例格式输出,不需要将最后的寄存器中的值写回内存

不再使用变量不用写回内存

## Sample
### Input

```shell
4 2
T:=A-B
U:=A-C
V:=T+U
W:=V+U
```

### Output

```shell
LD R0, A
SUB R0, B
LD R1, A
SUB R1, C
ADD R0, R1
ADD R0, R1
```