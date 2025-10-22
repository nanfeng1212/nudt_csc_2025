# <center>NUDT-CSC2025-COMPILER</center>
## 一、项目说明
- 项目名称：NUDT-CSC2025-Compiler
- 项目简介：本项目是2025年编译系统实现赛代码，实现了一个面向Sysy的简单编译器，项目基于Rust实现，后端支持ARMV8架构
- 项目地址： https://gitee.com/li-renzhe/csc2025.git
- 项目主要贡献者：[侯华玮](https://github.com/houhuawei23)、李仁哲、张广星(本人)、薛东旺、欧阳照林
- 项目架构来源：[南开大学编译原理课程实验代码](https://github.com/JuniMay/nku-compiler-2024-rs.git)( [南开大学-梅骏逸](https://github.com/JuniMay)) **梅师傅的许可证 ↓ orz、Orz、or2、Or2**

> MIT License
> 
> Copyright (c) 2025 Junyi Mei
> 
> Permission is hereby granted, free of charge, to any person obtaining a copy
> of this software and associated documentation files (the "Software"), to deal
> in the Software without restriction, including without limitation the rights
> to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
> copies of the Software, and to permit persons to whom the Software is
> furnished to do so, subject to the following conditions:
> 
> The above copyright notice and this permission notice shall be included in all
> copies or substantial portions of the Software.
> 
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
> IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
> FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
> AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
> LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
> OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
> SOFTWARE.

## 二、项目运行说明

1. **项目依赖**：
    - **Rust环境**
        - *本项目：* Cargo(1.87.0-nightly)、rustc(1.87.0-nightly)
        - *比赛测评平台：* rustc(1.85.0) with -O2
    - **基本的python3环境(主要用于运行脚本)**
    - **交叉编译工具和运行平台**
     ```bash
      sudo apt install gcc-aarch64-linux-gnu
      sudo apt install clang
      sudo apt install qemu-user-static
     ```
<div align=center>

**表：主要参考**

<div style="display: inline-block; margin: 0 auto;">

|网站名称|网站链接|说明|
|----|----|----|
|Rust语言圣经(繁星点点尽在你指尖)|https://course.rs/about-book.html|易懂风趣|
|pku编译实践课在线文档|https://pku-minic.github.io/online-doc/#/preface/||
|nku编译系统原理课程文档|https://junimay.github.io/nku-compiler-2024-rs/index.html|
|Compiler Explorer|https://godbolt.org/|可以查看ASM、LLVM IR等|
|Lalrpop使用文档|https://lalrpop.github.io/lalrpop/index.html|

</div>
</div>

2. **项目运行**：

- **克隆项目到本地**：`git clone https://gitee.com/li-renzhe/csc2025.git`
- **进入`syscompiler`目录,可以通过cargo命令编译运行项目：**
    ```bash 
    cargo build --r  #release模式，生成的可执行文件在target/release目录下
    cargo build      #debug模式，生成的可执行文件在target/debug目录下
    ```
- **在对应目录下找到可执行文件`compiler`，`[]`内为可选项：**
    ```bash
    #编译源文件，输出汇编文件(比赛指定编译命令行参数) ↓
    compiler -S -o output_file source_file [-O1] 
    #编译源文件，输出AST、IR、汇编文件(非比赛指定编译命令行参数) ↓
    compiler [-ast ast_file_name] [-ir ir_file_name] [-asm asm_file_name] [-O1] source_file 
    ```
    **注意**：`-O1`参数表示优化等级1，`-S`参数表示输出汇编文件，`-ast`参数表示输出AST文件，`-ir`参数表示输出IR文件，`-asm`参数表示输出汇编文件，其中`-asm`比`-S -o`指定输出文件名有更高的优先级。
- **运行脚本，运行脚本前需要先创建好输出目录：**
    ```bash
    #以下命令是默认在sysycompiler下，考虑到脚本需要获取当前目录，脚本正常执行都需要在sysycompiler目录下运行
    # 运行脚本前需要先创建好输出目录
    mkdir ./testbench/output/functional
    mkdir ./testbench/output/performance
    mkdir ./testbench/output/final_performance
    mkdir ./testbench/output/hidden_functional
    # 运行批处理脚本 ↓
    # -S/-I表示是否输出汇编/IR文件
    # -f/-p/-fp/-hf表示测试用例对应目
    # -O1表示是否开启优化选项
    # 输出文件在output对应文件夹下
    python3 ./testbench/test.py {-S/-I} {-f/-p/-fp/-hf} [-O1] 
    # 运行单个样例的脚本 ↓
    # -f/-p/-fp/-hf表示测试用例对应目
    # source_file 对应目录下的文件名
    # -O1表示是否开启优化选项
    # -ir 表示是否只测试IR文件，默认测试汇编文件
    # 输出文件在output下，测试汇编文件会输出IR和ASM文件，测试IR文件只输出IR文件
    python3 ./testbench/run.py [-f/-p/-fp/-hf] source_file [-O1] [-ir]
        ```

## 三、项目目录结构

```bash
|——\offical_docs 官方文档汇总
    |——|lectures 培训资料汇总
|————\syscompiler
    |———\.vscode 调试配置
        |———\lanuch.json 调试配置CodeLLDB
    |———\src
        |———\backend
            |———\mir 目录下是MIR结构相关代码
            |———\reg_alloc 寄存器分配相关代码
                |———|regalloc.rs 朴素寄存器算法实现
                |———\gcregalloc 图着色寄存器分配算法实现
                    |———|live_intervals.rs 计算活跃区间
                    |———|intergerence_graph.rs 冲突图构建
                    |———|graphic_coloring_regalloc.rs 图着色寄存器分配算法实现
            |———|codegen.rs MIR代码生成相关代码
            |———|asm2string.rs MIR结构输出成汇编文本代码
        |———\frontend
            |———\lalrpop 前端生成语法树的工具所需的语法文件和
            |———\ir ir层的数据结构
            |———|generate_ir.rs 前端生成IR代码
            |———|ir2string.rs  IR结构输出成LLVM IR文本代码
            |———|symbol_table.rs  符号表结构
            |———|type_check.rs    类型检查，实际主要是插入类型转化表达式、常量折叠、处理数组的初始化
        |———\passes
            |———|pass_manager.rs  遍管理器
            |———|pass_context.rs  遍上下文，有些可以复用的分析遍信息在这里存储
            |———\pass
                |———|structure.rs     遍可能会用到的数据结构
                |———\analysis_pass    分析遍
                |———\optimize_pas     优化遍
        |———\utils 使用的数据链表容器和Areana等
        |———\main.rs 主函数
    |———\testcase                样例
        |———\functional
        |———\performance
        |———\finnal_performance
        |———\hidden_functional
    |———\testbench
        |———\output  输出文件
            |———\functional
            |———\performance
            |———\final_performance
            |———\hidden_functional
        |———|test.py  批测试脚本
        |———|run.py   单个文件debug脚本呢
        |———|clear.py cargo clean + clear output文件夹(便于提交)
    |———|Cargo.toml 项目依赖
    |———|build.rs 调用Lalrpop工具
|——|公开样例与运行时库
|——|RESOURCE.md 项目学习资料
|——|LICENSE 项目许可证MIT
|——|LEARNING.md 学习笔记
|——|README.md 项目说明文档
```

## 四、编译器设计介绍

### 1、Sysy → AST
> 在正式生成AST之前需要先完成对宏定义的替换，[宏定义的替换](./sysycompiler/src/frontend/lalrpop/macro_replace.rs)通过正则表达式实现，此外在实现过程中我们将自定义的库函数也在生成语法树之前插入了传入文件的结尾.编译器从Sysy到AST采用lalrpop生成工具完成，需要完成.lalrpop文件作为语法文件，[lalrpop语法](https://lalrpop.github.io/lalrpop/tutorial/index.html)和EBNF基本类似，实现可以参见nku和pku的编译课程在线文档。lalrpop是一个Rust的词法生成器框架，默认使用LR(1)分析器可以通过选项指定为LALR(1)分析器，lalrpop一方面通过所给的[.lalrpop](sysycompiler/src/frontend/lalrpop/sysy.lalrpop)文件解析源文件，另一方面通过用rust语言定义的[语法树文件](sysycompiler/src/frontend/lalrpop/ast.rs)创建存储在编译器中的语法树数据结构

<div align="center">
    <figure>
        <img src="./pictures/image.png" alt="00_mian.sy的语法树结构" width="20%">
        <figcaption>图：00_mian.sy的语法树结构</figcaption>
    </figure>
</div>

### 2、AST → IR

> 编译器从AST到IR通过遍历语法树自顶向下一遍扫描实现，编译器为AST中的数据结构实现对应的递归下降子程序，区别于一般的递归下降子程序，我们在实现过程中也通过参考往届参赛队伍(主要是： [三进制冒险家](https://gitlab.eduxiji.net/educg-group-26173-2487151/T202490002203537-615.git)和[人工式生成智能](https://gitlab.eduxiji.net/educg-group-26173-2487151/T202410055203436-1338.git))和加入自己理解，在实现上主要有以下几点不同：

> - 在正式进行中间代码发射之前，加入[类型检查](./sysycompiler/src/frontend/typecheck.rs)。这一步主要是插入相应的类型转化表达式、常量折叠、处理数组的初始化等，卸载发射IR过程中的负担
> - 在正式进行IR生成时，参考LLVM，会为每个函数创建唯一的入口块和出口块，同时所有的alloca指令都放置到入口块中，便于后续的控制流分析和Mem2reg优化等
> - 在对Sysy中的Block进行分析时，判断当前基本块是否已经被终止(结尾是terminator指令，如： ret、br等)，如果已经终止就不在对Block后续的Stmt进行中间代码生成，这样一方面减少后续优化遍的工作，也保证了基本块的完整性
> - 在处理数组的初始化时，我们使用自定义库memset来进行循环赋值，一方面可以减少代码长度另一方面也可以通过循环优化提高代码的运行效率，同时在我们的理解中，对于一个局部数据的初始化，如果需要赋值但是有没有给定全部位置的数值，那么其余位置都要初始化为0，所以我们将只要有初始化值不论多少都会调用memset函数，只有非零位置才会逐个初始化，这样对于小数组可能会让时间变长，但是小数组的初始化不会是性能瓶颈，一个大数组的初始化很可能不会指定全部的初值，甚至可能只指定少部分初值，这样memset函数的使用就有价值了。

### 3、Passes

> 1. 遍管理器：遍管理器采用包含泛型的结构体和特性，这样可以保证IR和MIR可以共用一个框架实现进行遍的管理和各个优化遍和分析遍的实现，增加代码的重用性
> 2. 遍上下文：遍上下文存储哪些由分析遍产生可能被多个优化遍所使用的结果，这样可以不用重复执行分析遍
> 3. 目前我们的遍主要包含

<div align="center">

**表：Analysis Pass**

<div style="display: inline-block; margin: 0 auto;">

|序号|名称|简称|功能|
|----|----|----|----|
|1|CFAnalysi|CFA|控制流分析|
|2|DominatorRelAnalysis|ltdomra/domra|支配关系分析|
|3|LoopAnalysis|loopana|循环分析|
|4|FunctionCallAnalysis|fca|函数调用分析|
|5|LiveInstAnalysis|live_inst|活跃变量分析|
|6|MirCFGAnalysis|mcfa|MIR控制流图分析|
|7|别名分析|TO|DO|
|9|依赖关系分析|TO|DO|

</div>
</div>

<div align="center">

**表： Optimization Pass**

<div style="display: inline-block; margin: 0 auto;">

|序号|名称|简称|功能|
|----|----|----|----|
|1|UnreachableBlockElimination|UBE|不可达基本块删除|
|2|CininuousInstructionElimination|CIE|连续跳转指令删除(?)|
|3|GloabalValueToLoacal|GTL|全局值转化为局部值(更精细？)|
|4|PreprocessMem2Reg|PreMem2Reg|主要是删除进行单次store指令的传播[👉](https://roife.github.io/posts/mem2reg-pass/)|
|5|Mem2Reg|Mem2Reg|插入phi节点实现ssa|
|6|GlobalDeadCodeElimination|DCE|全局死代码删除|
|7|SparseConditionalConstantPropagation|SCPP|稀疏条件常量传播|
|8|LinearBasicBlockCoalescing|LBC|线性基本块合并|
|9|Reg2Mem|Reg2Mem|消除phi节点|
|1|循环标准化|TO|DO|
|2|循环不变量外提|TO|DO|
|3|强度削弱|TO|DO|
|4|循环展开|TO|DO|
|5|尾递归消除/内联？|TO|DO|
|6|消除gep指令|TO|DO|
|7|公共子表达式删除|TO|DO|
|8|并行|TO|DO|

</div>
</div>

### 4、IR → MIR
> 后端实现时从IR到汇编中间加入MIR层，MIR层主要是为了支持后端的优化和代码生成，MIR使用虚拟寄存器，指令格式基本参考ARMV8
### 5、MIR → 后端代码
> MIR经过寄存器分配后生成ARMV8汇编，寄存器分配算法目前实现了两个版本，一个是顺序的朴素寄存器分配算法，一个是图着色寄存器分配算法，基于Chatin算法。

<style>
  /* 设置标题混合字体 */
  h1, h2, h3, h4, h5, h6 {
    font-family: "Times New Roman", "楷体", KaiTi, STKaiti, serif;
  }
  
  /* 确保代码块不受影响 */
  code, pre {
    font-family: Consolas, Monaco, "Courier New", monospace;
  }
  
  /* 可选：设置正文字体 */
  body {
    font-family: "Times New Roman", "宋体", SimSun, serif;
    line-height: 1.6;
  }
  
  /* 中文优化 */
  :lang(zh) {
    text-align: justify;
    text-justify: inter-ideograph;
  }
</style>



