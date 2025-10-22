# Learning Rust

## cargo
```rust
cargo new <project_name> # create a new project
cargo build # build the project, generate a binary file in target/debug/
cargo  run # build and run the binary file
cargo check # check the code without building
cargo build --release # build the project with optimizations
cargo update # update dependencies
cargo doc --open # generate documentation and open it in the browser
```

## variables

- 静态类型语言：编译时期需要知道所有变量的类型
- 整数类型
|长度|有符号类型|无符号类型|
  |---|---|---|
  |8位|i8|u8|
  |16位|i16|u16|
  |32位|i32|u32|
  |64位|i64|u64|
  |128位|i128|u128|
  |arch|isize|usize|
- 不同进制
  |进制|表示|
  |---|---|
  |二进制|0b|
  |八进制|0o|
  |十六进制|0x|
  |十进制|无前缀|
  |字节(仅限u8)|b'A'|
> 当在调试（debug）模式编译时，Rust 会检查整型溢出，若存在这些问题则使程序在编译时 panic。
> 在当使用 --release 参数进行发布（release）模式构建时，Rust 不检测会导致 panic 的整型溢出。相反当检测到整型溢出时，Rust 会进行一种被称为二进制补码包裹（two’s complement wrapping）的操作。简而言之，大于该类型最大值的数值会被“包裹”成该类型能够支持的对应数字的最小值
- 浮点类型
  |类型|描述|
  |---|---|
  |f32|单精度浮点数，精度为 24 位|
  |f64|双精度浮点数，精度为 53 位|
- 数值运算
```rust
+ 加法
- 减法
* 乘法
/ 除法
% 取余
```
- 布尔类型
  Rust 并不会尝试自动地将非布尔值转换为布尔值
- 字符类型
unicode标量值，大小为4个字节

```rust
let x = 10; // immutable variable   
let mut y = 20; // mutable variable
/* 遮蔽和覆盖不太相同，通常情况下类型不变使用mut,类型改变使用遮蔽*/
const PI: f64 = 3.14159; // constant variable
let tup:(i32, f64, u8) = (500, 6.4, 1); // tuple type
let (x, y, z) = tup; // tuple destructure模式匹配解构
let one = tup.0; // tuple index
let arr = [1, 2, 3, 4, 5]; // array type 必须同类型，tuple可以不同类型，通常数组也用于长度不可变得情况
let a: [i32:5] = [1, 2, 3, 4, 5]; // array type with fixed size
let a = [3;5] // array type with fixed size and value 3
let s = "Hello, world!"; // string type
```

## 函数
```rust
fn function( para1: type, para2: type, ...) -> return_type {
    // function body
}
// example
fn plus(x: i32, y: i32) -> i32 {
    x + y // or return x+y;
}
```
-  rust区分语句和表达式，表达式返回值，语句不返回值，表达式可以是语句得一部分，宏调用是一个表达式。我们用来创建新作用域的大括号（代码块） {} 也是一个表达式
if 是表达式的一种
```rust
fn main() {
    let y = {
        let x = 3;
        x + 1   // 表达式结尾没有分号
    }
    println!("The value of y is: {}", y);

    let z = if x < y {
        x + 1
    } else {
        y - 1
    }；

    let z = loop{
        counter += 1;
        if counter == 10 {
            break counter;
        }   
    }; 

    while number != 0 {
        println!("{}!", number);

        number -= 1;
    }

    let a = [10, 20, 30, 40, 50];

    for element in a {
        println!("the value is: {}", element);
    }

    for number in (1..4).rev() { // .rev()用于反转range
        println!("{}!", number);
    }

}
```

## 所有权
- 堆上的内存: 移动(move)，深拷贝(clone), 引用(referencing), 
> - 变量的所有权总是遵循相同的模式：将值赋给另一个变量时移动它。当持有堆中数据值的变量离开作用域时，其值将通过 drop 被清理掉，除非数据被移动为另一个变量所有。
> - 不过可变引用有一个很大的限制：在同一时间，只能有一个对某一特定数据的可变引用。非词法作用域生命周期（Non-Lexical Lifetimes，简称 NLL）
> - 在任意给定时间，要么 只能有一个可变引用，要么 只能有多个不可变引用。
> - 引用必须总是有效的
> Drop trait和Copy trait冲突
- 实现了Copy trait的类型
>所有整数类型，比如 u32。
>布尔类型，bool，它的值是 true 和 false。
>所有浮点数类型，比如 f64。
>字符类型，char。
>元组，当且仅当其包含的类型也都实现 Copy 的时候。比如，(i32, i32) 实现了 Copy，但 (i32, String) 就没有。
```rust
fn main() {
    let s1 = String::from("hello");
    let s2 = s1.clone();

    println!("s1 = {}, s2 = {}", s1, s2);
}
```
```rust
fn main() {
  let s = String::from("hello");  // s 进入作用域

  takes_ownership(s);             // s 的值移动到函数里 ...
                                  // ... 所以到这里不再有效

  let x = 5;                      // x 进入作用域

  makes_copy(x);                  // x 应该移动函数里，
                                  // 但 i32 是 Copy 的，所以在后面可继续使用 x

} // 这里, x 先移出了作用域，然后是 s。但因为 s 的值已被移走，
  // 所以不会有特殊操作

fn takes_ownership(some_string: String) { // some_string 进入作用域
  println!("{}", some_string);
} // 这里，some_string 移出作用域并调用 `drop` 方法。占用的内存被释放

fn makes_copy(some_integer: i32) { // some_integer 进入作用域
  println!("{}", some_integer);
} // 这里，some_integer 移出作用域。不会有特殊操作

```

## 结构体
> 普通结构体、元组结构体、单元结构体
> impl方法

## 枚举
```rust

#![allow(unused)]
fn main() {
enum IpAddr {
    V4(u8, u8, u8, u8),
    V6(String),
}

let home = IpAddr::V4(127, 0, 0, 1);

let loopback = IpAddr::V6(String::from("::1"));
}

```
```rust
fn main() {
    fn plus_one(x: Option<i32>) -> Option<i32> {
        match x {
            None => None,
            Some(i) => Some(i + 1),
            //_ => (),
            //other => 
        }
    }

    let five = Some(5);
    let six = plus_one(five);
    let none = plus_one(None);
}
```

## 常见集合
> vector、String、HashMap
> 注意String UTF-8编码问题
```rust
fn main(){
    for c in "नमस्ते".chars() {
        println!("{}", c);
    }
    for b in "नमस्ते".bytes() {
        println!("{}", b);
    }
    let s1 = String::from("tic");
    let s2 = String::from("tac");
    let s3 = String::from("toe");

    let s = format!("{}-{}-{}", s1, s2, s3);
    scores.entry(String::from("Yellow")).or_insert(50);

    let text = "hello world wonderful world";

    let mut map = HashMap::new();

    for word in text.split_whitespace() {
        let count = map.entry(word).or_insert(0);
        *count += 1;
    }
}
```
## 错误处理
```rust
[profile.release]
panic = 'abort'
```
>当出现 panic 时，程序默认会开始 展开（unwinding），这意味着 Rust 会回溯栈并清理它遇到的每
>一个函数的数据，不过这个回溯并清理的过程有很多工作。另一种选择是直接 终止（abort），这会不清
>理数据就退出程序。那么程序所使用的内存需要由操作系统来清理。如果你需要项目的最终二进制文件越
>小越好，panic 时通过在 Cargo.toml 的 [profile] 部分增加 panic = 'abort'，可以由展开切换为终止
>例如，如果你想要在release模式中 panic 时直接终止，如上所示

