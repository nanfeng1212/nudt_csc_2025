pub static MEMSET_SYSY: &str = "
void nudt_memset_int( int arr[], int val, int size ){
    int i = 0;
    while( i < size ){
        arr[i] = val;
        i = i + 1;
    }
}

void nudt_memset_float( float arr[], float val, int size ){
    int i = 0;
    while( i < size ){
        arr[i] = val;
        i = i + 1;
    }
}
";

pub static MEMSET_IR: &str = "
define void @nudt_memset_int(i32* %0, i32 %1, i32 %2){
  %4 = icmp sgt i32 %2, 0
  br i1 %4, label %5, label %10

5:
  %6 = phi i32 [ %8, %5 ], [ 0, %3 ]
  %7 = getelementptr i32, i32* %0, i32 %6
  store i32 %1, i32* %7
  %8 = add i32 %6, 1
  %9 = icmp eq i32 %8, %2
  br i1 %9, label %10, label %5

10:
  ret void
}

define void @nudt_memset_float( float* %0, float %1, i32 %2){
  %4 = icmp sgt i32 %2, 0
  br i1 %4, label %5, label %10

5:
  %6 = phi i32 [ %8, %5 ], [ 0, %3 ]
  %7 = getelementptr float, float* %0, i32 %6
  store float %1, float* %7, align 4
  %8 = add i32 %6, 1
  %9 = icmp eq i32 %8, %2
  br i1 %9, label %10, label %5

10:
  ret void
}
";
