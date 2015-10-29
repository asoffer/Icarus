; ModuleID = 'global_module'

define i32 @main() {
entry:
  ret i32 0
}

define double @square(double %x) {
entry:
  %x1 = alloca double
  store double %x, double* %x1
  %x2 = load double, double* %x1
  %x3 = load double, double* %x1
  %multmp = fmul double %x2, %x3
  ret double %multmp
}

define double @wat(double %x) {
entry:
  %y = alloca double
  %x1 = alloca double
  store double %x, double* %x1
  store double 3.000000e+00, double* %y
  %y2 = load double, double* %y
  %x3 = load double, double* %x1
  %addtmp = fadd double %y2, %x3
  store double %addtmp, double* %y
  %y4 = load double, double* %y
  %divtmp = fdiv double %y4, 2.000000e+00
  store double %divtmp, double* %y
  %y5 = load double, double* %y
  %y6 = load double, double* %y
  %multmp = fmul double %y5, %y6
  ret double %multmp
}

define i32 @foo(i32 %n) {
entry:
  %n1 = alloca i32
  store i32 %n, i32* %n1
  %n2 = load i32, i32* %n1
  %n3 = load i32, i32* %n1
  %multmp = mul i32 %n2, %n3
  store i32 %multmp, i32* %n1
  %n4 = load i32, i32* %n1
  %n5 = load i32, i32* %n1
  %addtmp = add i32 %n4, %n5
  store i32 %addtmp, i32* %n1
  %n6 = load i32, i32* %n1
  %addtmp7 = add i32 %n6, 3
  ret i32 %addtmp7
}
