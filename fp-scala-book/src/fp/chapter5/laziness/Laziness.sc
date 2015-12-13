import fp.chapter5.laziness.Laziness._

println("**************************************************")
val a = 20
if1(a < 22, () => println("a"), () => println("b"))
if2(a < 10, println("a"), println("b"))
if2(false, sys.error("fail"), 3)
println("**************************************************")
maybeTwiceStrict(true, { println("hi"); 1+41 })
println("**************************************************")
maybeTwiceLazy(true, { println("hi"); 1+41 })
println("**************************************************")