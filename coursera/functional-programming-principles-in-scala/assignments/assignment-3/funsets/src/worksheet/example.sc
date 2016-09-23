object example {
import funsets.FunSets._
println(contains(singletonSet(1), 1))             //> true
println(contains(union(singletonSet(1), singletonSet(2)), 2))
                                                  //> true

def numsBetween10And20 = (e :Int) => e >= 10 && e <=20
                                                  //> numsBetween10And20: => Int => Boolean
def numsBetween15And20 = (e :Int) => e >= 15 && e <=20
                                                  //> numsBetween15And20: => Int => Boolean
def inter = intersect(numsBetween10And20, numsBetween15And20)
                                                  //> inter: => funsets.FunSets.Set
println(inter(12))                                //> false
println(inter(15))                                //> true

def diffNums = diff(numsBetween10And20, numsBetween15And20)
                                                  //> diffNums: => funsets.FunSets.Set
println(diffNums(10))                             //> true
println(diffNums(15))                             //> false

def setOfNums = union(union(union(union(union(singletonSet(1), singletonSet(3)), singletonSet(4)), singletonSet(5)), singletonSet(7)), singletonSet(1000))
                                                  //> setOfNums: => funsets.FunSets.Set

def filtered = filter(setOfNums, e => e < 5)      //> filtered: => funsets.FunSets.Set
contains(filtered, 1)                             //> res0: Boolean = true
contains(filtered, 2)                             //> res1: Boolean = false
contains(filtered, 3)                             //> res2: Boolean = true
contains(filtered, 4)                             //> res3: Boolean = true
contains(filtered, 5)                             //> res4: Boolean = false
contains(filtered, 1000)                          //> res5: Boolean = false



}