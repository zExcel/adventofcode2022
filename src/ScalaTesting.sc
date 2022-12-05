import scala.collection.mutable

val stack = mutable.Stack(1, 2, 3)
val stack2 = mutable.Stack(4,5,6)
stack.pushAll(stack2.reverse)
stack.pop()
stack