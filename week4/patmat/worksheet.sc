import patmat.Huffman.*
import patmat.CodeTree
import patmat.Leaf
import patmat.Fork

val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
println(t1)

val r = encode(t1)("ab".toList)
println(r)

val s = decode(t1, r)
println(s)

