import Matrices._
import Benchmark._

val tamanos = List(128, 256, 512, 1024, 2048) // Puedes probar hasta 4096 o más si tu máquina lo soporta

val resultados = for {
  n <- tamanos
  m1 = matrizAlAzar(n, 2)
  m2 = matrizAlAzar(n, 2)

  (tSec, tPar, accPar) = compararAlgoritmos(multMatriz, multMatrizPar)(m1, m2)
  (tRec, tRecPar, accRecPar) = compararAlgoritmos(multMatrizRec, multMatrizRecPar)(m1, m2)
} yield (
  n,
  tSec, tPar, accPar,
  tRec, tRecPar, accRecPar
)

// Imprimir resultados
println(f"Tamaño | multMatriz | multMatrizPar | Accel. | multMatrizRec | multMatrizRecPar | Accel.")
println("----------------------------------------------------------------------------------------------")
for ((n, t1, t2, acc1, t3, t4, acc2) <- resultados) {
  println(f"$n%6d | $t1%10.2f ms | $t2%13.2f ms | $acc1%6.2f | $t3%15.2f ms | $t4%18.2f ms | $acc2%6.2f")
}

val m1 = matrizAlAzar(16, 2)
val m2 = matrizAlAzar(16, 2)

multMatriz(m1, m2)
multMatrizPar(m1, m2)
multMatrizRec(m1, m2)
multMatrizRecPar(m1, m2)
multStrassen(m1, m2)
multStrassenPar(m1, m2)
