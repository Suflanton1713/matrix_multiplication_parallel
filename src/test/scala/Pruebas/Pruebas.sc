import Matrices._
import Benchmark._
import scala.collection.compat.immutable.ArraySeq

// --- Definición de las funciones de multiplicación de matrices en un arreglo ---
val algoritmosMult: ArraySeq[(Matriz, Matriz) => Matriz] = ArraySeq(
  multMatriz,       // Algoritmo de multiplicación básica
  multMatrizPar,    // Algoritmo de multiplicación paralela
  multMatrizRec,    // Algoritmo de multiplicación recursiva
  multMatrizRecPar, // Algoritmo de multiplicación recursiva paralela
  multStrassen,     // Algoritmo de multiplicación Strassen
  multStrassenPar   // Algoritmo de multiplicación Strassen paralela
)

// --- Preparación de las matrices para las pruebas ---
val k = 2
val dim = math.pow(2, k).toInt
val m1 = matrizAlAzar(dim, 2)
val m2 = matrizAlAzar(dim, 2)

// --- Comparación de algoritmos de multiplicación de matrices (secuencial vs paralelo) ---
val comparacionAlgoritmos = algoritmosMult.map { algoritmo =>
  val (tSeq, tPar, acc) = compararAlgoritmos(algoritmo, algoritmo)(m1, m2)
  (tSeq, tPar, acc) // Devolvemos tiempo secuencial, paralelo y aceleración
}

// --- Comparación de los algoritmos productoPunto y productoPuntoPar ---
val comparacionProductoPunto = (1 to 3).map { i =>
  val (tProdPunto, tProdPuntoPar, acc) = compararProdPunto(math.pow(10, i).toInt)
  (tProdPunto, tProdPuntoPar, acc)
}

// --- Casos de prueba para verificar que los algoritmos multiplican matrices correctamente ---
val n = 2
val m3 = matrizAlAzar(math.pow(2, n).toInt, 2)
val m4 = matrizAlAzar(math.pow(2, n).toInt, 2)

// --- Resultados de la multiplicación de matrices de cada algoritmo ---
val resultadosMultiplicacion = algoritmosMult.map { algoritmo =>
  algoritmo(m3, m4)
}

// --- Verificación de si todos los resultados de la multiplicación son iguales ---
val sonIguales = resultadosMultiplicacion.map(_.toList).distinct.size == 1


