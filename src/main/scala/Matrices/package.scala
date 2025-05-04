import common._
import scala.collection.parallel.immutable.ParVector
import scala.util.Random

package object Matrices {
  val random = new Random()
  type Matriz = Vector[Vector[Int]]

  def matrizAlAzar(long: Int, vals: Int): Matriz = {
    // Crea una matriz de enteros cuadrada de long x long,
    // con valores aleatorios entre 0 y vals
    val v = Vector.fill(long, long){ random.nextInt(vals) }
    v
  }

  def transpuesta(m: Matriz): Matriz = {
    val l = m.length
    Vector.tabulate(l, l)((i, j) => m(j)(i))
  }

  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map({ case (i, j) => (i * j) }).sum
  }

  def prodPuntoParD(v1: ParVector[Int], v2: ParVector[Int]): Int = {
    // A ser usada en el punto 1.5
    (v1 zip v2).map({ case (i, j) => (i * j) }).sum
  }

  // Ejercicio 1.1.1
  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    val m2T = transpuesta(m2)
    Vector.tabulate(n, n)((i, j) => prodPunto(m1(i), m2T(j)))
  }

  // Ejercicio 1.1.2
  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    val m2T = transpuesta(m2)
    val mitad = n / 2

    // Creamos dos tareas paralelas, cada una calcula media matriz
    val (parte1, parte2) = parallel(
      Vector.tabulate(mitad, n)((i, j) => prodPunto(m1(i), m2T(j))),
      Vector.tabulate(n - mitad, n)((i, j) => prodPunto(m1(i + mitad), m2T(j)))
    )

    // Concatenamos ambas mitades verticalmente
    parte1 ++ parte2
  }

  // Ejercicio 1.2.1
  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    // Dada m, matriz cuadrada de NxN, 1<=i,j<=N, i+n<=N, j+n<=N,
    // devuelve la submatriz de nxn correspondiente a m[i..i+(n−1), j..j+(n−1)
    Vector.tabulate(l,l)( (k,h) => m(k+i)(j+h))

  }

  // Ejercicio 1.2.2
  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    // recibe m1 y m2 matrices cuadradas del mismo tamaño, potencia de 2
    // y devuelve la suma de las 2 matrices
    val n = m1.length
    Vector.tabulate(n,n) ((i,j) => m1(i)(j) + m2(i)(j))

  }

  // Ejercicio 1.2.3
  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
    // recibe m1 y m2 matrices cuadradas del mismo tamaño, potencia de 2
    // y devuelve la multiplicación de las 2 matrices
    val n = m1.length
    val indiceMed = n/2
    val a11 = subMatriz(m1, 0, 0, indiceMed)
    val a12 = subMatriz(m1, 0, indiceMed, indiceMed)
    val a21 = subMatriz(m1, indiceMed, 0, indiceMed)
    val a22 = subMatriz(m1, indiceMed, indiceMed, indiceMed)

    val b11 = subMatriz(m2, 0, 0, indiceMed)
    val b12 = subMatriz(m2, 0, indiceMed, indiceMed)
    val b21 = subMatriz(m2, indiceMed, 0, indiceMed)
    val b22 = subMatriz(m2, indiceMed, indiceMed, indiceMed)

    val c11 = sumMatriz(multMatrizRec(a11, b11), multMatrizRec(a12, b21))
    val c12 = sumMatriz(multMatrizRec(a11, b12), multMatrizRec(a12, b22))
    val c21 = sumMatriz(multMatrizRec(a21, b11), multMatrizRec(a22, b21))
    val c22 = sumMatriz(multMatrizRec(a21, b12), multMatrizRec(a22, b22))


    (c11 zip c12).map { case (fila1, fila2) => fila1 ++ fila2 } ++
      (c21 zip c22).map { case (fila1, fila2) => fila1 ++ fila2 }
  }

  // Ejercicio 1.2.4
  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
    // recibe m1 y m2 matrices cuadradas del mismo tamaño, potencia de 2
    // y devuelve la multiplicación de las 2 matrices, en paralelo
    val n = m1.length
    val umbral = 32
      if(n<umbral){
        multMatrizRec(m1,m2)
      }else{
        val indiceMed = n/2
        val a11 = subMatriz(m1, 0, 0, indiceMed)
        val a12 = subMatriz(m1, 0, indiceMed, indiceMed)
        val a21 = subMatriz(m1, indiceMed, 0, indiceMed)
        val a22 = subMatriz(m1, indiceMed, indiceMed, indiceMed)

        val b11 = subMatriz(m2, 0, 0, indiceMed)
        val b12 = subMatriz(m2, 0, indiceMed, indiceMed)
        val b21 = subMatriz(m2, indiceMed, 0, indiceMed)
        val b22 = subMatriz(m2, indiceMed, indiceMed, indiceMed)

        // 1. Lanzamos 8 multiplicaciones recursivas como tareas paralelas
        val t1 = task { multMatrizRecPar(a11, b11) }
        val t2 = task { multMatrizRecPar(a12, b21) }
        val t3 = task { multMatrizRecPar(a11, b12) }
        val t4 = task { multMatrizRecPar(a12, b22) }
        val t5 = task { multMatrizRecPar(a21, b11) }
        val t6 = task { multMatrizRecPar(a22, b21) }
        val t7 = task { multMatrizRecPar(a21, b12) }
        val t8 = task { multMatrizRecPar(a22, b22) }

        // 2. Luego, paralelizamos las sumas usando parallel (estructurado)
        val (c11, c12, c21, c22) = parallel(
          sumMatriz(t1.join(), t2.join()),
          sumMatriz(t3.join(), t4.join()),
          sumMatriz(t5.join(), t6.join()),
          sumMatriz(t7.join(), t8.join())
        )

        /*
        val (c11,c12,c21,c22) = parallel(sumMatriz(multMatrizRecPar(a11, b11), multMatrizRecPar(a12, b21)),
        sumMatriz(multMatrizRecPar(a11, b12), multMatrizRecPar(a12, b22)),
        sumMatriz(multMatrizRecPar(a21, b11), multMatrizRecPar(a22, b21)),
        sumMatriz(multMatrizRecPar(a21, b12), multMatrizRecPar(a22, b22)))
*/

        (c11 zip c12).map { case (fila1, fila2) => fila1 ++ fila2 } ++
          (c21 zip c22).map { case (fila1, fila2) => fila1 ++ fila2 }

    }

  }

  // Ejercicio 1.3.1
  def restaMatriz(m1: Matriz, m2: Matriz): Matriz = {
    // recibe m1 y m2 matrices cuadradas del mismo tamaño, potencia de 2
    // y devuelve la resta de las 2 matrices
    val n = m1.length
    Vector.tabulate(n,n) ((i,j) => m1(i)(j) -m2(i)(j))
  }

  // Ejercicio 1.3.2
  def multStrassen(m1: Matriz, m2: Matriz): Matriz = {
    // recibe m1 y m2 matrices cuadradas del mismo tamaño, potencia de 2
    // y devuelve la multiplicación de las 2 matrices usando el algoritmo de Strassen

  }

  // Ejercicio 1.3.3
  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
    // recibe m1 y m2 matrices cuadradas del mismo tamaño, potencia de 2
    // y devuelve la multiplicación en paralelo de las 2 matrices usando el algoritmo de Strassen

  }
}
