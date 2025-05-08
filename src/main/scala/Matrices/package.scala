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

  def vectorAlAzar(long: Int, vals: Int): Vector[Int] = {
    // Crea un vector de enteros de longitud 'long',
    // con valores aleatorios entre 0 y 'vals'
    val v = Vector.fill(long)(scala.util.Random.nextInt(vals))
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
    val mitad = n / 2

    // Creamos dos tareas paralelas, cada una calcula media matriz
    val (parte1, parte2) =(
      Vector.tabulate(mitad, n)((i, j) => prodPunto(m1(i), m2T(j))),
      Vector.tabulate(n - mitad, n)((i, j) => prodPunto(m1(i + mitad), m2T(j)))
    )

    // Concatenamos ambas mitades verticalmente
    parte1 ++ parte2
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

  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val mitad = n / 2

      val a11 = subMatriz(m1, 0, 0, mitad)
      val a12 = subMatriz(m1, 0, mitad, mitad)
      val a21 = subMatriz(m1, mitad, 0, mitad)
      val a22 = subMatriz(m1, mitad, mitad, mitad)

      val b11 = subMatriz(m2, 0, 0, mitad)
      val b12 = subMatriz(m2, 0, mitad, mitad)
      val b21 = subMatriz(m2, mitad, 0, mitad)
      val b22 = subMatriz(m2, mitad, mitad, mitad)

      val c11 = sumMatriz(multMatrizRec(a11, b11), multMatrizRec(a12, b21))
      val c12 = sumMatriz(multMatrizRec(a11, b12), multMatrizRec(a12, b22))
      val c21 = sumMatriz(multMatrizRec(a21, b11), multMatrizRec(a22, b21))
      val c22 = sumMatriz(multMatrizRec(a21, b12), multMatrizRec(a22, b22))

      Vector.tabulate(n, n) { (i, j) =>
        if (i < mitad && j < mitad) c11(i)(j)
        else if (i < mitad) c12(i)(j - mitad)
        else if (j < mitad) c21(i - mitad)(j)
        else c22(i - mitad)(j - mitad)
      }
    }
  }


  // Ejercicio 1.2.4
  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    val umbral = 32 // 2^5
    if (n < umbral) {
      multMatrizRec(m1,m2)
    } else {

      val indiceMed = n / 2

      val a11 = subMatriz(m1, 0, 0, indiceMed)
      val a12 = subMatriz(m1, 0, indiceMed, indiceMed)
      val a21 = subMatriz(m1, indiceMed, 0, indiceMed)
      val a22 = subMatriz(m1, indiceMed, indiceMed, indiceMed)

      val b11 = subMatriz(m2, 0, 0, indiceMed)
      val b12 = subMatriz(m2, 0, indiceMed, indiceMed)
      val b21 = subMatriz(m2, indiceMed, 0, indiceMed)
      val b22 = subMatriz(m2, indiceMed, indiceMed, indiceMed)

      val ((m1b1, m1b2, m2b1, m2b2), (m3b1, m3b2, m4b1, m4b2)) = parallel(
        parallel(
          multMatrizRecPar(a11, b11), // para c11
          multMatrizRecPar(a12, b21), // para c11
          multMatrizRecPar(a11, b12), // para c12
          multMatrizRecPar(a12, b22)  // para c12
        ),
        parallel(
          multMatrizRecPar(a21, b11), // para c21
          multMatrizRecPar(a22, b21), // para c21
          multMatrizRecPar(a21, b12), // para c22
          multMatrizRecPar(a22, b22)  // para c22
        )
      )
      val (c11, c12, c21, c22) = parallel(
        sumMatriz(m1b1, m1b2),
        sumMatriz(m2b1, m2b2),
        sumMatriz(m3b1, m3b2),
        sumMatriz(m4b1, m4b2)
      )

      val resultado = Vector.tabulate(n, n) { (i, j) =>
        if (i < indiceMed && j < indiceMed) c11(i)(j)
        else if (i < indiceMed && j >= indiceMed) c12(i)(j - indiceMed)
        else if (i >= indiceMed && j < indiceMed) c21(i - indiceMed)(j)
        else c22(i - indiceMed)(j - indiceMed)
      }
      resultado
    }
  }


  // Ejercicio 1.3.1
  def restaMatriz(m1: Matriz, m2: Matriz): Matriz = {
    // recibe m1 y m2 matrices cuadradas del mismo tamaño, potencia de 2
    // y devuelve la resta de las 2 matrices
    val n = m1.length
    Vector.tabulate(n,n) ((i,j) => m1(i)(j) -m2(i)(j))
  }

  def multStrassen(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val mitad = n / 2

      val a11 = subMatriz(m1, 0, 0, mitad)
      val a12 = subMatriz(m1, 0, mitad, mitad)
      val a21 = subMatriz(m1, mitad, 0, mitad)
      val a22 = subMatriz(m1, mitad, mitad, mitad)

      val b11 = subMatriz(m2, 0, 0, mitad)
      val b12 = subMatriz(m2, 0, mitad, mitad)
      val b21 = subMatriz(m2, mitad, 0, mitad)
      val b22 = subMatriz(m2, mitad, mitad, mitad)

      val p1 = multStrassen(sumMatriz(a11, a22), sumMatriz(b11, b22))
      val p2 = multStrassen(sumMatriz(a21, a22), b11)
      val p3 = multStrassen(a11, restaMatriz(b12, b22))
      val p4 = multStrassen(a22, restaMatriz(b21, b11))
      val p5 = multStrassen(sumMatriz(a11, a12), b22)
      val p6 = multStrassen(restaMatriz(a21, a11), sumMatriz(b11, b12))
      val p7 = multStrassen(restaMatriz(a12, a22), sumMatriz(b21, b22))

      val c11 = sumMatriz(restaMatriz(sumMatriz(p1, p4), p5), p7)
      val c12 = sumMatriz(p3, p5)
      val c21 = sumMatriz(p2, p4)
      val c22 = sumMatriz(restaMatriz(sumMatriz(p1, p3), p2), p6)

      Vector.tabulate(n, n) { (i, j) =>
        if (i < mitad && j < mitad) c11(i)(j)
        else if (i < mitad) c12(i)(j - mitad)
        else if (j < mitad) c21(i - mitad)(j)
        else c22(i - mitad)(j - mitad)
      }
    }
  }
  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    val umbral = 16 // 2^4
    if (n < umbral) {
      multMatrizRec(m1,m2)
    } else {
      val mitad = n / 2

      val a11 = subMatriz(m1, 0, 0, mitad)
      val a12 = subMatriz(m1, 0, mitad, mitad)
      val a21 = subMatriz(m1, mitad, 0, mitad)
      val a22 = subMatriz(m1, mitad, mitad, mitad)

      val b11 = subMatriz(m2, 0, 0, mitad)
      val b12 = subMatriz(m2, 0, mitad, mitad)
      val b21 = subMatriz(m2, mitad, 0, mitad)
      val b22 = subMatriz(m2, mitad, mitad, mitad)

      val t1 = task(multStrassenPar(sumMatriz(a11, a22), sumMatriz(b11, b22))) // p1
      val t2 = task(multStrassenPar(sumMatriz(a21, a22), b11))                 // p2
      val t3 = task(multStrassenPar(a11, restaMatriz(b12, b22)))               // p3
      val t4 = task(multStrassenPar(a22, restaMatriz(b21, b11)))               // p4
      val t5 = task(multStrassenPar(sumMatriz(a11, a12), b22))                 // p5
      val t6 = task(multStrassenPar(restaMatriz(a21, a11), sumMatriz(b11, b12))) // p6
      val t7 = task(multStrassenPar(restaMatriz(a12, a22), sumMatriz(b21, b22))) // p7

      val p1 = t1.join()
      val p2 = t2.join()
      val p3 = t3.join()
      val p4 = t4.join()
      val p5 = t5.join()
      val p6 = t6.join()
      val p7 = t7.join()


      val (c11, c12, c21, c22) = parallel(
        sumMatriz(restaMatriz(sumMatriz(p1, p4), p5), p7),
        sumMatriz(p3, p5),
        sumMatriz(p2, p4),
        sumMatriz(restaMatriz(sumMatriz(p1, p3), p2), p6)
      )

      Vector.tabulate(n, n) { (i, j) =>
        if (i < mitad && j < mitad) c11(i)(j)
        else if (i < mitad) c12(i)(j - mitad)
        else if (j < mitad) c21(i - mitad)(j)
        else c22(i - mitad)(j - mitad)
      }
    }
  }

}
