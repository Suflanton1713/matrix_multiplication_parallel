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

  // Ejercicio 1.2.3
  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
    // recibe m1 y m2 matrices cuadradas del mismo tamaño, potencia de 2
    // y devuelve la multiplicación de las 2 matrices
    val n = m1.length
    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
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

      val c11 = sumMatriz(multMatrizRec(a11, b11), multMatrizRec(a12, b21))
      val c12 = sumMatriz(multMatrizRec(a11, b12), multMatrizRec(a12, b22))
      val c21 = sumMatriz(multMatrizRec(a21, b11), multMatrizRec(a22, b21))
      val c22 = sumMatriz(multMatrizRec(a21, b12), multMatrizRec(a22, b22))


      (c11 zip c12).map { case (fila1, fila2) => fila1 ++ fila2 } ++
        (c21 zip c22).map { case (fila1, fila2) => fila1 ++ fila2 }
    }
  }

  // Ejercicio 1.2.4
  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    val umbral = 16  // puedes ajustar este valor según pruebas

    if (n <= umbral) {
      // Caso base: uso de versión secuencial cuando el tamaño es pequeño
      multMatrizRec(m1, m2)
    } else {
      val indiceMed = n / 2


      /*
      val ((a11, a12, a21, a22), (b11, b12, b21, b22)) = parallel(
        parallel(
          subMatriz(m1, 0, 0, indiceMed),
          subMatriz(m1, 0, indiceMed, indiceMed),
          subMatriz(m1, indiceMed, 0, indiceMed),
          subMatriz(m1, indiceMed, indiceMed, indiceMed)
        ),
        parallel(
          subMatriz(m2, 0, 0, indiceMed),
          subMatriz(m2, 0, indiceMed, indiceMed),
          subMatriz(m2, indiceMed, 0, indiceMed),
          subMatriz(m2, indiceMed, indiceMed, indiceMed)
        )
      )
      val a11 = subMatriz(m1, 0, 0, indiceMed)
      val a12 = subMatriz(m1, 0, indiceMed, indiceMed)
      val a21 = subMatriz(m1, indiceMed, 0, indiceMed)
      val a22 = subMatriz(m1, indiceMed, indiceMed, indiceMed)

      val b11 = subMatriz(m2, 0, 0, indiceMed)
      val b12 = subMatriz(m2, 0, indiceMed, indiceMed)
      val b21 = subMatriz(m2, indiceMed, 0, indiceMed)
      val b22 = subMatriz(m2, indiceMed, indiceMed, indiceMed)

       val (c11, c12, c21, c22) = parallel(
        sumMatriz(multMatrizRecPar(a11, b11), multMatrizRecPar(a12, b21)),
        sumMatriz(multMatrizRecPar(a11, b12), multMatrizRecPar(a12, b22)),
        sumMatriz(multMatrizRecPar(a21, b11), multMatrizRecPar(a22, b21)),
        sumMatriz(multMatrizRecPar(a21, b12), multMatrizRecPar(a22, b22))
      )


      (c11 zip c12).map { case (fila1, fila2) => fila1 ++ fila2 } ++
        (c21 zip c22).map { case (fila1, fila2) => fila1 ++ fila2 }
*/



      /*

      // Extraer submatrices en paralelo (8 en total)
    val ((a11, a12, a21, a22), (b11, b12, b21, b22)) = parallel(
      parallel(
        subMatriz(m1, 0, 0, indiceMed),
        subMatriz(m1, 0, indiceMed, indiceMed),
        subMatriz(m1, indiceMed, 0, indiceMed),
        subMatriz(m1, indiceMed, indiceMed, indiceMed)
      ),
      parallel(
        subMatriz(m2, 0, 0, indiceMed),
        subMatriz(m2, 0, indiceMed, indiceMed),
        subMatriz(m2, indiceMed, 0, indiceMed),
        subMatriz(m2, indiceMed, indiceMed, indiceMed)
      )
    )

    // Multiplicaciones recursivas (8 tareas)
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

    // Sumas de los productos parciales (4 tareas)
    val (c11, c12, c21, c22) = parallel(
      sumMatriz(m1b1, m1b2),
      sumMatriz(m2b1, m2b2),
      sumMatriz(m3b1, m3b2),
      sumMatriz(m4b1, m4b2)
    )

    // Unión final de cuadrantes
    val sup = (c11 zip c12).map { case (f1, f2) => f1 ++ f2 }
    val inf = (c21 zip c22).map { case (f1, f2) => f1 ++ f2 }
    sup ++ inf
       */
      // Submatrices
      val a11 = subMatriz(m1, 0, 0, indiceMed)
      val a12 = subMatriz(m1, 0, indiceMed, indiceMed)
      val a21 = subMatriz(m1, indiceMed, 0, indiceMed)
      val a22 = subMatriz(m1, indiceMed, indiceMed, indiceMed)

      val b11 = subMatriz(m2, 0, 0, indiceMed)
      val b12 = subMatriz(m2, 0, indiceMed, indiceMed)
      val b21 = subMatriz(m2, indiceMed, 0, indiceMed)
      val b22 = subMatriz(m2, indiceMed, indiceMed, indiceMed)

      // Tareas paralelas para multiplicaciones
      val t1 = task { multMatrizRecPar(a11, b11) }
      val t2 = task { multMatrizRecPar(a12, b21) }
      val t3 = task { multMatrizRecPar(a11, b12) }
      val t4 = task { multMatrizRecPar(a12, b22) }
      val t5 = task { multMatrizRecPar(a21, b11) }
      val t6 = task { multMatrizRecPar(a22, b21) }
      val t7 = task { multMatrizRecPar(a21, b12) }
      val t8 = task { multMatrizRecPar(a22, b22) }

      // Cálculo de las sumas en paralelo
      val (c11, c12, c21, c22) = parallel(
        sumMatriz(t1.join(), t2.join()),
        sumMatriz(t3.join(), t4.join()),
        sumMatriz(t5.join(), t6.join()),
        sumMatriz(t7.join(), t8.join())
      )

      // Combinar submatrices en la matriz final
      val superior = (c11 zip c12).map { case (f1, f2) => f1 ++ f2 }
      val inferior = (c21 zip c22).map { case (f1, f2) => f1 ++ f2 }
      superior ++ inferior

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
    val n = m1.length
    if (n == 1) {
      // Caso base: matrices de 1x1
      Vector(Vector(m1(0)(0) * m2(0)(0)))
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

      val s1 = restaMatriz(b12, b22)
      val s2 = sumMatriz(a11, a12)
      val s3 = sumMatriz(a21, a22)
      val s4 = restaMatriz(b21, b11)
      val s5 = sumMatriz(a11, a22)
      val s6 = sumMatriz(b11, b22)
      val s7 = restaMatriz(a12, a22)
      val s8 = sumMatriz(b21, b22)
      val s9 = restaMatriz(a11, a21)
      val s10 = sumMatriz(b11, b12)


      val p1 = multStrassen(a11, s1)
      val p2 = multStrassen(s2, b22)
      val p3 = multStrassen(s3, b11)
      val p4 = multStrassen(a22, s4)
      val p5 = multStrassen(s5, s6)
      val p6 = multStrassen(s7, s8)
      val p7 = multStrassen(s9, s10)

      val c11 = sumMatriz(restaMatriz(sumMatriz(p5, p4), p2), p6)
      val c12 = sumMatriz(p1, p2)
      val c21 = sumMatriz(p3, p4)
      val c22 = restaMatriz(restaMatriz(sumMatriz(p5, p1), p3), p7)

      val arriba = (c11 zip c12).map { case (fila1, fila2) => fila1 ++ fila2 }
      val abajo = (c21 zip c22).map { case (fila1, fila2) => fila1 ++ fila2 }

      arriba ++ abajo
    }
  }

  // Ejercicio 1.3.3
  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
    // recibe m1 y m2 matrices cuadradas del mismo tamaño, potencia de 2
    // y devuelve la multiplicación en paralelo de las 2 matrices usando el algoritmo de Strassen
    val n = m1.length
    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
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

      val s1 = restaMatriz(b12, b22)
      val s2 = sumMatriz(a11, a12)
      val s3 = sumMatriz(a21, a22)
      val s4 = restaMatriz(b21, b11)
      val s5 = sumMatriz(a11, a22)
      val s6 = sumMatriz(b11, b22)
      val s7 = restaMatriz(a12, a22)
      val s8 = sumMatriz(b21, b22)
      val s9 = restaMatriz(a11, a21)
      val s10 = sumMatriz(b11, b12)

      val p1 = task {
        multStrassenPar(a11, s1)
      }
      val p2 = task {
        multStrassenPar(s2, b22)
      }
      val p3 = task {
        multStrassenPar(s3, b11)
      }
      val p4 = task {
        multStrassenPar(a22, s4)
      }
      val p5 = task {
        multStrassenPar(s5, s6)
      }
      val p6 = task {
        multStrassenPar(s7, s8)
      }
      val p7 = task {
        multStrassenPar(s9, s10)
      }

      val (c11, c12, c21, c22) = parallel(
        sumMatriz(restaMatriz(sumMatriz(p5.join(), p4.join()), p2.join()), p6.join()),
        sumMatriz(p1.join(), p2.join()),
        sumMatriz(p3.join(), p4.join()),
        restaMatriz(restaMatriz(sumMatriz(p5.join(), p1.join()), p3.join()), p7.join())
      )

      val arriba = (c11 zip c12).map { case (fila1, fila2) => fila1 ++ fila2 }
      val abajo = (c21 zip c22).map { case (fila1, fila2) => fila1 ++ fila2 }

      arriba ++ abajo
    }
  }
}
