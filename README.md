# Taller Paralelismo de Tareas y Datos

Proyecto de Scala que implementa diferentes algoritmos de multiplicación de matrices, tanto en versión secuencial como paralela, para comparar su rendimiento.

## Descripción

Este proyecto implementa y compara varios algoritmos de multiplicación de matrices:

- **Multiplicación básica**: Algoritmo tradicional de multiplicación de matrices
- **Multiplicación recursiva**: Implementación recursiva usando el método divide y vencerás
- **Algoritmo de Strassen**: Implementación del algoritmo de Strassen para multiplicación de matrices

Cada algoritmo tiene una versión secuencial y una versión paralela, permitiendo analizar las mejoras de rendimiento obtenidas mediante paralelización.

## Estructura del Proyecto

```
.
├── src/
│   ├── main/
│   │   └── scala/
│   │       ├── common/          # Utilidades para paralelización
│   │       ├── Matrices/        # Implementación de algoritmos de multiplicación
│   │       └── Benchmark/       # Utilidades para benchmarking
│   └── test/
│       └── scala/
│           └── Pruebas/         # Pruebas y comparaciones
├── build.sbt                    # Configuración del proyecto
└── README.md
```

## Algoritmos Implementados

### 1. Multiplicación Básica
- `multMatriz`: Versión secuencial
- `multMatrizPar`: Versión paralela que divide la matriz en dos mitades

### 2. Multiplicación Recursiva
- `multMatrizRec`: Versión secuencial recursiva
- `multMatrizRecPar`: Versión paralela recursiva con umbral de 32

### 3. Algoritmo de Strassen
- `multStrassen`: Versión secuencial del algoritmo de Strassen
- `multStrassenPar`: Versión paralela del algoritmo de Strassen con umbral de 16

### Funciones Auxiliares
- `transpuesta`: Calcula la transpuesta de una matriz
- `prodPunto`: Producto punto entre dos vectores (secuencial)
- `prodPuntoParD`: Producto punto entre dos vectores paralelos
- `subMatriz`: Extrae una submatriz de una matriz
- `sumMatriz`: Suma dos matrices
- `restaMatriz`: Resta dos matrices
- `matrizAlAzar`: Genera una matriz aleatoria
- `vectorAlAzar`: Genera un vector aleatorio

## Requisitos

- Scala 2.13.8
- sbt (Scala Build Tool)

## Dependencias

- `scalameter-core`: Para benchmarking y medición de rendimiento
- `scala-parallel-collections`: Para colecciones paralelas
- `munit`: Para pruebas unitarias

## Compilación y Ejecución

### Compilar el proyecto
```bash
sbt compile
```

### Ejecutar las pruebas
```bash
sbt test
```

### Ejecutar en modo interactivo (REPL)
```bash
sbt console
```

Una vez en el REPL, puedes importar los módulos:
```scala
import Matrices._
import Benchmark._
```

## Ejemplo de Uso

```scala
import Matrices._

// Crear dos matrices aleatorias de 4x4
val m1 = matrizAlAzar(4, 10)
val m2 = matrizAlAzar(4, 10)

// Multiplicar usando el algoritmo básico paralelo
val resultado = multMatrizPar(m1, m2)

// Comparar algoritmos
import Benchmark._
val (t1, t2, speedup) = compararAlgoritmos(multMatriz, multMatrizPar)(m1, m2)
println(s"Speedup: $speedup")
```

## Benchmarking

El proyecto incluye utilidades para comparar el rendimiento de diferentes algoritmos:

- `compararAlgoritmos`: Compara dos algoritmos de multiplicación y calcula el speedup
- `compararProdPunto`: Compara el producto punto secuencial vs paralelo

## Características de Paralelización

El proyecto utiliza:
- **ForkJoinPool**: Para la ejecución de tareas paralelas
- **DynamicVariable**: Para gestionar el scheduler de tareas
- **Colecciones paralelas**: Para paralelismo de datos en operaciones vectoriales

## Notas

- Los algoritmos recursivos (Strassen y multiplicación recursiva) requieren matrices de tamaño potencia de 2
- Los umbrales de paralelización están optimizados para balancear overhead vs beneficio
- El algoritmo de Strassen tiene complejidad O(n^log2(7)) ≈ O(n^2.81), mejor que el O(n^3) tradicional

## Autor

Proyecto desarrollado como parte del taller de "Paralelismo de Tareas y Datos".
