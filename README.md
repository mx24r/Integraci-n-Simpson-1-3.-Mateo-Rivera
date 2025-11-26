# Integración Simpson 1/3


# 📘 **Análisis de la Función `integracion`**

---

## 🔢 **Código de la Función**

```scala
def integracion(f: Double => Double, a: Double, b: Double): Double = {
  val xBarra = (a + b) / 2.0
  (b - a) * (f(a) + 4 * f(xBarra) + f(b)) / 6.0
}
