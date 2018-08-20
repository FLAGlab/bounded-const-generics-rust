Mucha de la sintaxis introducida acá no está completamente integrada dentro de GHC.

# Eliminating erroneous programs

## Vectors as indexed types

Un vector en _Haskell_ puede definirse como un tipo que depende del valor de su longitud.

```haskell
data Nat = Zero | Succ Nat
data Vec :: Type -> Nat -> Type where
    Nil  :: Vec a 'Zero
    (:>) :: a -> Vecc a n -> Vec a ('Succ n)
infixr 5:>
```

Ahora utilizaremos `Type` en vez de `*`.

### Append

Si tenemos una función que concatena dos vectores de tipo `Vec a n` y `Vec a m` su resultado debería ser de tipo `Vec a (n + m)`. Para esto debemos definir primero la suma de naturales:

```haskell
(+) :: Nat -> Nat -> Nat
Zero + m = m
Succ n + m = Succ (n + m)
```

Ahora `append`

```haskell
append :: Vec a n -> Vec a m -> Vec a(n '+ m)
append Nil w = w
append (a :> v) w = a :> (append v w)
```

escribimos `'+` en vez de `+` porque estamos haciendo referencia al _namespace_ de los términos y no de los tipos.

### Replicate

`replicate` toma un valor y crea un vector con `n` copias de este valor. Este es el primer ejemplo que usa `pi-types`:

```haskell
replicate :: forall a. pi (n :: Nat) -> a -> Vec a n
replicate Zero _ = Nil
replicate (Succ n) x = x :> replicate n x
```

Esto se debe a que el tipo resultante, un vector de tamaño `n`, está indexado por el valor del primer argumento de `replicate`. Hay una segunda forma de escribir `replicate`:

```haskell
replicate :: pi (n :: Nat). forall a. a -> Vec a n
replicate @Zero _ = Nil
replicate @(Succ _) x :> replicate x
```

Aquí, el primer argumento `n` es invisible, lo cual nos permite escribir funciones como

```haskell
fourTrues :: Vec Bool 4
fourTrues = replicate True
```

de tal manera que Haskell es capaz de inferir el argumento faltante de `replicate`. Sin embargo, debe usarse `@` para referirse al argumento invisible dentro de la implementación de `replicate`. Esto me recuerda a los valores implícitos de Scala.

### Length

Calcular la longitud de un vector es menos obvio de lo que parece. Dado que Haskell borra los tipos y estos no existen en ejecución, no basta con tomar `Vec a n` y retornar `n`.

Para resolver esto se introduce el concepto de relevancia

```haskell
length :: pi n. forall a. Vec a n -> Nat
length @n _ = n
```

Aquí, de nuevo `n` es un argumento invisible del tipo de `length`, sin embargo está disponible en ejecución. Mientras que el segundo argumento solo es necesario para inferir el tipo de `n`.

En realidad, `a` también es un argumento invisible de `length`, si cambiásemos el orden de `a` y `n` en la signatura, tendríamos que escribir el siguiente código

```haskell
length :: forall a. pi n. Vec a n -> Nat
length @_ @n _ = n
```

Por otro lado podemos usar la estructura de `Vec` para calcular la longitud sin necesitar a `n` en tiempo de ejecución:

```haskell
length forall n a. Vec a n -> Nat
length Nil = 0
length (_ :> v) = 1 + length v
```
