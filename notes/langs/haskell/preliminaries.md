# Type Classes

Son un tipo de polimorfismo similar a las interfaces o a los _traits_ (cuál es la diferencia?).

```haskell
class Show a where
    show :: a -> String

instance Show Bool where
    show True = "tru"
    show False = "fls"
```

Básicamente, una clase es paramétrica sobre un tipo. La diferencia de usar clases con funciones polimórficas paramétricas radica en que al crear una instancia de la clase usando un tipo específico, los métodos de la clase pueden saber éste tipo.

Para usar una clase como restricción sobre un tipo, se utiliza la siguiente sintaxis:

```haskell
Show a => [a] -> String
```

# Families

## Type Families

Son funciones de tipos a tipos

```haskell
type family Foo a where
    Foo Int = Bool
    Foo Char = Double
```

Con esto se puede hacer referencia a la familia en vez de los tipos específicamente

```haskell
useFoo :: Foo Int -> Foo Char
useFoo True = 1.0
useFoo False = (-1.0)
```

En este caso, `Foo` es una familia _cerrada_ porque todas las ecuaciones que la definen están en un solo lugar. El siguiente ejemplo es una familia _abierta_:

```haskell
type family Element c
class Collection c where
    singleton :: Element c -> c
```

Si un usuario crea un nuevo tipo colección, puede extender `Element`:

```haskell
type instance Element [a] = a
instance Collection [a] where
    singleton x = [x]
```

En ocasiones, las familias abiertas se extienden a la par con alguna clase. Por esto GHC tiene azúcar sintáctico para esto:

```haskell
class Collection c where
    type Element c
    singletion :: Element c -> c
```

Las familias pueden ser parciales. Si una familia se aplica sobre un tipo en el cual no está definida, se considera atascada.

## Data Families

Son familias de _datatypes_

```haskell
data family Array a
data instance Array Bool = MkArrayBool ByteArray
data instance Array Int = MkArrayInt (Vector Int)
```

Básicamente se usan para especializar estructuras.


# Rich kinds

## Kinds

Son los tipos de los _type constructors_. Usualmente un sistema de _kinds_ es un _simply typed lambda calculus_, donde el único tipo se denota por `*` (pronunciado _Type_).

Dado que las familias de tipos nos permiten escribir programas que razonan sobre tipos, es necesario chequear su correctitud, para esto se utilizan los _kinds_. Por ejemplo `Element Maybe` es una expresión inválida porque `Maybe` tiene _kind_ `* -> *`. Es decir, `Maybe` es un constructor unario, mientras que `Maybe Int`, es un constructor sin parámetros o _nullary_.

## Promoted datatypes

El sistema de _kinds_ de _Haskell98_ es generado por la gramática

```
k ::= * | k -> k
```

Lo cual es bastante limitante, por lo que se agregaron _promoted datatypes_, por ejemplo:

```haskell
data Bool = False | True
```

declara dos entidades: el tipo `Bool` con los términos `False` y `True` y el _kind_ `Bool` con los tipos `'False` y `'True`, en otras palabras el _datatype_ `Bool` se promueve a un _kind_. De esta manera es posible denotar los naturales con la suma usando el sistema de tipos de Haskell:

```haskell
data Nat = Zero | Succ Nat
type family a + b where
    'Zero + b = b
    'Succ a + b = 'Succ(a + b)
```

Esto permite que cuando escribamos `'Succ 'Zero + 'Succ 'Zero`, Haskell lo simplifique a `'Succ ('Succ 'Zero)`.

## Kind polymorphism

Permite hacer polimorfismo sobre _kinds_, por ejemplo en

```haskell
data T f a = MkT (f a)
```
el tipo `T` tiene _kind_ `(k -> *) -> k -> *` donde `k` puede ser cualquier _kind_. O por ejemplo en

```haskell
type family Length (list :: [k]) :: Nat where
Length '[] = 'Zero
Length (x ': xs) = 'Succ (Length xs)
```

 `Length` es paramétrica sobre algún _kind_ `k`. Esto permite que de hecho `k` pueda ser `Nat` o `*` y poder calcular la longitud de listas de `Nat`s o de términos con tipos de _kind_ `*`

## Constraint kinds

En Haskell, las restricciones impuestas sobre los tipos son _kinds_, por ejemplo el tipo `Show` tiene _kind_ `* -> Constraint`. El tipo `Some` es un ejemplo de esto

```haskell
data Some :: (* -> Constraint) -> * where
    Some :: c a => a -> Some c
```

Entonces, cualquier término de tipo `Some Show` internamente debe ser de algún tipo `a` que tiene una implementación de `Show`. Lo interesante es que `Some Show` es de _kind_ `*`. Esto se parece mucho a los _Trait objects_ de Rust.

# Generalised algebraic data types

Un _Algebraic data type_ simplemente es un tipo suma o producto (enums o tuplas respectivamente). Un __GADT__ admite que los _type constructors_ tengan un tipo no estándar. Por ejemplo

```haskell
data Empty
data NonEmpty
data List x y where
     Nil :: List a Empty
     Cons :: a -> List a b ->  List a NonEmpty

safeHead :: List x NonEmpty -> x
safeHead (Cons a b) = a
```

los constructores de `Nil` y `Cons` no retornan `List x y` sino `List a Empty` y `List a Nonempty`, esto permite que `safeHead` solo funcione con listas no vacías.

Un tipo interesante es la igualdad proposicional

```haskell
data (a :: k) :~: (b :: k) where
    Refl :: a :~: a
```

donde un valor de tipo `t:~:u` representa evidencia de que `t` y `u` son el mismo tipo. Un ejemplo de su uso es

```haskell
castWith :: (a:~:b) -> a -> b
castWith Refl x = x
```

a pesar de que `a` y `b` en principio son distintos, el valor `Refl` como instancia de `a:~:b` obliga a que `a` y `b` sean el mismo tipo. Sin embargo, Haskell es un lenguaje _lazy_ y por lo tanto el primer argumento de `castWith` solo se evalúa en ejecución, esto hace que la evidencia para igualdad de tipos se resuelva en ejecución y no en compilación.

# Higher-rank types

_Haskell98_ utiliza el sistema de tipos de _Hindley-Milner_ (El sistema de tipos de Rust está basado en HM?). Este sistema solo permite _cuantificación prenexa_, donde un tipo puede cuantificar sobre _type variables_ al comienzo.

En otras palabras, cualquier tipo polimórfico está intrínsecamente cuantificado, entonces `map :: (a -> b) -> [a] -> [b]` en realidad tiene tipo
 `forall a b. (a -> b) -> [a] -> [b]`.

Actualmente, _GHC_ permite que las cuantificaciones ocurran en otros lugares. Por ejemplo, el tipo `(forall a. a -> a -> ) -> Int` es válido. Sin embargo, los tipos de alto rango no pueden ser inferidos y deben declararse.

# Scoped type variables

Permite referirse a un tipo declarado anteriormente dentro del cuerpo de una función. Por ejemplo

```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z0 xs0 = lgo z0 xs0
    where
        lgo z [] = z
        lgo z (x : xs) = lgo (f z x) xs
```

sería mas claro si pudiéramos agregarle una signatura a `lgo`:

```haskell
lgo :: b -> [a] -> b
```

Sin esta extensión, Haskell asume que los `a` y `b` en la signatura de `foldl` son independientes de los declarados en `lgo`.
