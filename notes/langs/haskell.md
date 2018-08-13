
## Type Classes

Son un tipo de polimorfismo similar a las interfaces o a los traits.

```haskell
class Show a where
    show :: a -> String

instance Show Bool where
    show True = "tru"
    show False = "fls"
```

Básicamente, una clase es paramétrica sobre un tipo. La diferencia de usar clases con funciones polimórficas paramétricas radica en que al crear una instancia de la clase usando un tipo específico `a`, los métodos de la clase pueden saber el tipo.

Para usar una clase como restricción sobre un tipo, se utiliza la siguiente sintaxis:

```haskell
Show a => [a] -> String
```

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

## Kinds

Son los tipos de los tipos. Usualmente un sistema de _kinds_ es un _simply typed lambda calculus_, donde el único tipo se denota por `*`.

Dado que las familias de tipos nos permiten escribir programas que razonan sobre tipos, es necesario chequear su correctitud, para esto se utilizan los _kinds_. Por ejemplo `Element Maybe` es una expresión inválida porque `Maybe` tiene _kind_ `* -> *`. Es decir, `Maybe` no es propiamente un tipo, mientras que `Maybe Int`, por ejemplo, si lo es.

## Promoted datatypes

El sistema de _kinds_ de _Haskell98_ es generado por la gramática $k ::= * \mid k \rightarrow k$, lo cual es bastante limitante, por lo que se agregaron _promoted datatypes_, por ejemplo:

```haskell
data Bool = False | True
```

declara dos entidades: el tipo `Bool` con los términos `False` y `True` y el _kind_ `Bool` con los tipos `'False` y `'True`. De esta manera es posible denotar los naturales con la suma usando el sistema de tipos de Haskell:

```haskell
data Nat = Zero | Succ Nat
type family a + b where
    'Zero + b = b
    'Succ a + b = 'Succ(a + b)
```

Esto permite que cuando escribamos `'Succ 'Zero + 'Succ 'Zero`, Haskell lo simplifique a `'Succ ('Succ 'Zero)`.

## Kind polymorphism
