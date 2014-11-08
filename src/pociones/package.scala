package object pociones {

  type Persona = (String, Niveles)
  type Niveles = (Int, Int, Int)
  type Pocion = (String, List[Ingrediente])
  type Ingrediente = (String, Int, List[Efecto])
  type Efecto = Niveles => Niveles

  def aplicar3(f: Int => Int)(niveles: Niveles) =
    (f(niveles._1), f(niveles._2), f(niveles._3))

  def aplicar3Prima(f: Int => Int, niveles: Niveles) =
    niveles match {
      case (a, b, c) => (f(a), f(b), f(c))
    }

  val aplicar3Prima2: (Int => Int, Niveles) => Niveles =
    { case (f, (a, b, c)) => (f(a), f(b), f(c)) }

  def sinRepetidos[T](lista: List[T]): List[T] = {
    lista match {
      case Nil => Nil
      case x :: xs if xs.contains(x) => sinRepetidos(xs)
      case x :: xs => x :: sinRepetidos(xs)
    }
  }

  def sinRepetidos2[T](lista: List[T]) =
    lista.toSet.toList

  val f1 = (c: Niveles) => c match {
    case (ns, nc, nf) => (ns + 1, nc + 2, nf + 3)
  }

  val f2 = aplicar3((math.max _).curried.apply(7)) _

  val max = (math.max _).curried
  
  val f3: Niveles => Niveles = {
    case (ns, nc, nf) if ns >= 8 => (ns, nc, nf + 5)
    case (ns, nc, nf) => (ns, nc, nf - 3)
  }

  val f4: Efecto = { case (a, b, c) => (a, a, c) }

  def invertir3(caracteristicas: Niveles) =
    (caracteristicas._3, caracteristicas._2, caracteristicas._1)

  val multijugos = 
    ("Multijugos", 
    List(("Cuerno de Bicornio en Polvo", 10, List(invertir3 _, f4)),
    ("Sanguijuela hormonal", 54, List((aplicar3(_ * 2) _), f4))))

  val felixFelices = ("Felix Felices", List(("Escarabajos Machacados", 52, List(f1, f2)), ("Ojo de Tigre Sucio", 2, List(f3))))

  val floresDeBach = ("Flores de Bach", List(("Orquidea Salvaje", 8, List(f3)), ("Rosita", 1, List(f1))))

  def misPociones: List[Pocion] = List(felixFelices, multijugos, floresDeBach)

  val harry = ("Harry", (11, 5, 4))
  val ron = ("Ron", (6, 4, 6))
  val hermione = ("Hermione", (8, 12, 2))
  val draco = ("Draco", (7, 9, 6))
  def personas = List(harry, ron, hermione, draco)
  
  val sumaNiveles: Niveles => Int =
  {case (a,b,c) => a + b + c}
  
  val maximo: Niveles => Int = 
    {case (a,b,c) => a.max(b).max(c)}
  val minimo: Niveles => Int = 
    {case (a,b,c) =>a.min(b).min(c)}
  
  val maximo1: Niveles => Int =
    {case (a,b,c) => math.max(math.max(a,b),c)}
  val maximo2: Niveles => Int =
	  {case (a,b,c) => max(b).compose(max(a))(c)}
  
  val diferenciaNiveles: Niveles => Int =
  {niveles => maximo(niveles) - minimo(niveles) }
  
  val sumaNivelesPersona: Persona => Int =
  {case (_, niveles) => sumaNiveles(niveles)}
  
  val diferenciaNivelesPersona: Persona => Int =
	  {case (_, niveles) => diferenciaNiveles(niveles)}
  
  
  val efectosDePocion: Pocion => List[Efecto] =
  {case (_, ingredientes) => 
    ingredientes.flatMap(_._3)}

  val map: (Ingrediente => List[Efecto]) => 
      List[Ingrediente] => 
        List[List[Efecto]] = 
        {f => _.map(f)}
  
  val flatten: List[List[Efecto]] => List[Efecto] = 
  _.flatten      
        
  val snd2: Pocion => List[Ingrediente] = _._2
  
  implicit class FExt[A, B](f: A => B) {
    def <*[C](g: C => A) = {
      f compose g
    }
  }
  
  val efectosDePocion2: Pocion => List[Efecto] =
     flatten <* map(_._3) <* snd2
     
     
     
  val pocionesHeavies: List[Pocion] => List[String] =
    {pociones: List[Pocion] => pociones.map{_._1}} <* 
    {_.filter{efectosDePocion(_).size >= 4}}
    
  val pocionesHeaviesForExpression: List[Pocion] => List[String] =
  {pociones => 
    for(pocion <- pociones 
        if efectosDePocion(pocion).size >= 4)
    yield pocion._1 
  }
  
  //Punto 4
  def incluyeA[T](incluida: List[T], incluyente: List[T]) = 
    incluida.forall(incluyente.contains(_))
    
  val vocales = "aeiou".toList  
  
  val esPar: Integer => Boolean = _ % 2 == 0
  
  val esPocionMagica: Pocion => Boolean =
  {pocion => pocion._2.exists{
	  ing => incluyeA(vocales, ing._1.toList)
  	} && pocion._2.forall { ing =>
  	  		esPar(ing._2) 
  		}
  }
  
  val tomarPocion: (Pocion, Persona) => Persona =
  {(pocion, persona) =>
  	(persona._1, efectosDePocion(pocion).foldLeft(persona._2){(niveles,efecto) =>
  		efecto(niveles)
  	})
  }
  
  val tomarPocion2: Pocion => Persona => Persona =
    {(pocion: Pocion) => {tomarPocion(pocion, _)}}
  
  val esAntidoto: (Persona, Pocion, Pocion) => Boolean =
  {(persona, pocion, antidoto) =>
    ({pers: Persona => pers == persona} <* 
        tomarPocion2(antidoto) <* 
        tomarPocion2(pocion))(persona)
  }
  
  def maximoF[T](f: T => Int, lista: List[T]): T = lista match {
    case List(x) => x
    case x :: y :: xs => 
      if(f(x) > f(y)) maximoF(f, x::xs) 
      else maximoF(f, y::xs)
  }
    
  val personaMasAfectada: (Pocion, (Niveles => Int), List[Persona]) => Persona =
  {(pocion, f, personas) =>
  	maximoF(
  	    {persona: Persona => 
  	      (f 
  	      <* {pers: Persona => pers._2} 
  	      <* tomarPocion2(pocion))(persona)
  	    }, 
  	    personas)
  }

  val promedioDeNiveles: Niveles => Int = {niveles => sumaNiveles(niveles) / 3}
  val fuerzaFisica: Niveles => Int = {niveles => niveles._3}
  
}