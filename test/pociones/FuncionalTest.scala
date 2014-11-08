package pociones

import org.junit.Test
import org.junit.Assert

class PocionesTest {

  @Test
  def sumaNivelesTest {
    Assert.assertEquals(6, sumaNiveles((1, 2, 3)))
  }

  @Test
  def diferenciaNivelesTest {
    Assert.assertEquals(2, diferenciaNiveles((1, 2, 3)))
  }

  @Test
  def sumaNivelesPersonaTest {
    Assert.assertEquals(20, sumaNivelesPersona(("Harry", (11, 5, 4))))
  }

  @Test
  def diferenciaNivelesPersonaTest {
    Assert.assertEquals(7, diferenciaNivelesPersona(("Harry", (11, 5, 4))))
  }

  @Test
  def efectosDePocionTest {
    val efectos = efectosDePocion("Felix Felices", List(("Escarabajos Machacados", 5, List(f1, f2)), ("Ojo de Tigre", 2, List(f3))))
    Assert.assertEquals(List(f1, f2, f3), efectos)
  }

  @Test
  def pocionesHeaviesTest {
    val pociones = pocionesHeavies(misPociones)
    Assert.assertEquals(List("Multijugos"), pociones)
  }

  @Test
  def pocionesHeaviesForExpressionTest {
    val pociones = pocionesHeaviesForExpression(misPociones)
    Assert.assertEquals(List("Multijugos"), pociones)
  }

  @Test
  def incluyeATest {
    Assert.assertTrue(incluyeA(List(3, 6, 9), (1 until 10).toList))
  }

  @Test
  def multijugosEsMagicaTest {
    Assert.assertTrue(esPocionMagica(multijugos))
  }

  @Test
  def felixFelicesNoEsMagica {
    Assert.assertFalse(esPocionMagica(felixFelices))
  }

  @Test
  def floresDeBachNoEsMagica {
    Assert.assertFalse(esPocionMagica(floresDeBach))
  }

  @Test
  def tomarPocionTest {
    Assert.assertEquals(("Harry", (12, 7, 12)), tomarPocion(felixFelices, ("Harry", (11, 5, 4))))
  }
  
  @Test
  def esAntidotoTest {
    val pocion = ("bla", List(("ing1", 2, List(invertir3 _))))
    Assert.assertTrue(esAntidoto(harry, pocion, pocion))
  }

  @Test
  def personaMasAfectadaTest {
    val persona = personaMasAfectada(multijugos, sumaNiveles, personas)
    Assert.assertEquals(("Draco", (7, 9, 6)), persona)
  }

  @Test
  def promedioDeNivelesTest {
    val persona = personaMasAfectada(multijugos, promedioDeNiveles, personas)
    Assert.assertEquals(("Draco", (7, 9, 6)), persona)
  }

  @Test
  def fuerzaFisicaTest {
    val persona = personaMasAfectada(multijugos, fuerzaFisica, personas)
    Assert.assertEquals(("Harry", (11, 5, 4)), persona)
  }
  
  @Test
  def diferenciaNivelesPersonaMasAfectadaTest {
    val persona = personaMasAfectada(multijugos, diferenciaNiveles, personas)
    Assert.assertEquals(("Harry", (11, 5, 4)), persona)
  }

}