/**
 * Created by admin on 3/1/15.
 */
object TypeLevel {

  //type difference evidence
  @annotation.implicitNotFound("!!!Type difference is not enforced. Cannot prove that ${A} =!= ${B}!!!")
  trait =!= [A,B]
  implicit def uneq[A,B]: =!=[A,B] =null

  //Ambiguity for the cases where types are equal including def foo[A] (implicit ev: A=!=A)
  implicit def excludeEq1[A]: A =!= A = null
  implicit def excludeEq2[A]: A =!= A = null

  //evidence that type A is not convertible to B
  @annotation.implicitNotFound("!!!${A} should NOT be implicitly convertible to ${B}!!!")
  class <%![A,B]
  implicit def nonConvertible[A, B]: A <%! B = null

  //Ambiguity for the cases where types are  convertible
  implicit def excludeConvertible1[A,B](implicit ev:A=>B):A <%! B = null
  implicit def excludeConvertible2[A,B](implicit ev:A=>B):A <%! B = null


}
