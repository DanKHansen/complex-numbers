case class ComplexNumber(real: Double = 0, imaginary: Double = 0):
   def +(that: ComplexNumber): ComplexNumber = ComplexNumber(real + that.real, imaginary + that.imaginary)
   def -(that: ComplexNumber): ComplexNumber = ComplexNumber(real - that.real, imaginary - that.imaginary)
   def *(that: ComplexNumber): ComplexNumber =
      ComplexNumber(real * that.real - imaginary * that.imaginary, imaginary * that.real + real * that.imaginary)
   def /(that: ComplexNumber): ComplexNumber =
      val denom = that.real * that.real + that.imaginary * that.imaginary
      ComplexNumber(
        (real * that.real + imaginary * that.imaginary) / denom,
        (imaginary * that.real - real * that.imaginary) / denom)
   def abs: Double = math.hypot(real, imaginary)
   def conjugate: ComplexNumber = ComplexNumber(real, -imaginary)

object ComplexNumber:
   def exp(c: ComplexNumber): ComplexNumber =
      val er = math.exp(c.real)
      ComplexNumber(er * math.cos(c.imaginary), er * math.sin(c.imaginary))
