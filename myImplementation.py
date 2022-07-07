import hashlib

def hash256(message):
  # returns a bytes object (essentially a sequence of integers [0, 255], that is immutable)
  # the message can be a bytes object (or whatever else sha256 accepts)
  # applies sha256 twice
  return hashlib.sha256(hashlib.sha256(message).digest()).digest()

class FieldElement:
  def __init__(self, num, prime):
    if num < 0 or num >= prime:
        raise ValueError(f'Num {num} not in field range 0 to {prime-1}')
    self.num = num
    self.prime = prime
  def __repr__(self):
    return f'FieldElement_{self.prime}({self.num})'
  def __eq__(self, other):
    """
    this method is used in the Point class' __eq__ method, when the Point's x, y, a, and b are FieldElements

    so i believe the "if other is None" part is saying: if one point is the point at infinity (i.e., x and y will be None), 
    and the other is a normal point (with x & y being FieldElements), when we compare their x and ys False is returned 
    so we return that they're not equal, which is correct

    in the case where we have two points at infinities, this method is never used wehn comparing z & y since they're both None, and not FieldElements, so no prob here
    """
    if other is None: # im still not entirely sure what this is for
      return False
    return self.num == other.num and self.prime == other.prime
  def __ne__(self, other):
    return not (self == other)

  def __add__(self, other):
    if self.prime != other.prime:
      raise TypeError('Cannot add two numbers in different Fields')
    return self.__class__((self.num + other.num) % self.prime, self.prime) # __class__ lets us access self's class; we could directly use FieldElement, but then the add method won't work for classes that inherit from FieldElement in the future
  def __sub__(self, other):
    if self.prime != other.prime:
      raise TypeError('Cannot subtract two numbers in different Fields')
    return self.__class__((self.num - other.num) % self.prime, self.prime)
  def __mul__(self, other):
    if self.prime != other.prime:
      raise TypeError('Cannot multiply two numbers in different Fields')
    return self.__class__((self.num * other.num) % self.prime, self.prime)
  def __rmul__(self, coef): 
    # this is called when we have a normal number in front of a FieldElement multiplied, like k*myFieldElement
    # this allows us to multiply a FieldElement by a coefficient without having to make that coefficient a FieldElement and use __mul__
    # this is useful in the Point class [see ch.3]
    return self.__class__((coef * self.num) % self.prime, self.prime)
  def __pow__(self, exp): # i believe this magic method is only used for a ** b and not pow(a, b)
    exp = exp % (self.prime - 1)
    return self.__class__(pow(self.num, exp, self.prime), self.prime)
  def __truediv__(self, other):
    if self.prime != other.prime:
      raise TypeError('Cannot divide two numbers in different Fields')
    inverse = pow(other.num, self.prime - 2, self.prime) % self.prime
    return self.__class__((self.num * inverse) % self.prime, self.prime)

class Point:
  def __init__(self, x, y, a, b):
    self.x = x
    self.y = y
    self.a = a
    self.b = b
    if x is None: # is None better than == None apparently
      return # this exits the function, with our object having x & y as None and the appropriate a and b; it does not mean that the object returned is None
    if y**2 != x**3 + a*x + b:
      raise ValueError(f'({x}, {y}) is not on the curve')

  def __eq__(self, other):
    return self.x == other.x and self.y == other.y and self.a == other.a and self.b == other.b
  def __ne__(self, other):
    return not (self == other)

  def __repr__(self):
    if self.x is None:
      return 'Point(Infinity)'
    else:
      return f'Point({self.x}, {self.y})_{self.a}_{self.b}'

  def __add__(self, other):
    if self.a != other.a or self.b != other.b:
      raise TypeError(f'Points ({self.x},{self.y}) and ({other.x},{other.y}) are not on the same curve')

    if self.x is None:
      return other
    if other.x is None:
      return self
    if self == other:
      if self.y == 0 * self.x: # self.y == 0 * self.x, not self.y == 0, since when self.y is a FieldElement we really want to test if it's num == 0, so we can use 0*self.x (which uses __rmul__ of FieldElement)
        return self.__class__(None, None, self.a, self.b)
      else:
        s = (3*(self.x ** 2) + self.a) / (2 * self.y)
        x = s**2 - 2*self.x
        y = s*(self.x - x) - self.y
        return self.__class__(x, y, self.a, self.b)
    else:
      if self.x == other.x:
        return self.__class__(None, None, self.a, self.b)
      else:
        s = (self.y - other.y)/(self.x - other.x)
        x = s**2 - self.x - other.x
        y = s*(self.x - x) - self.y
        return self.__class__(x, y, self.a, self.b)
  
  def __rmul__(self, coef):
    # notice how if coef = 0, we return the point at infinity
    result = self.__class__(None, None, self.a, self.b)
    pnt = self
    while coef:
      if coef & 1:
        result += pnt # += uses __add__ of the Point class
      coef >>= 1
      pnt += pnt
    return result

s256prime = 2**256 - 2**32 - 977
s256a = 0
s256b = 7
s256n = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141


class S256Field(FieldElement):
  def __init__(self, num, prime=None):
    """
    here we need "prime=None" since even tho we're not using it, when S256Field objects are initialized
    in FieldElement's methods when we have self.__class__ where self is an S256Field object, a prime is passed in
    and we must accept it otherwise TypeError arises telling u that u passed in the wrong number of arguments
    """
    super().__init__(num, s256prime)
  def __repr__(self):
    # :x converts it to hexadecimal
    # we want to show 256 bits (recall that all s256 field elements r storable in 256 bits), even leading zeros
    # this means showing 64 hex digits (since 1 hex digit represents 4 bits); zfill(64) left pads zeroes until we have 64 characters
    return f'{self.num:x}'.zfill(64)

class S256Point(Point):
  def __init__(self, x, y, a=None, b=None):
    """
    here we need a=None and b=None since when self.__class__ appears in one of Point's methods
    and self is a S256Point, that method will pass in a and b, and we have to accept it, despite already knowing what it is
    """
    """
    is there a reason why the book didn't do it like if x is None then let directly thru, otherwise pass thru S256Field first?
    """
    if type(x) == int:
      super().__init__(S256Field(x), S256Field(y), S256Field(s256a), S256Field(s256b))
    else: # if x & y are None (we have the point at inf), we need to let them directly thru without S256Field
      super().__init__(x, y, S256Field(s256a), S256Field(s256b))
  
  def __repr__(self):
    if self.x is None:
      return f'S256Point(Infinity)'
    else:
      return f'S256Point({self.x},{self.y})'

  def __rmul__(self, coef):
    # we can make rmul a bit more efficient by first modding by s256n; since we know the points we'll be scalar multiplying
    # are elements of the group generated by G, and we know k*G = (k%n)*G
    return super().__rmul__(coef % s256n)

  def verify(self, z, sig):
    sInv = pow(sig.s, s256n-2, s256n) # we use fermat's little theorem to calculate inverse of s under mod n; possible since n is prime
    u = (z * sInv) % s256n
    v = (sig.r * sInv) % s256n
    return (u*G + v*self).x.num == sig.r

# apparently we'll be using this later
G = S256Point(0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798, 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8)

class Signature:
  def __init__(self, r, s):
    self.r = r
    self.s = s
  def __repr__(self):
    return f'Signature({self.r:x},{self.s:x})'

