import hashlib

def hash256(message):
  # returns a bytes object (essentially a sequence of integers [0, 255], that is immutable)
  # the message can be a bytes object (or whatever else sha256 accepts??)
  # applies sha256 twice
  return hashlib.sha256(hashlib.sha256(message).digest()).digest()
def hash160(message):
  # applies sha256, then ripemd160 to the message which is a bytes object
  return hashlib.new("ripemd160", hashlib.sha256(message).digest()).digest()

BASE58_ALPHABET = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'
def encode_base58(s):
  # s is a bytes object, and we're converting it into a base58 string

  """I don't entirely understand why we pad our base58 with 0s (or 1s technically) equal to the number of 0 bytes that s is padded with yet"""
  numZeroes = 0 # the number of \x00 that pad s to the left (other \x00 aren't counted)
  for byte in s:
    if byte: # if the byte isn't 0
      break
    numZeroes += 1

  result = ""
  num = int.from_bytes(s, "big") # reading s's base256 digits (byte by byte) in big-endian is equivalent to reading s's base2 digits (8 for each "base256 digit") and converting it to base10
  while num > 0:
    num, digit = divmod(num, 58)
    result = BASE58_ALPHABET[digit] + result

  return ("1"*numZeroes) + result # 1 is the digit for 0 in base58
def encode_base58_checksum(s):
  # appends a checksum for s, which is the first 4 digits of a hash256, and then encodes it all in base58
  return encode_base58(s + hash256(s)[:4])


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

# constants specific to the secp256k1 cryptographic curve
s256prime = 2**256 - 2**32 - 977
s256a = 0
s256b = 7
s256n = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
# G is later down in the code, since it must be after the S256Point class

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
  
  def sqrt(self):
    # returns one of the 2 possible sqrts
    # this formula works for S256Field elements since s256p%4 == 3
    return self ** ((self.prime + 1) // 4)

class S256Point(Point):
  def __init__(self, x, y, a=None, b=None):
    """
    here we need a=None and b=None since when self.__class__ appears in one of Point's methods
    and self is a S256Point, that method will pass in a and b, and we have to accept it, despite already knowing what it is
    """
    # is there a reason why the book didn't do it like if x is None then let directly thru, otherwise pass thru S256Field first?
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
  
  def sec(self, compressed = True):
    # compressed (or uncompressed if specified) SEC serialization of a public key (a S256Point)
    # remember that the x & y coordinates are < s256p < 2^256, so storable in 256 bits or 32 bytes
    if not compressed:
      return b'\x04' + self.x.num.to_bytes(32, "big") + self.y.num.to_bytes(32, "big")
    if self.y.num % 2:
      return b'\x03' + self.x.num.to_bytes(32, "big")
    else:
      return b'\x02' + self.x.num.to_bytes(32, "big")

  def hash160(self, compressed = True):
    # gets the compressed/uncompressed SEC for the public key, then puts it through hash160
    return hash160(self.sec(compressed))

  def address(self, compressed = True, testnet = False):
    # gets the address of the public key; we can use compressed/uncompressed SEC, and mainnet or testnet
    h160 = self.hash160(compressed)
    return encode_base58_checksum( (b'\x6f' if testnet else b'\x00') + h160)

  @classmethod
  def parse(self, sec_bin):
    # sec_bin is the SEC binary that we get; it is a bytes object i believe
    # it appears comparisons like sec_bin[0] == b'\x04' don't work, we need to do == 4
    if sec_bin[0] == 4: # uncompressed SEC
      x = int.from_bytes(sec_bin[1:33], "big")
      y = int.from_bytes(sec_bin[33:65], "big")
      return S256Point(x, y)
    if sec_bin[0] == 3: # compressed, even y
      x = S256Field(int.from_bytes(sec_bin[1:], "big"))
      y = (x**3 + S256Field(s256b)).sqrt()
      if not (y.num % 2):
        y = -1 * y
      return S256Point(x, y)
    if sec_bin[0] == 2: # compressed, odd y
      x = S256Field(int.from_bytes(sec_bin[1:], "big"))
      y = (x**3 + S256Field(s256b)).sqrt()
      if y.num % 2:
        y = -1 * y
      return S256Point(x, y)

      

# the point that generates the finite cyclic group of secp256k1 that we use 
G = S256Point(0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798, 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8)

class Signature:
  def __init__(self, r, s):
    self.r = r
    self.s = s
  def __repr__(self):
    return f'Signature({self.r:x},{self.s:x})'
  
  def der(self):
    # serialization of the signature in DER format
    r = self.r.to_bytes(32, "big")
    r = r.lstrip(b'\x00')
    if r[0] & 0x80: # 0x80 is 1000 0000
      r = b'\x00' + r
    result = bytes([2, len(r)]) + r

    s = self.s.to_bytes(32, "big")
    s = s.lstrip(b'\x00')
    if s[0] & 0x80:
      s = b'\x00' + s
    result += bytes([2, len(s)]) + s

    return bytes([0x30, len(result)]) + result

class PrivateKey: 
  """
  i believe this class is meant to store private key specifically for the ECDSA with secp256k1
   - since the point we keep around is secret * G specifically
   - since hex seems to assume bounds on the size of the secret
  """
  def __init__(self, secret):
    self.secret = secret # doesn't this allow us to pass in a secret greater than s256n? should we mod by n??
    self.point = secret*G # apparently we keep this around for convenience 

  def hex(self):
    # here we pad with 0s to show 64 hex digits, or 256 bits
    # i believe this is bc the secret, which in ECDSA with secp256k1 is a scalar multiple of G, is thus [0, n-1]
    # (where n is s256n, or the order of the cyclic group of points on secp256k1 generated by G)
    # and since n is lower than but close to 2^256, we know that the secret is then storable in 256 bits
    return f'{self.secret:x}'.zfill(64) 

  def wif(self, compressed = True, testnet = False):
    # serialization of the private key in WIF format
    # compressed indicates whether the associated public key's address used compressed or uncompressed SEC
    return encode_base58_checksum((b'\xef' if testnet else b'\x80') + self.secret.to_bytes(32, "big") + (b'\x01' if compressed else b''))
