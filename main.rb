#!/usr/bin/env ruby
require 'matrix'
include Math

# we specialize in spherical coordinates here.
# coordinate systems are internally represented by unit vectors
# in J2000 equatorial system.

# constellation data taken from
# http://www.iau.org/public/constellations/

class Vector
  def cross_product(other)
    unless size() == 3 && other.size() ==3
      throw 'cross products are only defined on three-dimensional vectors'
    end
    ret = (0..2).map{|i|
      j=(i+1)%3
      k=(i+2)%3
      self[j]*other[k] - self[k]*other[j]
    }
    return Vector.elements(ret)
  end
end

class CoordinateSystem
  def initialize(label, np, zp)
    @label = label
    @northPole = np
    @zeroPoint = zp

    STDERR.puts <<MSG
making: 
#{np.vec.r}
#{zp.vec.r}
#{np.vec.inner_product(zp.vec)}
MSG
  end

  attr_accessor :northPole, :zeroPoint, :label

  def makePt(longitude, latitude)
    ez = @northPole.vec
    ex = @zeroPoint.vec
    ey = ez.cross_product(ex)
    z = sin(latitude)
    x = cos(longitude) * cos(latitude)
    y = sin(longitude) * cos(latitude)
    return Point::new(self, longitude, latitude, ex*x + ey*y + ez*z)
  end
  
  def makePtDegree(lo, la)
    makePt(lo / 180.0 * PI, la / 180.0 * PI)
  end

  def transform(pt)
    ez = @northPole.vec
    ex = @zeroPoint.vec
    ey = ez.cross_product(ex)
    x = ex.inner_product(pt.vec)
    y = ey.inner_product(pt.vec)
    z = ez.inner_product(pt.vec)
    r_xy = (x**2 + y**2)**0.5
    lo = atan2(y,x)
    la = atan2(z,r_xy)    
    return Point::new(self, lo, la, pt.vec)
  end
end

    
class Point
  def initialize(cd, lo, la, vec)
    @coordinate = cd
    @longitude  = lo
    @latitude   = la
    @vec        = vec * (1.0 / vec.r)
  end
  
  def toDecimal()
    return [@longitude * 180/PI, @latitude * 180/PI]
  end
  attr_accessor :coordinate 
  attr_accessor :longitude  # E-W position
  attr_accessor :latitude   # N-S position
  attr_accessor :vec
end

def fromRD(raHour, raMin, raSec, dec, decMin, decSec)
  # h:m:s = right ascension
  # d = declination in degree
  ra = (raHour + (raMin + (raSec / 60.0)) / 60.0)/24.0 * 360
  de = dec + (decMin + (decSec) / 60.0) / 60.0
  return fromRDdecimal(ra, de)
end

def fromRDdecimal(ra, de)
  # ra = right ascension in degrees
  # de = declination in degrees
  return $equatorialSystem.makePt(ra / 180 * PI, de / 180 * PI)
end

#bootstrapping 
$equatorialSystem = CoordinateSystem::new(:equatorial, 
  Point::new(nil, 0.0, 0.5*PI, Vector[1.0, 0.0, 0.0]),
  Point::new(nil, 0.0, 0.0   , Vector[0.0, 1.0, 0.0]))
$equatorialSystem.northPole.coordinate = $equatorialSystem
$equatorialSystem.zeroPoint.coordinate = $equatorialSystem

$galacticSystem = CoordinateSystem::new(:galactic, 
  fromRD(12,51,26.282,27,07,42.01), 
  fromRD(17,45,37.224,-28,-56,-10.23))
                                       
puts $galacticSystem.northPole.toDecimal.join(' ')
puts $galacticSystem.zeroPoint.toDecimal.join(' ')

sagittariusAStar = fromRD(17,45,40.04, -29,0,-28.1)
sasG = $galacticSystem.transform(sagittariusAStar)

puts sasG.toDecimal.join(' ')

sas2 = $galacticSystem.makePtDegree(359 + (56 + (39.5)/60)/60, -(2+46.3/60)/60)

puts (sas2.vec - sasG.vec).r

