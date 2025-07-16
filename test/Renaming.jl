module TestRenaming

using TraitInterfaces, Test

@interface ThMonoid begin
  X::TYPE

  unit()::X
  @op (e) := unit

  mul(x::X,y::X)::X
  @op (⋅) := mul

  assoc := (x ⋅ y) ⋅ z == x ⋅ (y⋅ z) ⊣ [(x,y,z)::X]
  unitality := ((e() ⋅ x) == x == (x ⋅ e())) ⊣ [x::X]
end


@interface ThGroup <: ThMonoid begin
  inv(x::X)::X
  @op (⁻¹) := inv

  ⁻¹(x) ⋅ x == x == x ⋅ ⁻¹(x) ⊣ [x::X]
end

@interface ThAbGroup <: ThGroup begin
  x⋅y == y⋅x ⊣ [x::X, y::X]
end

module RingModule
  export ThRing
  using TraitInterfaces
  using ..ThAbGroup, ..ThMonoid

  @interface ThRing begin
    using ThAbGroup: unit => zero, mul => plus,  :⋅ => :+, e => Z, 
                     inv => minus, ⁻¹ => :-
    using ThMonoid:  unit => one,  mul => times, :⋅ => :×, e => I

    left_distrib  := a × (b + c) == (a × b) + (a × c) ⊣ [(a, b, c)::X]
    right_distrib := (b + c) × a == (b × a) + (c × a) ⊣ [(a, b, c)::X]
  end
end
using .RingModule: ThRing
T = ThRing.Meta.theory

struct NatRingTrait end
NatRing = NatRingTrait()

@instance ThRing{Int} [model::NatRingTrait] begin 
  one()::Int = 1
  zero()::Int = 0 
  plus(a::Int, b::Int)::Int = a + b
  times(a::Int, b::Int)::Int = a * b
  minus(a::Int)::Int = -a
end


@test ThRing.plus[NatRing](3,4) == 7 
@test ThRing.times[NatRing](3,4) == 12

struct Modulo 
  n::UInt8
end
ℤm3 = Modulo(3)

@instance ThRing{Int} [model::Modulo] begin 
  one()::Int = 1
  zero()::Int = 0 
  plus(a::Int, b::Int)::Int = (a + b) % model.n
  times(a::Int, b::Int)::Int = (a * b) % model.n
  minus(a::Int)::Int = (-a) % model.n
end

@test ThRing.plus[ℤm3](2,2) == 1
@test ThRing.times[ℤm3](2,2) == 1

module RingSquareModule
  export ThRingSquare
  using TraitInterfaces
  using ..ThRing
  using .ThRing
  import .ThRing: X, zero, plus, minus, one, times

  @interface ThRingSquare <: ThRing begin
    square(a::X) := a × a
    @op (⁺²) := square
  end
end

using .RingSquareModule: ThRingSquare
using .ThRingSquare: square

# nothing more to be done
@instance ThRingSquare{Int} [model::Modulo] begin end

@test implements(ℤm3, ThRingSquare)

@test square[ℤm3](2) == 1

end # module
